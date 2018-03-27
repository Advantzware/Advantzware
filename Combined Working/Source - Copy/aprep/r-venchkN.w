&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: aprep\r-venchk.w

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
DEFINE VARIABLE excelHeader AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i NEW SHARED}
  
ASSIGN
 cocode = gcompany
 locode = gloc.

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEF VAR ll-secure AS LOG NO-UNDO.

DEF TEMP-TABLE tt-report    LIKE report
    FIELD check-no          LIKE ap-pay.check-no
    FIELD check-date        LIKE ap-pay.check-date
    FIELD vend-no           LIKE ap-pay.vend-no
    FIELD vend-name         LIKE vend.NAME
    FIELD inv-no            LIKE ap-payl.inv-no
    FIELD due-date          LIKE ap-payl.due-date
    FIELD gross-amt         LIKE ap-payl.amt-paid
    FIELD amt-disc          LIKE ap-payl.amt-disc
    FIELD amt-paid          LIKE ap-payl.amt-paid
    FIELD LINE              AS INT
    FIELD c-no              AS INT
    FIELD row-id            AS ROWID.

DEF TEMP-TABLE tt-report2 LIKE tt-report.

DEFINE BUFFER b-tt-report FOR tt-report.

DEFINE STREAM excel.

DEF VAR v-frt-acct LIKE ap-ctrl.freight NO-UNDO.
DEF VAR v-acct-dscr AS CHAR NO-UNDO.
FIND FIRST ap-ctrl WHERE ap-ctrl.company = cocode NO-LOCK NO-WAIT NO-ERROR.
IF AVAIL ap-ctrl THEN ASSIGN v-frt-acct = ap-ctrl.freight.


DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg .
DEF VAR cTextListToDefault AS cha NO-UNDO.


ASSIGN cTextListToSelect = "CHECK#,CHK DATE,INVOICE#,VENDOR#,VEND NAME,DUE DATE,GROSS AMT,DISCOUNT,NET AMT" 

       cFieldListToSelect = "chk,chk-date,inv,vend,vend-name,due-date,gross-amt,dis,net-amt" 

       cFieldLength = "8,8,12,8,30,8,14,10,14" /*+ "7,15,7,4,20,6,13,12"*/
       cFieldType = "i,c,c,c,c,c,i,i,i" /*+ "c,c,c,i,c,i,i,i" */
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "CHECK#,CHK DATE,INVOICE#,VENDOR#,VEND NAME,DUE DATE,GROSS AMT,DISCOUNT,NET AMT"  .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_date end_date begin_vend ~
end_vend begin_check end_check Begin_Bank End_Bank tb_prt-acc tb_post-date ~
sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest ~
lv-ornt lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file ~
btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date begin_vend end_vend ~
begin_check end_check Begin_Bank End_Bank tb_prt-acc tb_post-date sl_avail ~
sl_selected rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
td-show-parm tb_excel tb_runExcel fi_file 

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

DEFINE VARIABLE Begin_Bank AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beginning Bank" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_check AS INTEGER FORMAT ">>>>>>>>>":U INITIAL 0 
     LABEL "Beginning Check#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Check Date" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE End_Bank AS CHARACTER FORMAT "X(256)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Bank" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_check AS INTEGER FORMAT ">>>>>>>>>":U INITIAL 999999999 
     LABEL "Ending Check#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Check Date" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-venchk.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1
     FGCOLOR 9 .

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
     SIZE 19 BY 6.67 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 9.05.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 9.52.

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

DEFINE VARIABLE tb_post-date AS LOGICAL INITIAL no 
     LABEL "Run By Post Date?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prt-acc AS LOGICAL INITIAL no 
     LABEL "Print Invoice & GL Account Detail?" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY 1 NO-UNDO.

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
     begin_date AT ROW 2.67 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Check Date"
     end_date AT ROW 2.67 COL 66 COLON-ALIGNED HELP
          "Enter Ending Check Date"
     begin_vend AT ROW 4.1 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Vendor Number"
     end_vend AT ROW 4.1 COL 66 COLON-ALIGNED HELP
          "Enter Ending Vendor Number"
     begin_check AT ROW 5.52 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Check Date"
     end_check AT ROW 5.52 COL 66 COLON-ALIGNED HELP
          "Enter Ending Check Date"
     Begin_Bank AT ROW 6.95 COL 24 COLON-ALIGNED WIDGET-ID 4
     End_Bank AT ROW 6.95 COL 66 COLON-ALIGNED WIDGET-ID 6
     tb_prt-acc AT ROW 8.29 COL 25 WIDGET-ID 2
     tb_post-date AT ROW 9.43 COL 25
     sl_avail AT ROW 11.81 COL 5.4 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 11.81 COL 41.4 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 11.81 COL 60.8 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 12.81 COL 41.4 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 13.81 COL 41.4 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 14.86 COL 41.4 WIDGET-ID 40
     btn_down AT ROW 15.86 COL 41.4 WIDGET-ID 42
     rd-dest AT ROW 18.43 COL 7 NO-LABEL
     lv-ornt AT ROW 18.67 COL 31 NO-LABEL
     lines-per-page AT ROW 18.67 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 20.57 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 21.52 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 22.67 COL 30
     tb_excel AT ROW 24.14 COL 50.2 RIGHT-ALIGNED
     tb_runExcel AT ROW 24.14 COL 72.4 RIGHT-ALIGNED
     fi_file AT ROW 25 COL 28.2 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 26.67 COL 26
     btn-cancel AT ROW 26.67 COL 56
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 11.1 COL 6.2 WIDGET-ID 38
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 17.48 COL 5
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 11.1 COL 60.8 WIDGET-ID 44
     RECT-6 AT ROW 17.24 COL 2
     RECT-7 AT ROW 1.24 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 96.8 BY 27.81.


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
         TITLE              = "AP Check Register"
         HEIGHT             = 28.05
         WIDTH              = 98.2
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
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       Begin_Bank:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_check:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       End_Bank:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_check:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
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

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* AP Check Register */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* AP Check Register */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Begin_Bank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Begin_Bank C-Win
ON LEAVE OF Begin_Bank IN FRAME FRAME-A /* Beginning Bank */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_check
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_check C-Win
ON LEAVE OF begin_check IN FRAME FRAME-A /* Beginning Check# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Check Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend C-Win
ON LEAVE OF begin_vend IN FRAME FRAME-A /* Beginning Vendor# */
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
  RUN run-report.
  STATUS DEFAULT "Processing Complete".

  IF tb_excel THEN DO:
    OUTPUT STREAM excel CLOSE.
    IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
  END. 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_vend
                            &END_cust=end_vend
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
               {custom/asimailr.i &TYPE = "Vendor"
                                  &begin_cust= begin_vend
                                  &END_cust=end_vend
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.
       END. 
       WHEN 6 THEN run output-to-port.
  end case.
  SESSION:SET-WAIT-STATE (""). 

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


&Scoped-define SELF-NAME End_Bank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL End_Bank C-Win
ON LEAVE OF End_Bank IN FRAME FRAME-A /* Ending Bank */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_check
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_check C-Win
ON LEAVE OF end_check IN FRAME FRAME-A /* Ending Check# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Check Date */
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
ON HELP OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
DO:
   def var ls-filename as cha no-undo.
   def var ll-ok as log no-undo.

   system-dialog get-file ls-filename 
                 title "Select File to Save "
                 filters "Excel Files    (*.csv)" "*.csv",
                         "All Files    (*.*) " "*.*"
                 initial-dir "c:\tmp"
                 MUST-EXIST
                 USE-FILENAME
                 UPDATE ll-ok.

    IF ll-ok THEN self:screen-value = ls-filename.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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


&Scoped-define SELF-NAME tb_post-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_post-date C-Win
ON VALUE-CHANGED OF tb_post-date IN FRAME FRAME-A /* Run By Post Date? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-acc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-acc C-Win
ON VALUE-CHANGED OF tb_prt-acc IN FRAME FRAME-A /* Print Invoice  GL Account Detail? */
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
  RUN DisplaySelectionList.
  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    APPLY "entry" TO begin_vend.
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
  DISPLAY begin_date end_date begin_vend end_vend begin_check end_check 
          Begin_Bank End_Bank tb_prt-acc tb_post-date sl_avail sl_selected 
          rd-dest lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm 
          tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_date end_date begin_vend end_vend begin_check 
         end_check Begin_Bank End_Bank tb_prt-acc tb_post-date sl_avail Btn_Def 
         sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest lv-ornt 
         lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file 
         btn-ok btn-cancel 
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
/*      DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.      */
/*                                                         */
/*      if init-dir = "" then init-dir = "c:\temp" .       */
/*      SYSTEM-DIALOG GET-FILE list-name                   */
/*          TITLE      "Enter Listing Name to SAVE AS ..." */
/*          FILTERS    "Listing Files (*.rpt)" "*.rpt",    */
/*                     "All Files (*.*)" "*.*"             */
/*          INITIAL-DIR init-dir                           */
/*          ASK-OVERWRITE                                  */
/*     /*     CREATE-TEST-FILE*/                           */
/*          SAVE-AS                                        */
/*          USE-FILENAME                                   */
/*                                                         */
/*          UPDATE OKpressed.                              */
/*                                                         */
/*      IF NOT OKpressed THEN  RETURN NO-APPLY.            */
{custom/out2file.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-gl-acct C-Win 
PROCEDURE print-gl-acct :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR v-frgt-amt LIKE ap-inv.freight NO-UNDO.
DEF VAR v-line-amt LIKE ap-invl.amt NO-UNDO.
{sys/form/r-topw.f}

ASSIGN
 str-tit2 = c-win:TITLE + " - Summary by Account"
 {sys/inc/ctrtext.i str-tit2 112}.  


FORM HEADER
 "                                                          Summary by Account " SKIP(2)
 "ACCOUNT CHECK#                      PO#   DATE   VENDOR#  INVOICE#    "
 "LINE DESCRIPTION              QTY    UNIT PRICE     AMT PAID" SKIP
 "------- ----------------------- ------- -------- ------- ---------    "
 "---- --------------------- ------  ------ ------  ----------"
 WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top45 PAGE-TOP WIDTH 190 STREAM-IO.

  DISPLAY "" WITH FRAME f-top45.

IF tb_excel THEN DO:
 /* OUTPUT STREAM excel TO VALUE(fi_file).*/
  excelHeader = 'ACCOUNT,ACT DESCRIPTION,CHECK#,PO#,DATE,VENDOR#,INVOICE#,LINE,DESCRIPTION,QTY,UNIT PRICE,AMT PAID'.
  PUT STREAM excel UNFORMATTED SKIP(2)  ",,,,,Summary by Account" SKIP (2) '"' REPLACE(excelHeader,',','","') '"' SKIP.
END. /* if tb_excel */

ASSIGN v-acct-dscr = "" .

FOR EACH tt-report:

    /*exclude voided checks*/
    IF NOT CAN-FIND(FIRST b-tt-report where
       b-tt-report.key-01 = tt-report.key-01 AND
       b-tt-report.key-02 = tt-report.key-02 AND
       b-tt-report.inv-no = "Void" AND
       ROWID(b-tt-report) NE ROWID(tt-report)) THEN
       DO:
          CREATE tt-report2.
          BUFFER-COPY tt-report TO tt-report2.
          RELEASE tt-report2.
       END.
END.

FOR EACH tt-report2,
    FIRST ap-pay NO-LOCK WHERE
      ROWID(ap-pay) EQ tt-report2.row-id,
    FIRST ap-payl WHERE
          ap-payl.c-no EQ tt-report2.c-no AND
          ap-payl.LINE EQ tt-report2.LINE
          NO-LOCK,
    FIRST ap-inv WHERE
          ap-inv.company EQ ap-pay.company AND
          ap-inv.vend-no EQ ap-payl.vend-no AND
          ap-inv.inv-no  EQ ap-payl.inv-no AND
          ap-inv.freight NE 0 NO-LOCK,
    FIRST vend NO-LOCK WHERE
          vend.company EQ ap-inv.company AND
          vend.vend-no EQ ap-inv.vend-no
          USE-INDEX vend
    BREAK BY ap-inv.vend-no
          BY ap-inv.inv-no:

    IF FIRST(ap-inv.inv-no) THEN DO:
       FIND FIRST account WHERE
            account.company EQ ap-inv.company AND
            account.actnum  EQ v-frt-acct
            NO-LOCK NO-ERROR.

       ASSIGN v-acct-dscr = (IF AVAIL account THEN account.dscr ELSE "Not on file") .

       PUT v-frt-acct + " - " +
          (IF AVAIL account THEN account.dscr ELSE "Not on file") FORMAT "x(40)"
          SKIP.
    END. /* FIRST(ap-inv.vend-no)*/

    ASSIGN v-frgt-amt = ap-payl.amt-paid * 
                        (ap-inv.freight / (ap-inv.net + ap-inv.freight)).

    PUT ap-payl.check-no FORMAT ">>>>>>>>" AT 6
        ap-inv.inv-date         AT 41   FORMAT "99/99/99"
        SPACE(1)
        ap-inv.vend-no
        SPACE(1)
        ap-inv.inv-no
        SPACE(6)
        "Freight"                       FORMAT "x(18)"
        SPACE(7)
        1.0                             FORMAT "9.9"
        SPACE(1)
        ap-inv.freight          TO 118
        v-frgt-amt              TO 131
        SKIP.

    ACCUM v-frgt-amt (TOTAL).

    IF LAST(ap-inv.inv-no) THEN
       PUT "** TOTAL " TO 114
           (ACCUM TOTAL v-frgt-amt) FORMAT "->>,>>>,>>9.99" TO 128
           " *" SKIP(1).

    IF tb_excel THEN
    PUT STREAM excel UNFORMATTED
        '"' STRING(v-frt-acct) '",'
        '"' v-acct-dscr '",'
        '"' ap-payl.check-no '",'
        '"' "" '",'
        '"' ap-inv.inv-date '",'
        '"' ap-inv.vend-no '",'
        '"' ap-inv.inv-no '",'
        '"' "" '",'
        '"' "Freight" '",'
        '"' "1.0" '",'
        '"' ap-inv.freight '",'
        '"' v-frgt-amt '",'
        SKIP.

END. /* FOR EACH tt-report */

EMPTY TEMP-TABLE tt-report2.

FOR EACH tt-report:

    /*exclude voided checks*/
    IF NOT CAN-FIND(FIRST b-tt-report where
       b-tt-report.key-01 = tt-report.key-01 AND
       b-tt-report.key-02 = tt-report.key-02 AND
       b-tt-report.inv-no = "Void" AND
       ROWID(b-tt-report) NE ROWID(tt-report)) THEN
       DO:
          CREATE tt-report2.
          BUFFER-COPY tt-report TO tt-report2.
          RELEASE tt-report2.
       END.
END.

ASSIGN v-acct-dscr = "" .

FOR EACH tt-report2,
    FIRST ap-pay NO-LOCK WHERE
          ROWID(ap-pay) EQ tt-report2.row-id,
    FIRST ap-payl WHERE
          ap-payl.c-no EQ tt-report2.c-no AND
          ap-payl.LINE EQ tt-report2.LINE
          NO-LOCK,
    FIRST ap-inv WHERE
          ap-inv.company EQ ap-pay.company AND
          ap-inv.vend-no EQ ap-payl.vend-no AND
          ap-inv.inv-no  EQ ap-payl.inv-no
          NO-LOCK,
    FIRST vend WHERE
          vend.company EQ ap-inv.company AND
          vend.vend-no EQ ap-inv.vend-no
          NO-LOCK,
    EACH ap-invl WHERE
         ap-invl.i-no EQ ap-inv.i-no USE-INDEX i-no
         NO-LOCK
    BREAK BY ap-invl.actnum
          BY ap-invl.inv-no
          BY ap-invl.LINE
    WITH WIDTH 132 NO-LABELS:

    IF FIRST-OF(ap-invl.actnum) THEN DO:
       FIND FIRST account WHERE
            account.company eq ap-inv.company AND
            account.actnum  eq ap-invl.actnum
            NO-LOCK NO-ERROR.

       ASSIGN v-acct-dscr = (IF AVAIL account THEN account.dscr ELSE "Not on file")  .

       PUT ap-invl.actnum + " - " +
           (IF AVAIL account THEN account.dscr ELSE "Not on file") FORMAT "x(40)" SKIP.
    END.

    ASSIGN v-line-amt = ap-payl.amt-paid * 
                        (ap-invl.amt / (ap-inv.net + ap-inv.freight)).

    PUT ap-payl.check-no FORMAT ">>>>>>>>" AT 6
        ap-invl.po-no         AT 34
        SPACE(1)
        ap-inv.inv-date       FORMAT "99/99/99"
        SPACE(1)
        ap-inv.vend-no
        SPACE(1)
        ap-inv.inv-no
        SPACE(1)
        {ap/invlline.i -1}    FORMAT ">>>9"
        SPACE(1)
        ap-invl.dscr          FORMAT "x(18)"
        SPACE(1)
        ap-invl.qty           FORMAT "->>,>>9.9<<"
        SPACE(1)
        ap-invl.unit-pr
        SPACE(1)
        v-line-amt
        SPACE(1)
        SKIP.

    ACCUM v-line-amt (TOTAL BY ap-invl.actnum).
    ACCUM v-line-amt (TOTAL).

    IF LAST-OF(ap-invl.actnum) THEN
       PUT "** TOTAL " TO 114
           (ACCUM TOTAL BY ap-invl.actnum v-line-amt) FORMAT "->>,>>>,>>9.99" TO 128
           " *" SKIP(1).

    IF tb_excel THEN
    PUT STREAM excel UNFORMATTED
        '"' STRING(ap-invl.actnum) '",'
        '"' v-acct-dscr '",'
        '"' ap-payl.check-no '",'
        '"' ap-invl.po-no '",'
        '"' ap-inv.inv-date '",'
        '"' ap-inv.vend-no '",'
        '"' ap-inv.inv-no '",'
        '"' {ap/invlline.i -1} '",'
        '"' ap-invl.dscr '",'
        '"' ap-invl.qty '",'
        '"' ap-invl.unit-pr '",'
        '"' v-line-amt '",'
        SKIP.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/*{sys/form/r-topw.f}*/

DEF VAR v-check-no LIKE ap-pay.check-no NO-UNDO.
DEF VAR v-amt-disc  LIKE ap-payl.amt-disc NO-UNDO.
DEF VAR v-gross-amt LIKE ap-payl.amt-paid NO-UNDO.
DEF VAR v-amt-paid LIKE ap-payl.amt-paid NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR v-vend-name LIKE vend.NAME NO-UNDO.


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
DEF VAR str-line2 AS cha FORM "x(300)" NO-UNDO.

{sys/form/r-top5DL3.f} 
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.


FORM tt-report.check-no    FORMAT ">>>>>>>>"  COLUMN-LABEL "Check#"
     tt-report.check-date  FORMAT "99/99/99"  COLUMN-LABEL "Chk Date"
     tt-report.inv-no                         COLUMN-LABEL "Invoice#"
     tt-report.vend-no                        COLUMN-LABEL "Vendor#"
     tt-report.vend-name                      COLUMN-LABEL "Name"
     tt-report.due-date    FORMAT "99/99/99"  COLUMN-LABEL "Due Date"
     tt-report.gross-amt                      COLUMN-LABEL "Gross Amt"
     tt-report.amt-disc                       COLUMN-LABEL "Discount"
     tt-report.amt-paid                       COLUMN-LABEL "Net Amt"

    WITH NO-BOX FRAME ap-chk DOWN WIDTH 180 STREAM-IO.  


SESSION:SET-WAIT-STATE ("general").

ASSIGN
 str-tit2 = c-win:TITLE
 {sys/inc/ctrtext.i str-tit2 112}. 

/*"CHECK#,CHK DATE,INVOICE#,VENDOR#,VEND NAME,DUE DATE,GROSS AMT,DISCOUNT,NET AMT," +
                           "ACCOUNT,PO#,ACC DATE,LINE,DESCRIPTION,QTY,UNIT PRICE,AMT PAID"*/

DEF VAR cslist AS cha NO-UNDO.
 FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:
   IF LOOKUP(ttRptSelected.TextList, "CHECK#,CHK DATE,INVOICE#,VENDOR#,VEND NAME,DUE DATE,GROSS AMT,DISCOUNT,NET AMT") <> 0    THEN
       DO:

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
            IF LOOKUP(ttRptSelected.TextList, "CHECK#,INVOICE#,VENDOR#,VEND NAME,GROSS AMT,DISCOUNT,NET AMT,") <> 0    THEN
                ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
            ELSE
                str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 

           IF LOOKUP(ttRptSelected.TextList, "GROSS AMT,DISCOUNT,NET AMT") <> 0    THEN
                ASSIGN
                str-line2 = str-line2 + FILL("-",ttRptSelected.FieldLength) + " " .
            ELSE
                str-line2 = str-line2 + FILL(" ",ttRptSelected.FieldLength) + " " . 
   END.
 END.

{sys/inc/print1.i}
IF rd-dest EQ 1 THEN
    IF lines-per-page GT 94  THEN
        ASSIGN lines-per-page = 94 .
{sys/inc/outprint.i VALUE(lines-per-page)}
    
IF td-show-parm THEN RUN show-param.
DISPLAY "" WITH FRAME r-top.

FOR EACH tt-report:
  DELETE tt-report.
END.

EMPTY TEMP-TABLE tt-report2.

FOR EACH ap-pay
    WHERE ap-pay.company    EQ cocode
      AND ap-pay.vend-no    GE begin_vend
      AND ap-pay.vend-no    LE end_vend
    /*Note - if posting is checked, the report takes ages to run since the
    ap-pay table was not filtered by a date range.  The workaround was to add
    range of dates +/- 120 days from the check dates entered.*/
      AND ((ap-pay.check-date GE begin_date
      AND ap-pay.check-date LE end_date) OR 
           (tb_post-date AND (ap-pay.check-date GE begin_date - 120
      AND ap-pay.check-date LE end_date + 120)))  
      AND ap-pay.check-no   GE begin_check
      AND ap-pay.check-no   LE end_check
      AND ap-pay.bank-code  GE begin_bank
      AND ap-pay.bank-code  LE end_bank
      AND ap-pay.posted     EQ YES
      AND ap-pay.memo       EQ NO
    USE-INDEX vend-no NO-LOCK,

    EACH ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no NO-LOCK

    BREAK BY ap-pay.check-act
          BY ap-pay.check-no
          BY ap-payl.inv-no
          BY ap-payl.line
          BY ap-payl.amt-paid:

    {custom/statusMsg.i " 'Processing Vendor #  '  + string(ap-pay.vend-no) "}

    FIND first ap-ledger
      where ap-ledger.company  eq ap-pay.company
      and ap-ledger.vend-no  eq ap-pay.vend-no      
      and ap-ledger.refnum   eq ("AC" + string(ap-pay.check-no,"999999"))      
      and ((ap-ledger.tr-date GE begin_date AND ap-ledger.tr-date le end_date) OR NOT tb_post-date)
      /*use-index ap-ledger */NO-LOCK NO-ERROR.

    IF NOT AVAIL ap-ledger THEN DO:
         FIND FIRST bank NO-LOCK
           WHERE bank.company   EQ cocode
            AND bank.bank-code EQ ap-pay.bank-code
        NO-ERROR.
        FIND first ap-ledger
            where ap-ledger.company  eq ap-pay.company
            and ap-ledger.vend-no  eq ap-pay.vend-no      
            and ap-ledger.refnum   eq ("CHK# " + string(ap-pay.check-no) +
                                           " CD#" + bank.bank-code)      
            and ((ap-ledger.tr-date GE begin_date AND ap-ledger.tr-date le end_date) OR NOT tb_post-date)
            /*use-index ap-ledger */NO-LOCK NO-ERROR.
    END.


  ASSIGN
     v-vend-name = ""
     li = li + 1.

  IF FIRST-OF(ap-pay.check-no) THEN v-check-no = ap-pay.check-no.

  v-gross-amt = v-gross-amt + (ap-payl.amt-paid + ap-payl.amt-disc).

  IF FIRST-OF(ap-payl.inv-no) THEN DO:
    CREATE tt-report.

    /* rtc start */
    FIND FIRST ap-dis WHERE ap-dis.company   EQ ap-pay.company
                        AND ap-dis.check-no  EQ ap-pay.check-no
                        AND ap-dis.bank-code EQ ap-pay.bank-code
                        AND ap-dis.vend-no   EQ ap-pay.vend-no NO-LOCK NO-ERROR. 
     IF AVAILABLE(ap-dis) THEN
         v-vend-name = ap-dis.payee.
     ELSE DO: 
        FIND FIRST vend WHERE vend.company EQ cocode 
                          AND vend.vend-no EQ ap-pay.vend-no NO-LOCK NO-ERROR.
        IF AVAILABLE(vend) THEN
           v-vend-name = vend.NAME.
     END.

    /* rtc end */

    ASSIGN
     tt-report.rec-id     = RECID(ap-pay)
     tt-report.key-01     = ap-pay.check-act
     tt-report.key-02     = STRING(ap-pay.check-no,"9999999999")
     tt-report.key-03     = ap-payl.inv-no
     tt-report.check-no   = v-check-no
     tt-report.check-date = IF v-check-no NE 0 THEN ap-pay.check-date ELSE ?
     tt-report.inv-no     = ap-payl.inv-no
     tt-report.due-date   = ap-payl.due-date
     tt-report.gross-amt  = ap-payl.amt-paid + ap-payl.amt-disc
     tt-report.amt-disc   = ap-payl.amt-disc
     tt-report.amt-paid   = ap-payl.amt-paid
     tt-report.c-no       = ap-payl.c-no
     tt-report.LINE       = ap-payl.LINE
     tt-report.vend-no    = ap-pay.vend-no
     tt-report.vend-name  = v-vend-name
     v-amt-disc           = v-amt-disc + ap-payl.amt-disc
     v-check-no = 0
     tt-report.row-id       = ROWID(ap-pay).

     RELEASE vend.
  END.

  IF LAST-OF(ap-pay.check-no) THEN DO:
    IF NOT FIRST-OF(ap-pay.check-no) OR v-gross-amt EQ 0 THEN DO:
      CREATE tt-report.

      FIND FIRST vend WHERE
           vend.company EQ cocode AND
           vend.vend-no EQ ap-pay.vend-no
           NO-LOCK NO-ERROR.

       /* rtc start */
       FIND FIRST ap-dis WHERE ap-dis.company   EQ ap-pay.company
                           AND ap-dis.check-no  EQ ap-pay.check-no
                           AND ap-dis.bank-code EQ ap-pay.bank-code
                           AND ap-dis.vend-no   EQ ap-pay.vend-no NO-LOCK NO-ERROR. 
        IF AVAILABLE(ap-dis) THEN
            v-vend-name = ap-dis.payee.
        ELSE DO: 
           FIND FIRST vend WHERE vend.company EQ cocode 
                             AND vend.vend-no EQ ap-pay.vend-no NO-LOCK NO-ERROR.
           IF AVAILABLE(vend) THEN
              v-vend-name = vend.NAME.
        END.
       /* rtc end */

      ASSIGN
       tt-report.key-01     = ap-pay.check-act
       tt-report.key-02     = STRING(ap-pay.check-no,"9999999999")
       tt-report.key-03     = FILL("z",100) + "TOTAL"
       tt-report.check-no   = ap-pay.check-no
       tt-report.check-date = ap-pay.check-date
       tt-report.inv-no     = IF v-gross-amt EQ 0 THEN "Void" ELSE ""
       tt-report.due-date   = ap-payl.due-date
       tt-report.gross-amt  = ap-pay.check-amt + v-amt-disc
       tt-report.amt-disc   = v-amt-disc
       tt-report.amt-paid   = ap-pay.check-amt
       tt-report.vend-no    = ap-pay.vend-no
       tt-report.vend-name  = v-vend-name.

      IF tt-report.inv-no EQ "Void" THEN
        ASSIGN
         tt-report.gross-amt = tt-report.gross-amt * -1
         tt-report.amt-disc  = tt-report.amt-disc * -1
         tt-report.amt-paid  = tt-report.amt-paid * -1.

      RELEASE vend.
    END.

    ASSIGN
     v-gross-amt = 0
     v-amt-disc  = 0. 
  END.
END.

/* gdm - */


IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  /*excelHeader = 'Check#,Check Date,Vendor#,Name,Invoice#,Due Date,Gross Amt,Discount,Net Amt'.*/
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelHeader,',','","') '"' SKIP.
END. /* if tb_excel */

RELEASE tt-report.

ASSIGN
 v-gross-amt = 0
 v-amt-disc  = 0
 v-amt-paid  = 0.

IF CAN-FIND(FIRST tt-report) THEN
FOR EACH tt-report NO-LOCK /*WITH FRAME ap-chk*/
    BREAK BY tt-report.key-01
          BY tt-report.key-02
          BY tt-report.key-03:

    {custom/statusMsg.i " 'Processing Vendor #  '  + string(tt-report.vend-no) "}

  IF tt-report.key-03 EQ FILL("z",100) + "TOTAL" THEN DO:
   PUT str-line FORMAT "x(125)" SKIP .
   /* UNDERLINE tt-report.check-no
              tt-report.check-date
              tt-report.vend-no
              tt-report.vend-name
              tt-report.gross-amt
              tt-report.amt-disc
              tt-report.amt-paid. 
    DOWN.

    CLEAR NO-PAUSE.*/ 
  END.

  IF tt-report.key-03 NE FILL("z",100) + "TOTAL" OR tt-report.inv-no EQ "Void" THEN
    ASSIGN
     v-gross-amt = v-gross-amt + tt-report.gross-amt
     v-amt-disc  = v-amt-disc  + tt-report.amt-disc
     v-amt-paid  = v-amt-paid  + tt-report.amt-paid.



 /* DISPLAY tt-report.check-no
          tt-report.check-date
          tt-report.vend-no
          tt-report.vend-name
          tt-report.inv-no
          tt-report.due-date
          tt-report.gross-amt
          tt-report.amt-disc
          tt-report.amt-paid.
  DOWN.*/
/*"chk,chk-date,inv,vend,vend-name,due-date,gross-amt,dis,net-amt," +
                            "acc,po,acc-date,line,desc,qty,unit-pri,amt-paid"  
                            cFieldLength = "8,8,12,8,30,8,14,10,14," + "7,15,7,4,20,6,13,12"
       cFieldType = "i,c,c,c,c,i,i,i," + "c,c,c,i,c,i,i,i"
                            */
  ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "chk"    THEN cVarValue = string(tt-report.check-no,">>>>>>>9") .
                         WHEN "chk-date"   THEN cVarValue = IF tt-report.check-date <> ? THEN string(tt-report.check-date,"99/99/99") ELSE "".
                         WHEN "inv"   THEN cVarValue = STRING(tt-report.inv-no,"x(12)").
                         WHEN "vend"  THEN cVarValue = STRING(tt-report.vend-no,"x(8)") .
                         WHEN "vend-name"   THEN cVarValue = STRING(tt-report.vend-name,"x(30)") .
                         WHEN "due-date"  THEN cVarValue = IF tt-report.due-date <> ? THEN STRING(tt-report.due-date,"99/99/99") ELSE "" .
                         WHEN "gross-amt"   THEN cVarValue = STRING(tt-report.gross-amt,"->>,>>>,>>9.99") .
                         WHEN "dis"  THEN cVarValue = STRING(tt-report.amt-disc,"->>>>>9.99") .
                         WHEN "net-amt"  THEN cVarValue = STRING(tt-report.amt-paid,"->>,>>>,>>9.99") .
                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED cDisplay FORMAT "x(125)" SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.

  /*IF tb_excel THEN
    PUT STREAM excel UNFORMATTED
        '"' STRING(tt-report.check-no,">>>>>>>>") '",'
        '"' (IF tt-report.check-date NE ? THEN STRING(tt-report.check-date,"99/99/99") ELSE "") '",'
        '"' tt-report.vend-no '",'
        '"' tt-report.vend-name '",'
        '"' tt-report.inv-no '",'
        '"' (IF tt-report.due-date NE ? THEN STRING(tt-report.due-date,"99/99/9999") ELSE "") '",'
        '"' tt-report.gross-amt '",'
        '"' tt-report.amt-disc '",'
        '"' tt-report.amt-paid '"'
        SKIP.*/

  RELEASE vend NO-ERROR.

  IF LAST-OF(tt-report.key-02) THEN /*DOWN 2*/ PUT SKIP(2) .

  IF LAST(tt-report.key-01) THEN DO:
    PUT str-line2 FORMAT "x(125)" SKIP .

    ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "chk"    THEN cVarValue = "" .
                         WHEN "chk-date"   THEN cVarValue = "".
                         WHEN "inv"   THEN cVarValue = "".
                         WHEN "vend"  THEN cVarValue = "" .
                         WHEN "vend-name"   THEN cVarValue = "" .
                         WHEN "due-date"  THEN cVarValue =  "" .
                         WHEN "gross-amt"   THEN cVarValue = STRING(v-gross-amt,"->>,>>>,>>9.99") .
                         WHEN "dis"  THEN cVarValue = STRING(v-amt-disc,"->>>>>9.99") .
                         WHEN "net-amt"  THEN cVarValue = STRING(v-amt-paid,"->>,>>>,>>9.99") .
                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED cDisplay FORMAT "x(125)" SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.
  END.
END.

IF tb_prt-acc THEN DO:
    ASSIGN str-tit4 = ""
        str-tit5 = "" .
    PAGE. 
    RUN print-gl-acct. 
END.


OUTPUT STREAM excel CLOSE.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2004 Advanced Software, Inc. */

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

