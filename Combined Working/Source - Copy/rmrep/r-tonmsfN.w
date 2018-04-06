&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: rmrep\r-tonmsf.w

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
DEF VAR ls-fax-file AS CHAR NO-UNDO.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report.
def stream s-excel.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg .
DEF VAR cTextListToDefault AS cha NO-UNDO.

ASSIGN cTextListToSelect = "TRANS TYPE,ITEM,DESCRIPTION,DATE,P.O.#,VENDOR#,JOB#," +
                           "QUANTITY,MSF,TONS,COST,VALUE" 
       cFieldListToSelect = "trn-typ,i-no,dscr,dat,po,vend,job," +
                            "qty,msf,ton,cst,val"
       cFieldLength = "10,10,15,10,9,8,10," + "12,13,13,10,17" 
       cFieldType = "c,c,c,c,c,c,c," + "i,i,i,i,i"  
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "TRANS TYPE,ITEM,DESCRIPTION,DATE,P.O.#,VENDOR#,JOB#," +
                           "QUANTITY,MSF,TONS,COST,VALUE" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_rm-no end_rm-no ~
begin_procat end_procat begin_date end_date begin_vend end_vend tb_receipts ~
tb_adjustments tb_issues tb_counts tb_transfers sl_avail Btn_Def ~
sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest lv-ornt ~
lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_rm-no end_rm-no begin_procat ~
end_procat begin_date end_date begin_vend end_vend tb_receipts ~
tb_adjustments tb_issues tb_counts tb_transfers sl_avail sl_selected ~
rd-dest lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm ~
tb_excel tb_runExcel fi_file 

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_procat AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_rm-no AS CHARACTER FORMAT "X(10)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Vendor" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
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

DEFINE VARIABLE end_vend AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Vendor" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\tmp~\r-tonmsf.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9  NO-UNDO.

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

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 10.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 9.52.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tb_adjustments AS LOGICAL INITIAL yes 
     LABEL "Adjustments?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE tb_counts AS LOGICAL INITIAL yes 
     LABEL "Cycle Counts?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL no 
     LABEL "Output to Excel File?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_issues AS LOGICAL INITIAL yes 
     LABEL "Issues?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE tb_receipts AS LOGICAL INITIAL yes 
     LABEL "Receipts?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_transfers AS LOGICAL INITIAL yes 
     LABEL "Transfers?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_rm-no AT ROW 2.19 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Item Number"
     end_rm-no AT ROW 2.19 COL 69 COLON-ALIGNED HELP
          "Enter Ending Item number"
     begin_procat AT ROW 3.14 COL 28 COLON-ALIGNED HELP
          "Enter Begining Category"
     end_procat AT ROW 3.14 COL 69 COLON-ALIGNED HELP
          "Enter Ending Category"
     begin_date AT ROW 4.1 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 4.1 COL 69 COLON-ALIGNED HELP
          "Enter ending Date"
     begin_vend AT ROW 5.05 COL 28 COLON-ALIGNED HELP
          "Enter Beginng Vendor"
     end_vend AT ROW 5.05 COL 69 COLON-ALIGNED HELP
          "Enter Endng Vendor"
     tb_receipts AT ROW 7.43 COL 29.6
     tb_adjustments AT ROW 7.43 COL 52.4
     tb_issues AT ROW 8.38 COL 29.6
     tb_counts AT ROW 8.38 COL 52.4
     tb_transfers AT ROW 9.33 COL 29.6
     sl_avail AT ROW 11.48 COL 4.4 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 11.48 COL 40.4 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 11.48 COL 59.8 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 12.48 COL 40.4 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 13.48 COL 40.4 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 14.52 COL 40.4 WIDGET-ID 40
     btn_down AT ROW 15.57 COL 40.4 WIDGET-ID 42
     rd-dest AT ROW 18.29 COL 5.8 NO-LABEL
     lv-ornt AT ROW 18.29 COL 29.8 NO-LABEL
     lines-per-page AT ROW 18.29 COL 82.8 COLON-ALIGNED
     lv-font-no AT ROW 20.1 COL 33.8 COLON-ALIGNED
     lv-font-name AT ROW 21.05 COL 27.8 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 22.24 COL 29.8
     tb_excel AT ROW 23.91 COL 29.8 WIDGET-ID 2
     tb_runExcel AT ROW 23.91 COL 54.8 WIDGET-ID 4
     fi_file AT ROW 24.95 COL 27.8 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 6
     btn-ok AT ROW 27.62 COL 26.8
     btn-cancel AT ROW 27.62 COL 58.8
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 10.57 COL 5.4 WIDGET-ID 38
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 17.33 COL 1.8
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Transaction Types" VIEW-AS TEXT
          SIZE 20 BY 1.19 AT ROW 6.29 COL 12
          FGCOLOR 9 
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 10.67 COL 60 WIDGET-ID 44
     RECT-6 AT ROW 16.86 COL 1
     RECT-7 AT ROW 1 COL 1.2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 95.8 BY 28.24.


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
         TITLE              = "RM Transaction by TON/MSF"
         HEIGHT             = 28.24
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
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_procat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_rm-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_procat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_rm-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_adjustments:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_counts:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_issues:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_receipts:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_transfers:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* RM Transaction by TON/MSF */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* RM Transaction by TON/MSF */
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


&Scoped-define SELF-NAME begin_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_procat C-Win
ON LEAVE OF begin_procat IN FRAME FRAME-A /* Beginning Category */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rm-no C-Win
ON LEAVE OF begin_rm-no IN FRAME FRAME-A /* Beginning Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend C-Win
ON LEAVE OF begin_vend IN FRAME FRAME-A /* Beginning Vendor */
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
    ASSIGN {&DISPLAYED-OBJECTS}.
  END.

  SESSION:SET-WAIT-STATE("general").
  RUN GetSelectionList.
  run run-report. 

  STATUS DEFAULT "Processing Complete". 
  SESSION:SET-WAIT-STATE("").

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type= "Vendor "
                            &begin_cust= "begin_vend"
                            &END_cust= "begin_vend" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = "Vendor "
                             &begin_cust= "begin_vend"
                             &END_cust= "begin_vend"
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Vendor "
                                  &begin_cust="begin_vend"
                                  &END_cust="begin_vend"
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.
       END. 
       WHEN 6 THEN RUN OUTPUT-to-port.
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


&Scoped-define SELF-NAME end_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_procat C-Win
ON LEAVE OF end_procat IN FRAME FRAME-A /* Ending Category */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_rm-no C-Win
ON LEAVE OF end_rm-no IN FRAME FRAME-A /* Ending Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend C-Win
ON LEAVE OF end_vend IN FRAME FRAME-A /* Ending Vendor */
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


&Scoped-define SELF-NAME tb_adjustments
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_adjustments C-Win
ON VALUE-CHANGED OF tb_adjustments IN FRAME FRAME-A /* Adjustments? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_counts
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_counts C-Win
ON VALUE-CHANGED OF tb_counts IN FRAME FRAME-A /* Cycle Counts? */
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
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Output to Excel File? */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_issues
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_issues C-Win
ON VALUE-CHANGED OF tb_issues IN FRAME FRAME-A /* Issues? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_receipts
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_receipts C-Win
ON VALUE-CHANGED OF tb_receipts IN FRAME FRAME-A /* Receipts? */
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


&Scoped-define SELF-NAME tb_transfers
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_transfers C-Win
ON VALUE-CHANGED OF tb_transfers IN FRAME FRAME-A /* Transfers? */
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
def var v-mat-list as char no-undo.
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

  end_date = today.

  RUN DisplaySelectionList.
  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    APPLY "entry" TO begin_rm-no.
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
  DISPLAY begin_rm-no end_rm-no begin_procat end_procat begin_date end_date 
          begin_vend end_vend tb_receipts tb_adjustments tb_issues tb_counts 
          tb_transfers sl_avail sl_selected rd-dest lv-ornt lines-per-page 
          lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_rm-no end_rm-no begin_procat end_procat begin_date 
         end_date begin_vend end_vend tb_receipts tb_adjustments tb_issues 
         tb_counts tb_transfers sl_avail Btn_Def sl_selected Btn_Add Btn_Remove 
         btn_Up btn_down rd-dest lv-ornt lines-per-page lv-font-no td-show-parm 
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
/* ---------------------------------------------- rm/rep/rm-trans.p 07/98 JLF */
/* raw materials - transactions edit list                                     */
/* -------------------------------------------------------------------------- */

/*{sys/form/r-topw.f}*/

def var v-fvend like item.vend-no NO-UNDO.
def var v-tvend like v-fvend                  init "zzzzzzzz" NO-UNDO.
def var v-fitem like rm-rcpth.i-no NO-UNDO.
def var v-titem like v-fitem                  init "zzzzzzzzzz" NO-UNDO.
def var v-fpcat like item.procat NO-UNDO.
def var v-tpcat like v-fpcat                  init "zzzzz" NO-UNDO.
def var v-fdate as   date format "99/99/99"   init 01/01/0001 NO-UNDO.
def var v-tdate like v-fdate                  init TODAY NO-UNDO.
def var v-type  as   char format "x(5)"       init "RITAC" NO-UNDO.
def var v-code  like rm-rcpth.rita-code NO-UNDO.

def var v-value as dec format "->,>>>,>>>,>>9.99" NO-UNDO.
def var v-job-no as char format "x(9)" NO-UNDO.
def var v-qty like rm-rdtlh.qty extent 3 NO-UNDO.
def var v-val like v-value extent 3 NO-UNDO.
def var v-first as log extent 3 NO-UNDO.
def var v-msf as dec extent 4 NO-UNDO.
def var v-ton as dec extent 4 NO-UNDO.
def var v-wid like po-ordl.s-wid NO-UNDO.
def var v-len like po-ordl.s-len NO-UNDO.
def var v-dep like item.s-dep NO-UNDO.
def var v-bwt like item.basis-w NO-UNDO.

DEF VAR v-i-no       AS CHAR NO-UNDO.
DEF VAR v-i-name     AS CHAR NO-UNDO.
DEF VAR v-trans-date AS CHAR NO-UNDO.
DEF VAR v-vend-no    AS CHAR NO-UNDO.

DEF VAR v-export     AS LOGICAL NO-UNDO.
DEF VAR v-excel-hdr  AS CHAR NO-UNDO.
DEF VAR v-exp-name   AS CHAR FORMAT "x(40)" INIT "c:\tmp\r-tonsmsf.csv" NO-UNDO.

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

{custom/statusMsg.i "'Processing...'"} 

form header ""

    with frame r-top STREAM-IO WIDTH 332 no-labels no-box no-underline page-top.

form rm-rcpth.i-no label "ITEM"
     rm-rcpth.i-name format "x(14)" label "DESCRIPTION"
     rm-rcpth.trans-date label "DATE" FORM "99/99/99"
     rm-rcpth.po-no label "P.O.#"
     po-ord.vend-no label "Vendor#"
     v-job-no label "   Job #"
     rm-rdtlh.qty format "->>>>>>>9.99<<" label "QUANTITY"
     v-msf[4] format "->>>>>>9.9999" label "MSF"
     v-ton[4] format "->>>>>>9.9999" label "TONS"
     rm-rdtlh.cost format "->>>>>9.99<<<<" label "COST"
     space(0)
     v-value label "VALUE"  
     skip

    with frame itemx no-box down STREAM-IO width 432.

assign
   str-tit2 = c-win:title
   {sys/inc/ctrtext.i str-tit2 112}
   v-fitem = begin_rm-no
   v-titem = end_rm-no
   v-fpcat = begin_procat
   v-tpcat = end_procat
   v-fdate = begin_date
   v-tdate = end_date
   v-fvend = begin_vend
   v-tvend = end_vend
   v-type  = (if tb_receipts    then "R" else "") +
             (if tb_issues      then "I" else "") +
             (if tb_transfers   then "T" else "") +
             (if tb_adjustments then "A" else "") +
             (if tb_counts      then "C" else "")
   v-export = tb_excel
   v-exp-name = fi_file.


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

        IF LOOKUP(ttRptSelected.TextList, "QUANTITY,MSF,TONS,VALUE") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

/*IF v-export THEN
   OUTPUT STREAM s-excel TO VALUE(v-exp-name).*/


IF tb_excel THEN DO:
  OUTPUT STREAM s-excel TO VALUE(TRIM(fi_file)).
  PUT STREAM s-excel UNFORMATTED excelheader SKIP.
END. 

SESSION:SET-WAIT-STATE ("general").

EMPTY TEMP-TABLE tt-report.

/*IF v-export THEN 
   PUT STREAM s-excel UNFORMATTED 
      'TRANS TYPE,ITEM,DESCRIPTION,DATE,P.O.#,VENDOR#,JOB#,QUANTITY,MSF,TONS,COST,VALUE'                 
      SKIP.   */

for each rm-rcpth where rm-rcpth.company    eq cocode
                    and rm-rcpth.i-no       ge v-fitem
                    and rm-rcpth.i-no       le v-titem
                    and rm-rcpth.trans-date ge v-fdate
                    and rm-rcpth.trans-date le v-tdate
                    and index(caps(v-type),rm-rcpth.rita-code) gt 0
                    and (can-find(first po-ord
                                  where po-ord.company eq cocode
                                    and po-ord.po-no   eq int(rm-rcpth.po-no)
                                    and po-ord.vend-no ge v-fvend
                                    and po-ord.vend-no le v-tvend) or
                         ("" ge v-fvend and "" le v-tvend))
                    use-index i-no no-lock,

   first ITEM where item.company eq cocode
                and item.i-no    eq rm-rcpth.i-no
                and item.procat  ge v-fpcat
                and item.procat  le v-tpcat no-lock,

   each rm-rdtlh where rm-rdtlh.r-no      eq rm-rcpth.r-no
                   and rm-rdtlh.rita-code eq rm-rcpth.rita-code NO-LOCK
              break by rm-rcpth.rita-code
                    by rm-rcpth.i-no
                    by rm-rcpth.trans-date

      transaction:

    {custom/statusMsg.i "'Processing Item # ' + string(rm-rcpth.i-no)"} 

         v-code = rm-rcpth.rita-code.

         if first-of(rm-rcpth.rita-code) then do:
           if first(rm-rcpth.rita-code) then view frame r-top.

           page.
         end.

         if first-of(rm-rcpth.trans-date) then v-first[1] = yes.
         if first-of(rm-rcpth.i-no)       then v-first[2] = yes.

         assign
          v-job-no = fill(" ",6 - length(trim(rm-rdtlh.job-no))) +
                     trim(rm-rdtlh.job-no) + "-" + string(rm-rdtlh.job-no2,"99")
          v-value  = rm-rdtlh.cost * rm-rdtlh.qty
          v-bwt    = item.basis-w
          v-wid    = if item.r-wid eq 0 then item.s-wid else item.r-wid
          v-len    = if item.r-wid eq 0 then item.s-len else 12
          v-dep    = item.s-dep.

         release po-ordl.
         release po-ord.

         if rm-rcpth.po-no ne "" then
         find first po-ordl
             where po-ordl.company eq rm-rcpth.company
               and po-ordl.po-no   eq int(rm-rcpth.po-no)
               and po-ordl.i-no    eq rm-rcpth.i-no
               and po-ordl.s-num   eq rm-rdtlh.s-num
               and po-ordl.b-num   eq rm-rdtlh.b-num
             no-lock no-error.
         if rm-rcpth.po-no ne "" AND NOT AVAIL po-ordl then 
            find first po-ordl
             where po-ordl.company eq rm-rcpth.company
               and po-ordl.po-no   eq int(rm-rcpth.po-no)
               and po-ordl.i-no    eq rm-rcpth.i-no
               and po-ordl.s-num   eq rm-rdtlh.s-num
               no-lock no-error.

         if avail po-ordl then do:

           FIND FIRST po-ord WHERE
                po-ord.company EQ po-ordl.company AND
                po-ord.po-no   EQ po-ordl.po-no
                NO-LOCK NO-ERROR.

           assign
            v-wid = po-ordl.s-wid
            v-len = po-ordl.s-len.
         end.
         ELSE IF ITEM.i-code EQ "E" THEN
         DO:
            FIND FIRST job-mat WHERE
                 job-mat.company EQ rm-rcpth.company AND
                 job-mat.job-no EQ rm-rcpth.job-no AND
                 job-mat.job-no2 EQ rm-rcpth.job-no2 AND
                 job-mat.i-no EQ rm-rcpth.i-no AND
                 job-mat.frm EQ rm-rdtlh.s-num
                 NO-LOCK NO-ERROR.

            IF AVAIL job-mat THEN
               ASSIGN
                  v-wid = job-mat.wid
                  v-len = job-mat.len.
         END.

         if rm-rcpth.pur-uom eq "MSF" then
           v-msf[4] = rm-rdtlh.qty.
         else
           run sys/ref/convquom.p (rm-rcpth.pur-uom, "MSF",
                                   v-bwt, v-len, v-wid, v-dep,
                                   rm-rdtlh.qty, output v-msf[4]).

         if rm-rcpth.pur-uom eq "TON" then
           v-ton[4] = rm-rdtlh.qty.
         else
           run sys/ref/convquom.p (rm-rcpth.pur-uom, "TON",
                                   v-bwt, v-len, v-wid, v-dep,
                                   rm-rdtlh.qty, output v-ton[4]).

         if v-job-no begins "-" then v-job-no = "".

         /*display "" @ rm-rcpth.i-no
                 rm-rcpth.i-no       when first-of(rm-rcpth.i-no)
                 "" @ rm-rcpth.i-name
                 rm-rcpth.i-name     when first-of(rm-rcpth.i-no)
                 "" @ rm-rcpth.trans-date
                 rm-rcpth.trans-date when first-of(rm-rcpth.trans-date)
                 rm-rcpth.po-no
                 po-ord.vend-no      when avail po-ord
                 v-job-no
                 rm-rdtlh.qty
                 v-msf[4]
                 v-ton[4]
                 rm-rdtlh.cost
                 v-value
             with frame itemx.
         down with frame itemx. */

        /* IF v-export THEN DO: */
            IF FIRST-OF(rm-rcpth.i-no) THEN
               ASSIGN
                  v-i-no   = rm-rcpth.i-no
                  v-i-name = rm-rcpth.i-name.   
            ELSE
               ASSIGN
                  v-i-no   = ""
                  v-i-name = "".

            IF FIRST-OF(rm-rcpth.trans-date) THEN
               v-trans-date = STRING(rm-rcpth.trans-date).
            ELSE
               v-trans-date = "".

            IF AVAILABLE(po-ord) THEN
               v-vend-no = po-ord.vend-no.
            ELSE
               v-vend-no = "".

           /* PUT STREAM s-excel UNFORMATTED 
               '"' v-code           '",'
               '"' v-i-no           '",'
               '"' v-i-name         '",'
               '"' v-trans-date     '",'
               '"' rm-rcpth.po-no   '",'
               '"' v-vend-no        '",' 
               '"' v-job-no         '",'
               '"' rm-rdtlh.qty     '",'
               '"' v-msf[4]         '",'
               '"' v-ton[4]         '",'
               '"' rm-rdtlh.cost    '",'
               '"' v-value          '"'
               SKIP.
         END.  */

        ASSIGN cDisplay = ""
               cTmpField = ""
               cVarValue = ""
               cExcelDisplay = ""
               cExcelVarValue = "".

        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
           cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                     WHEN "trn-typ"  THEN cVarValue = string(v-code) .
                     WHEN "i-no"     THEN cVarValue = STRING(v-i-no,"x(10)") .
                     WHEN "dscr"     THEN cVarValue = string(v-i-name,"x(15)") .
                     WHEN "dat"      THEN cVarValue = string(v-trans-date) .
                     WHEN "po"       THEN cVarValue = string(rm-rcpth.po-no,"x(9)")  .
                     WHEN "vend"     THEN cVarValue = STRING(v-vend-no).
                     WHEN "job"      THEN cVarValue = string(v-job-no).
                     WHEN "qty"      THEN cVarValue = STRING(rm-rdtlh.qty,"->>>>>>>9.99<<") .
                     WHEN "msf"      THEN cVarValue = string(v-msf[4],"->>>>>>9.9999") .
                     WHEN "ton"      THEN cVarValue = string(v-ton[4],"->>>>>>9.9999") .
                     WHEN "cst"      THEN cVarValue = string(rm-rdtlh.cost,"->>>>>9.99<<<<") .
                     WHEN "val"      THEN cVarValue = STRING(v-value,"->,>>>,>>>,>>9.99")    .
                END CASE.
                IF cTmpField = "i-no" THEN
                    cExcelVarValue = rm-rcpth.i-no.
                ELSE IF  cTmpField = "dscr" THEN
                    cExcelVarValue = rm-rcpth.i-name.
                ELSE IF  cTmpField = "dat" THEN
                    cExcelVarValue = IF rm-rcpth.trans-date NE ? THEN string(rm-rcpth.trans-date) ELSE "".
                ELSE
                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                           FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.

        PUT UNFORMATTED cDisplay SKIP.
        IF tb_excel THEN DO:
             PUT STREAM s-excel UNFORMATTED  
                   cExcelDisplay SKIP.
        END.

         assign
          v-qty[1] = v-qty[1] + rm-rdtlh.qty
          v-val[1] = v-val[1] + v-value
          v-msf[1] = v-msf[1] + v-msf[4]
          v-ton[1] = v-ton[1] + v-ton[4].

         if last-of(rm-rcpth.trans-date) then do:
           if not v-first[1] then do:
             /*underline rm-rdtlh.qty
                       v-msf[4]
                       v-ton[4]
                       v-value
                 with frame itemx.

             display " DATE TOTALS" @ rm-rcpth.i-name
                     v-qty[1]       @ rm-rdtlh.qty
                     v-msf[1]       @ v-msf[4]
                     v-ton[1]       @ v-ton[4]
                     v-val[1]       @ v-value
                 with frame itemx.*/
               PUT SKIP str-line SKIP .
               ASSIGN cDisplay = ""
                      cTmpField = ""
                      cVarValue = ""
                      cExcelDisplay = ""
                      cExcelVarValue = "".

               DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                  cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                       CASE cTmpField:             
                            WHEN "trn-typ"  THEN cVarValue = "" .
                            WHEN "i-no"     THEN cVarValue = "" .
                            WHEN "dscr"     THEN cVarValue = "" .
                            WHEN "dat"      THEN cVarValue = "" .
                            WHEN "po"       THEN cVarValue = "" .
                            WHEN "vend"     THEN cVarValue = "" .
                            WHEN "job"      THEN cVarValue = "" .
                            WHEN "qty"      THEN cVarValue = STRING(v-qty[1],"->>>>>>>9.99<<") .
                            WHEN "msf"      THEN cVarValue = string(v-msf[1],"->>>>>>9.9999") .
                            WHEN "ton"      THEN cVarValue = string(v-ton[1],"->>>>>>9.9999") .
                            WHEN "cst"      THEN cVarValue = "" .
                            WHEN "val"      THEN cVarValue = string(v-val[1],"->,>>>,>>>,>>9.99")    .
                       END CASE.

                       cExcelVarValue = cVarValue.
                       cDisplay = cDisplay + cVarValue +
                                  FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                       cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
               END.

               PUT UNFORMATTED  "       DATE TOTALS" substring(cDisplay,19,300) SKIP.
           end.

           if not last-of(rm-rcpth.i-no) then put skip(1).

           assign
            v-qty[2] = v-qty[2] + v-qty[1]
            v-val[2] = v-val[2] + v-val[1]
            v-msf[2] = v-msf[2] + v-msf[1]
            v-ton[2] = v-ton[2] + v-ton[1]

            v-qty[1] = 0
            v-val[1] = 0
            v-msf[1] = 0
            v-ton[1] = 0.
         end.

         if last-of(rm-rcpth.i-no) then do:
           if not v-first[2] then do:
             /*underline rm-rdtlh.qty
                       v-msf[4]
                       v-ton[4]
                       v-value
                 with frame itemx.*/

            /* display " ITEM TOTALS" @ rm-rcpth.i-name
                     v-qty[2]       @ rm-rdtlh.qty
                     v-msf[2]       @ v-msf[4]
                     v-ton[2]       @ v-ton[4]
                     v-val[2]       @ v-value
                 with frame itemx.*/
               PUT SKIP str-line SKIP .
               ASSIGN cDisplay = ""
                      cTmpField = ""
                      cVarValue = ""
                      cExcelDisplay = ""
                      cExcelVarValue = "".

               DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                  cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                       CASE cTmpField:             
                            WHEN "trn-typ"  THEN cVarValue = "" .
                            WHEN "i-no"     THEN cVarValue = "" .
                            WHEN "dscr"     THEN cVarValue = "" .
                            WHEN "dat"      THEN cVarValue = "" .
                            WHEN "po"       THEN cVarValue = "" .
                            WHEN "vend"     THEN cVarValue = "" .
                            WHEN "job"      THEN cVarValue = "" .
                            WHEN "qty"      THEN cVarValue = STRING(v-qty[2],"->>>>>>>9.99<<") .
                            WHEN "msf"      THEN cVarValue = string(v-msf[2],"->>>>>>9.9999") .
                            WHEN "ton"      THEN cVarValue = string(v-ton[2],"->>>>>>9.9999") .
                            WHEN "cst"      THEN cVarValue = "" .
                            WHEN "val"      THEN cVarValue = string(v-val[2],"->,>>>,>>>,>>9.99")    .
                       END CASE.

                       cExcelVarValue = cVarValue.
                       cDisplay = cDisplay + cVarValue +
                                  FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                       cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
               END.

               PUT UNFORMATTED  "       ITEM TOTALS" substring(cDisplay,19,300) SKIP(1).
           end.

           put skip(2).

           assign
            v-qty[3] = v-qty[3] + v-qty[2]
            v-val[3] = v-val[3] + v-val[2]
            v-msf[3] = v-msf[3] + v-msf[2]
            v-ton[3] = v-ton[3] + v-ton[2]

            v-qty[2] = 0
            v-val[2] = 0
            v-msf[2] = 0
            v-ton[2] = 0.
         end.

         v-first[1] = no.
         if last-of(rm-rcpth.trans-date) then v-first[2] = no.

         if last-of(rm-rcpth.rita-code) then do:
          /* underline rm-rdtlh.qty
                     v-msf[4]
                     v-ton[4]
                     v-value
               with frame itemx.

           display " TYPE TOTALS" @ rm-rcpth.i-name
                   v-qty[3]       @ rm-rdtlh.qty
                   v-msf[3]       @ v-msf[4]
                   v-ton[3]       @ v-ton[4]      
                   v-val[3]       @ v-value
                 with frame itemx.*/

             PUT SKIP str-line SKIP .
             ASSIGN cDisplay = ""
                      cTmpField = ""
                      cVarValue = ""
                      cExcelDisplay = ""
                      cExcelVarValue = "".

               DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                  cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                       CASE cTmpField:             
                            WHEN "trn-typ"  THEN cVarValue = "" .
                            WHEN "i-no"     THEN cVarValue = "" .
                            WHEN "dscr"     THEN cVarValue = "" .
                            WHEN "dat"      THEN cVarValue = "" .
                            WHEN "po"       THEN cVarValue = "" .
                            WHEN "vend"     THEN cVarValue = "" .
                            WHEN "job"      THEN cVarValue = "" .
                            WHEN "qty"      THEN cVarValue = STRING(v-qty[3],"->>>>>>>9.99<<") .
                            WHEN "msf"      THEN cVarValue = string(v-msf[3],"->>>>>>9.9999") .
                            WHEN "ton"      THEN cVarValue = string(v-ton[3],"->>>>>>9.9999") .
                            WHEN "cst"      THEN cVarValue = "" .
                            WHEN "val"      THEN cVarValue = string(v-val[3],"->,>>>,>>>,>>9.99")    .
                       END CASE.

                       cExcelVarValue = cVarValue.
                       cDisplay = cDisplay + cVarValue +
                                  FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                       cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
               END.

               PUT UNFORMATTED  "       TYPE TOTALS" substring(cDisplay,19,300) SKIP(1).

           assign
            v-qty[3] = 0
            v-val[3] = 0
            v-msf[3] = 0
            v-ton[3] = 0.
         end.

         if v-code ne "T" then do:
          find first costtype
              where costtype.company   eq cocode
                and costtype.loc       eq rm-rdtlh.loc
                and costtype.cost-type eq item.cost-type
              no-lock no-error.

          if v-code eq "R" then
          do:
             create tt-report.
             assign
               tt-report.term-id = ""
               tt-report.key-01  = if avail costtype then costtype.inv-asset
                                   else "Cost Type not found"
               tt-report.key-02  = string(v-value,"->>,>>>,>>9.99").
          end.

          else
          do:
            create tt-report.
             assign
               tt-report.term-id = ""
               tt-report.key-01  = if avail costtype then costtype.cons-exp
                                   else "Cost Type not found"
               tt-report.key-02  = string(v-value,"->>,>>>,>>9.99").    
          end.
      END.
END.

hide frame r-top2.

v-value = 0.
PUT SKIP(2) .
for each tt-report where tt-report.term-id eq "",
     first account
     where account.company eq cocode
       and account.actnum  eq tt-report.key-01
     no-lock
     break by tt-report.key-01
     transaction:

   if first(tt-report.key-01) then page.

   hide frame r-top.
   v-value = v-value + dec(tt-report.key-02).

   if last-of(tt-report.key-01) then do:
     display account.actnum
             account.dscr
             v-value  label "Amount" (total)      format "->>,>>>,>>9.99"
         with STREAM-IO width 132.

     v-value = 0.
   end.

   delete tt-report.
end.

IF v-export THEN DO:
   OUTPUT STREAM s-excel close.
   IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(v-exp-name)).
END.



RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).




SESSION:SET-WAIT-STATE (""). 

OUTPUT CLOSE.   
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
