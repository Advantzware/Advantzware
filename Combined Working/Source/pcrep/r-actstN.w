&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: pcrep\r-actest.w

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

   def TEMP-TABLE work-rep NO-UNDO
   field job like job-mch.job
   field job-no like job-mch.job-no
   field job-no2 like job-mch.job-no2
   field form-no like job-mch.frm
   field blank-no like job-mch.blank-no
   field pass like job-mch.pass
   field dept like mch-act.code
   field chrg like work-rep.dept
   field m-code like mach.m-code
   field r-std-hrs as dec format '>>>>>>9.99'
   field r-act-hrs as dec format '>>>>>>9.99'
   field m-std-hrs as dec format '>>>>>>9.99'
   field m-act-hrs as dec format '>>>>>>9.99'
   field dt-chg-hrs as dec format '>>>>>>9.99'
   field dt-nochg-hrs as dec format '>>>>>>9.99'
   FIELD qty-prod LIKE mch-srt.qty-prod
   FIELD run-qty LIKE job-mch.run-qty.

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

ASSIGN cTextListToSelect = "Dept,Mach,Job#,S,B,Run Hrs,MR Hrs," +
                           "Job DT,NC DT,Tot Hrs," +
                           "Std Run,Std MR,Tot Std Hrs," +
                           "Variance,Estimate,Cailper,Run Qty,Order Qty,Item Name"
           cFieldListToSelect = "dept,mach-code,job,S,B,act-run,act-m-hrs," +
                           "dntme-chrg,dntme-no-chrg,ttl-act-hrs," +
                           "est-run-hrs,est-mr-hrs,ttl-est-hrs," +
                           "labr-varnc,est-no,clpr,run-qty,ord-qty,itm-nam"
           cFieldLength = "4,6,9,2,2,11,11," + "11,10,11," + "11,11,11," + "12,8,7,11,13,15"
           cFieldType = "c,c,c,c,c,i,i," + "i,i,i," + "i,i,i," + "i,c,i,i,i,c"
           .

{sys/inc/ttRptSel.i}

ASSIGN cTextListToDefault  = "Dept,Mach,Job#,S,B,Run Hrs,MR Hrs," +
                           "Job DT,NC DT,Tot Hrs," +
                           "Std Run,Std MR,Tot Std Hrs," +
                           "Variance" .


{sys/inc/oereordr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_date end_date begin_dept ~
end_dept begin_mach end_mach as-of-date tb_JOB rd-dest lv-ornt ~
lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok ~
btn-cancel Btn_Def sl_avail sl_selected Btn_Add Btn_Remove btn_Up btn_down
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date begin_dept end_dept ~
begin_mach end_mach as-of-date tb_JOB rd-dest lv-ornt lines-per-page ~
lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file sl_avail sl_selected

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

DEFINE VARIABLE as-of-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "For Jobs Closed After" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_dept AS CHARACTER FORMAT "X(4)" 
     LABEL "Beginning Department" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_mach AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_dept AS CHARACTER FORMAT "X(4)" INITIAL "zzzz" 
     LABEL "Ending Department#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_mach AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-actest.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

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
     SIZE 20 BY 6.67 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 8.95.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.33.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33.8 BY 5.71 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.71 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_JOB AS LOGICAL INITIAL yes 
     LABEL "Show Job Order Detail?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_date AT ROW 2.67 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 2.67 COL 70 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_dept AT ROW 3.86 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Department Number"
     end_dept AT ROW 3.86 COL 70 COLON-ALIGNED HELP
          "Enter Ending Department Number"
     begin_mach AT ROW 5.05 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Machine"
     end_mach AT ROW 5.05 COL 70 COLON-ALIGNED HELP
          "Enter Ending Machine"
     as-of-date AT ROW 6.95 COL 40 COLON-ALIGNED HELP
          "Jobs Closed after"
     tb_JOB AT ROW 8.14 COL 42
     sl_avail AT ROW 10.14 COL 3 NO-LABEL WIDGET-ID 26
     sl_selected AT ROW 10.14 COL 61 NO-LABEL WIDGET-ID 28
     Btn_Def AT ROW 10.33 COL 40.6 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     Btn_Add AT ROW 11.38 COL 40.6 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 12.43 COL 40.6 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 13.52 COL 40.6 WIDGET-ID 40
     btn_down AT ROW 14.62 COL 40.6 WIDGET-ID 42
     rd-dest AT ROW 17.38 COL 5 NO-LABEL
     lv-ornt AT ROW 17.38 COL 31 NO-LABEL
     lines-per-page AT ROW 17.38 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 19.29 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 20.24 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 22.14 COL 25
     tb_excel AT ROW 22.38 COL 68 RIGHT-ALIGNED
     tb_runExcel AT ROW 22.38 COL 92 RIGHT-ALIGNED
     fi_file AT ROW 23.48 COL 46 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 25.19 COL 19
     btn-cancel AT ROW 25.19 COL 57
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 9.38 COL 3.2 WIDGET-ID 38
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 16.43 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 9.38 COL 61 WIDGET-ID 44
     RECT-6 AT ROW 16 COL 2
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 25.86.

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
         TITLE              = "Labor Hours vs Estimated Report"
         HEIGHT             = 26.1
         WIDTH              = 95.6
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
                                                                        */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       as-of-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_dept:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_dept:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
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

ASSIGN 
       tb_JOB:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Labor Hours vs Estimated Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Labor Hours vs Estimated Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME as-of-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL as-of-date C-Win
ON LEAVE OF as-of-date IN FRAME FRAME-A /* For Jobs Closed After */
DO:
  assign {&self-name}.
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


&Scoped-define SELF-NAME begin_dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_dept C-Win
ON LEAVE OF begin_dept IN FRAME FRAME-A /* Beginning Department */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_mach C-Win
ON LEAVE OF begin_mach IN FRAME FRAME-A /* Beginning Machine */
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
  run run-report. 

  STATUS DEFAULT "Processing Complete". 
  SESSION:SET-WAIT-STATE(""). 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type= ''
                            &begin_cust=begin_mach
                            &END_cust= begin_mach
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = ''
                             &begin_cust= begin_mach
                             &END_cust=begin_mach
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = ''
                                  &begin_cust= begin_mach
                                  &END_cust=begin_mach
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


&Scoped-define SELF-NAME end_dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_dept C-Win
ON LEAVE OF end_dept IN FRAME FRAME-A /* Ending Department# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_mach C-Win
ON LEAVE OF end_mach IN FRAME FRAME-A /* Ending Machine */
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


&Scoped-define SELF-NAME tb_JOB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_JOB C-Win
ON VALUE-CHANGED OF tb_JOB IN FRAME FRAME-A /* Show Job Order Detail? */
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

  ASSIGN
   as-of-date =  date(month(TODAY), 1, year(TODAY)).

  RUN DisplaySelectionList.
  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}

    RUN DisplaySelectionList2.
    APPLY "entry" TO begin_date.
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
  DISPLAY begin_date end_date begin_dept end_dept begin_mach end_mach as-of-date 
          tb_JOB rd-dest lv-ornt lines-per-page lv-font-no lv-font-name 
          td-show-parm tb_excel tb_runExcel fi_file sl_avail sl_selected
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_date end_date begin_dept end_dept begin_mach 
         end_mach as-of-date tb_JOB rd-dest lv-ornt lines-per-page lv-font-no 
         td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
         Btn_Def sl_avail sl_selected Btn_Add Btn_Remove btn_Up btn_down 
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
/* ------------------------------------------------ pc/rep/lab-est.p 8/94 gb */
/* Labor vs Estimated Hours Report                                            */
/* -------------------------------------------------------------------------- */

/*{sys/form/r-topw.f}*/


def var v-dept as ch format 'x(4)' extent 2 initial ["","zzzz"].
def var v-mach like mach.m-code extent 2 initial ["","zzzzzz"].
def var show-det as log format 'Y/N' no-undo.
def var mr-hr like job-mch.mr-hr no-undo.
def var run-hr like job-mch.run-hr no-undo.
def var cls-dte as date format "99/99/9999" no-undo.
def var m-dt-nochg-hrs as dec format '>>>>>>9.99'no-undo.
def var m-dt-chg-hrs as dec format '>>>>>>9.99' no-undo.
def var m-m-act-hrs as dec format '>>>>>>9.99' no-undo.
def var m-m-std-hrs as dec format '>>>>>>9.99' no-undo.
def var m-r-act-hrs as dec format '>>>>>>9.99' no-undo.
def var m-r-std-hrs as dec format '>>>>>>9.99' no-undo.
def var d-dt-nochg-hrs as dec format '>>>>>>9.99'no-undo.
def var d-dt-chg-hrs as dec format '>>>>>>9.99' no-undo.
def var d-m-act-hrs as dec format '>>>>>>9.99' no-undo.
def var d-m-std-hrs as dec format '>>>>>>9.99' no-undo.
def var d-r-act-hrs as dec format '>>>>>>9.99' no-undo.
def var d-r-std-hrs as dec format '>>>>>>9.99' no-undo.

DEF VAR vlabr-varnc AS DEC NO-UNDO.
DEF VAR vttl-act-hrs AS DEC NO-UNDO.

def var hdr-tit as char no-undo.
def var hdr-tit2 as char no-undo.
def var hdr-tit3 as char no-undo.

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
DEF VAR v-page AS LOGICAL NO-UNDO.

{sys/form/r-top5DL3.f}
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.

/*FORM HEADER
     hdr-tit format "x(132)" skip
     hdr-tit2 format "x(132)" skip
     hdr-tit3 format "x(132)" 

    WITH FRAME r-top. */

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 v-dept[1]   = begin_dept
 v-dept[2]   = end_dept
 v-mach[1]   = begin_mach
 v-mach[2]   = end_mach
 cls-dte     = as-of-date
 show-det    = tb_job

 /* hdr-tit  = "       MACH                        ACTUAL     ACTUAL  " +
             " DOWNTIME   DOWNTIME      TOTAL   ESTIMATE   ESTIMATE   " +
             "   TOTAL      LABOR"
  hdr-tit2 = "DEPT CODE        JOB #    S/ B    RUN HRS     MR HRS  " +
             " CHARGED    NOCHARGE  ACT HOURS    RUN HRS   MR HOURS  E" +
             "ST HOURS   VARIANCE"
  hdr-tit3 = fill("-", 130) */ . 



SESSION:SET-WAIT-STATE("general").

IF tb_excel THEN DO:
  /* OUTPUT STREAM excel TO VALUE(fi_file).
   EXPORT STREAM excel DELIMITER ","
       "Dept"
       "Mach.Code"
       "Job#"
       "S"
       "B"
       "Actual Run Hrs"
       "Actual MR Hrs"
       "Downtime Charged"
       "Downtime No Charged"
       "Total Act Hours"
       "Estimate Run Hrs"
       "Estimate MR Hours"
       "Total Est Hours"
       "Labor Variance"
       SKIP. */
END. 

 assign m-r-act-hrs = 0
        m-m-act-hrs  = 0
        m-dt-chg-hrs = 0
        m-dt-nochg-hrs = 0
        m-r-std-hrs    = 0
        m-m-std-hrs    = 0
        m-r-act-hrs    = 0
        d-m-act-hrs    = 0
        d-dt-chg-hrs   = 0
        d-dt-nochg-hrs = 0
        d-r-std-hrs    = 0
        d-m-std-hrs    = 0.


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

        IF LOOKUP(ttRptSelected.TextList, "Run Hrs,MR Hrs,Job DT,NC DT,Tot Hrs,Std Run,Std MR,Tot Std Hrs,Variance") <> 0    THEN
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

display "" with frame r-top.

 for each job
       where job.company    eq cocode
         and job.opened     eq no
         and job.close-date ge cls-dte
       no-lock:


     for each mch-act
         where mch-act.company eq cocode
           and mch-act.job     eq job.job
           and mch-act.m-code  ge v-mach[1]
           and mch-act.m-code  le v-mach[2]
           and mch-act.dept    ge v-dept[1]
           and mch-act.dept    le v-dept[2]
           AND mch-act.op-date GE begin_date
           AND mch-act.op-date LE end_date
         no-lock:


       {pc/rep/lab-est.i dept mch-act.dept}
     end.

     for each work-rep:
       /*find first job-mch
           where job-mch.company  eq cocode
             and job-mch.job      eq work-rep.job
             and job-mch.frm      eq work-rep.form-no
             and job-mch.blank-no eq work-rep.blank-no
             and job-mch.pass     eq work-rep.pass
           no-lock no-error.*/

         {custom/statusMsg.i "'Processing Department # ' + string(work-rep.dept)"}

       find first job-mch where job-mch.company  = cocode and
                                job-mch.job      eq work-rep.job and
                                job-mch.job-no  eq work-rep.job-no and
                                job-mch.job-no2 eq work-rep.job-no2 and
                                job-mch.frm      = work-rep.form-no and
                                (job-mch.blank-no = work-rep.blank-no or
                                 work-rep.blank-no = 0) and
                                job-mch.m-code   = work-rep.m-code and
                                job-mch.pass     = work-rep.pass
                                no-lock no-error.
       if not avail job-mch then
       find first job-mch where job-mch.company eq cocode and
                                job-mch.job      eq work-rep.job and
                                job-mch.job-no  eq work-rep.job-no and
                                job-mch.job-no2 eq work-rep.job-no2 and
                                job-mch.frm      eq work-rep.form-no and
                                (job-mch.blank-no = work-rep.blank-no or
                                 work-rep.blank-no = 0) and
                                job-mch.m-code   eq work-rep.m-code
                                no-lock no-error.
       if not avail job-mch then
       find first job-mch where job-mch.company eq cocode and
                                job-mch.job     eq work-rep.job and
                                job-mch.job-no  eq work-rep.job-no and
                                job-mch.job-no2 eq work-rep.job-no2 and
                                job-mch.frm     eq work-rep.form-no and
                                job-mch.m-code  eq work-rep.m-code and
                                job-mch.speed   ne 0
                                no-lock no-error.
       if not avail job-mch then
       find first job-mch where job-mch.company eq cocode and
                                job-mch.job     eq work-rep.job and
                                job-mch.job-no  eq work-rep.job-no and
                                job-mch.job-no2 eq work-rep.job-no2 and
                                job-mch.frm     eq work-rep.form-no and
                                job-mch.m-code  eq work-rep.m-code
                                no-lock no-error.
       if available job-mch then
          assign work-rep.r-std-hrs = IF work-rep.qty-prod NE 0 THEN
                                        work-rep.qty-prod / job-mch.speed
                                      ELSE job-mch.run-hr
                 work-rep.m-std-hrs = job-mch.mr-hr .
        IF AVAIL job-mch THEN
            work-rep.run-qty = job-mch.run-qty .
       /*if avail job-mch then
         assign
          work-rep.r-std-hrs = job-mch.run-hr
          work-rep.m-std-hrs = job-mch.mr-hr.*/



     end.
   end.

 for each work-rep break by work-rep.dept
                              by work-rep.m-code:

     {custom/statusMsg.i "'Processing Department # ' + string(work-rep.dept)"}

     FIND FIRST job-hdr WHERE job-hdr.company EQ cocode
            AND job-hdr.job     EQ work-rep.job
            AND job-hdr.job-no  EQ work-rep.job-no
            AND job-hdr.job-no2 EQ work-rep.job-no2 
          /*  AND job-hdr.frm EQ work-rep.form-no */
            /*AND job-hdr.blank-no EQ work-rep.blank-no*/
            NO-LOCK NO-ERROR .

        IF AVAILABLE job-hdr THEN DO:
            FIND FIRST ef WHERE ef.company EQ cocode
                AND ef.est-no EQ job-hdr.est-no 
                AND ef.form-no EQ work-rep.form-no  NO-LOCK NO-ERROR. 

            FIND FIRST itemfg WHERE itemfg.company EQ cocode
                AND itemfg.i-no EQ job-hdr.i-no NO-LOCK NO-ERROR. 

            FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ cocode
                AND oe-ordl.i-no EQ job-hdr.i-no
                AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
        END. /* avail job-hdr */ 

         if show-det THEN do:
           /* display work-rep.dept
                    space(2)
                    work-rep.m-code
                    work-rep.job-no space(0) "-" space(0)
                    work-rep.job-no2 format "99"
                    work-rep.form-no space(0) "/" space(0)
                    work-rep.blank-no format "99"
                    work-rep.r-act-hrs
                    work-rep.m-act-hrs
                    work-rep.dt-chg-hrs
                    work-rep.dt-nochg-hrs
                    (work-rep.r-act-hrs + work-rep.m-act-hrs +
                    work-rep.dt-chg-hrs + work-rep.dt-nochg-hrs)
                    work-rep.r-std-hrs
                    work-rep.m-std-hrs
                    (work-rep.r-std-hrs + work-rep.m-std-hrs)
                    ((work-rep.r-act-hrs + work-rep.m-act-hrs +
                    work-rep.dt-chg-hrs + work-rep.dt-nochg-hrs) -
                    (work-rep.r-std-hrs + work-rep.m-std-hrs))
                    format ">>>>>>9.99-"
                 with frame det width 132 no-box no-labels STREAM-IO down.

IF tb_excel THEN 
   EXPORT STREAM excel DELIMITER ","


          ()
          STRING()
          STRING(work-rep.blank-no)
          work-rep.r-act-hrs
          work-rep.m-act-hrs
          work-rep.dt-chg-hrs
          work-rep.dt-nochg-hrs
          (work-rep.r-act-hrs + work-rep.m-act-hrs + work-rep.dt-chg-hrs + work-rep.dt-nochg-hrs)
          work-rep.r-std-hrs
          work-rep.m-std-hrs
          (work-rep.r-std-hrs + work-rep.m-std-hrs)
          ((work-rep.r-act-hrs + work-rep.m-act-hrs + work-rep.dt-chg-hrs + work-rep.dt-nochg-hrs) -
          (work-rep.r-std-hrs + work-rep.m-std-hrs)) */

          ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = ""
           vlabr-varnc = ((work-rep.r-act-hrs + work-rep.m-act-hrs + work-rep.dt-chg-hrs + work-rep.dt-nochg-hrs) - (work-rep.r-std-hrs + work-rep.m-std-hrs)).
           vttl-act-hrs = (work-rep.r-std-hrs + work-rep.m-std-hrs) .
           IF work-rep.r-std-hrs EQ ? THEN ASSIGN work-rep.r-std-hrs = 0 .

    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).

            CASE cTmpField:               
                 WHEN "dept" THEN cVarValue = STRING(work-rep.dept,"x(4)").
                 WHEN "mach-code" THEN cVarValue = string(work-rep.m-code,"x(6)").
                 WHEN "job" THEN cVarValue = STRING(work-rep.job-no + "-" + STRING(work-rep.job-no2)).
                 WHEN "s" THEN cVarValue = string(work-rep.form-no).
                 WHEN "b" THEN cVarValue = string(work-rep.blank-no).
                 WHEN "act-run" THEN cVarValue = string(work-rep.r-act-hrs,"->>>>>>9.99").
                 WHEN "act-m-hrs" THEN cVarValue = string(work-rep.m-act-hrs,"->>>>>>9.99").
                 WHEN "dntme-chrg" THEN cVarValue = string(work-rep.dt-chg-hrs,"->>>>>>9.99").
                 WHEN "dntme-no-chrg" THEN cVarValue = string(work-rep.dt-nochg-hrs,">>>>>>9.99").
                 WHEN "ttl-act-hrs" THEN cVarValue = string(work-rep.r-act-hrs + work-rep.m-act-hrs + work-rep.dt-chg-hrs + work-rep.dt-nochg-hrs,"->>>>>>9.99").
                 WHEN "est-run-hrs" THEN cVarValue = if work-rep.r-std-hrs ne ? then string(work-rep.r-std-hrs,"->>>>>>9.99") else "".
                 WHEN "est-mr-hrs" THEN cVarValue = string(work-rep.m-std-hrs,"->>>>>>9.99").                        
                 WHEN "ttl-est-hrs" THEN cVarValue = IF vttl-act-hrs NE ? THEN string(vttl-act-hrs,"->>>>>>9.99") ELSE "". 
                 WHEN "labr-varnc" THEN cVarValue = IF vlabr-varnc NE ? THEN string(vlabr-varnc,"->>>>>>>9.99") ELSE "". 
                 WHEN "run-qty" THEN cVarValue = IF work-rep.run-qty NE ? THEN string(work-rep.run-qty,">,>>>,>>9.9<<") ELSE " " . 
                 WHEN "est-no" THEN cVarValue = IF AVAIL job-hdr THEN string(job-hdr.est-no,"x(8)") ELSE " ". 
                 WHEN "clpr" THEN cVarValue = IF AVAIL ef THEN string(ef.cal,"9.99999") ELSE " ".
                 WHEN "ord-qty" THEN cVarValue = IF AVAIL oe-ordl THEN string(oe-ordl.qty,"->>,>>>,>>9.9<<") ELSE " ".
                 WHEN "itm-nam" THEN cVarValue = IF AVAIL itemfg THEN string(itemfg.i-name,"x(15)") ELSE " ".                                                                                                        
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
         RELEASE ef.

         assign m-r-act-hrs    = m-r-act-hrs + work-rep.r-act-hrs
                m-m-act-hrs    = m-m-act-hrs + work-rep.m-act-hrs
                m-dt-chg-hrs   = m-dt-chg-hrs + work-rep.dt-chg-hrs
                m-dt-nochg-hrs = m-dt-nochg-hrs + work-rep.dt-nochg-hrs
                m-r-std-hrs    = m-r-std-hrs + work-rep.r-std-hrs
                m-m-std-hrs    = m-m-std-hrs + work-rep.m-std-hrs
                d-r-act-hrs    = d-r-act-hrs + work-rep.r-act-hrs
                d-m-act-hrs    = d-m-act-hrs + work-rep.m-act-hrs
                d-dt-chg-hrs   = d-dt-chg-hrs + work-rep.dt-chg-hrs
                d-dt-nochg-hrs = d-dt-nochg-hrs + work-rep.dt-nochg-hrs
                d-r-std-hrs    = d-r-std-hrs + work-rep.r-std-hrs
                d-m-std-hrs    = d-m-std-hrs + work-rep.m-std-hrs.

         {pc/rep/lab-estN.i "m-code" "dept" "m-" "d-"}

      end. /* each item */

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

EMPTY TEMP-TABLE work-rep.

  IF tb_excel THEN DO:
     OUTPUT STREAM excel CLOSE.
     IF tb_runExcel THEN
         OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
  END.

SESSION:SET-WAIT-STATE("").

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
  def var lv-label as cha NO-UNDO.

  ASSIGN
  lv-frame-hdl = frame {&frame-name}:HANDLE
  lv-group-hdl = lv-frame-hdl:first-child
  lv-field-hdl = lv-group-hdl:first-child.

  do while true:
     if not valid-handle(lv-field-hdl) then leave.
     if lookup(lv-field-hdl:private-data,"parm") > 0
        then do:
           if lv-field-hdl:label <> ? then 
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:label + ",".
           else do:  /* radio set */
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     lv-field2-hdl = lv-group-hdl:first-child.
              repeat:
                  if not valid-handle(lv-field2-hdl) then leave. 
                  if lv-field2-hdl:private-data = lv-field-hdl:name THEN
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:screen-value + ",".

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

