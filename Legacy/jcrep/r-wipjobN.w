&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: jcrep\r-backlo.w

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

DEF VAR v-print-fmt AS CHARACTER.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

DEFINE STREAM excel.

DEFINE TEMP-TABLE tt-job-mch NO-UNDO
   FIELD job-no AS CHAR
   FIELD job-no2 AS INT
   FIELD m-code AS CHAR
   FIELD cust-no AS CHAR
   FIELD die AS CHAR
   FIELD style AS CHAR
   FIELD total-hrs AS DEC
   FIELD total-qty AS INT
   FIELD cst-act AS DEC format '>>>>>>9.99'
   FIELD cst-std AS DEC format '>>>>>>9.99'
   INDEX job job-no job-no2.

define TEMP-TABLE mch NO-UNDO
   FIELD job-no AS CHAR
   FIELD job-no2 AS INT
   FIELD est-no AS CHAR
   field line like job-mch.line
   field form-no like mch-act.frm
   field blank-no like mch-act.blank-no
   field m-code like mch-act.m-code
   field i-no   like job-mch.i-no
   field run-hr like job-mch.run-hr
   field run-act like job-mch.run-hr
   field run-var as dec format '->>9.99'
   field mr-hr like job-mch.mr-hr
   field mr-act like job-mch.mr-hr
   field mr-var as dec format '->>9.99'
   FIELD cst-act AS DEC format '>>>>>>9.99'
   FIELD cst-std AS DEC format '>>>>>>9.99'
   field act-qty as DEC
   INDEX job job-no job-no2.

define TEMP-TABLE x-mch NO-UNDO
   FIELD job-no AS CHAR
   FIELD job-no2 AS INT
   FIELD est-no AS CHAR
   field line like job-mch.line
   field form-no like mch-act.frm
   field blank-no like mch-act.blank-no
   field m-code like mch-act.m-code
   field i-no   like job-mch.i-no
   field run-hr like job-mch.run-hr
   field run-act like job-mch.run-hr
   field run-var as dec format '->>9.99'
   field mr-hr like job-mch.mr-hr
   field mr-act like job-mch.mr-hr
   field mr-var as dec format '->>9.99'
   field est-speed like job-mch.speed
   field act-qty as DEC
   FIELD cst-act AS DEC
   FIELD cst-std AS DEC
   INDEX job job-no job-no2.


def TEMP-TABLE tt-mat
   FIELD job-no AS CHAR
   FIELD job-no2 AS INT
   FIELD mat-seq AS INT
   field form-no like job-mat.frm
   field blank-no like job-mat.blank-no
   field rm-i-no like job-mat.rm-i-no
   field updatable as log init no
   field qty-std as decimal format '>>>>>9.9'
   field qty-act as decimal format '>>>>>9.9'
   field qty-var as decimal format '->>>>>9.9'
   field cst-std as decimal format '>>>>>>9.99'
   field cst-act as decimal format '>>>>>>9.99'
   field cst-var as decimal format '->>>>>>9.99'
   field basis-w as dec
   field len as dec
   field wid as dec
   field cst-uom like job-mat.sc-uom
   INDEX tt-mat form-no blank-no rm-i-no.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF BUFFER bf-item FOR ITEM.
DEF VAR cTextListToDefault AS cha NO-UNDO.

ASSIGN cTextListToSelect = "Machine,JOB#,Customer,Die#,Style," +
                           "RM Item,Caliper,Vendor,Weight,Sht Wid,Sht Len," +
                           "Sheets,MSF,Tons,MLF,Cost/Ton,Total Value,Total Hours"
       cFieldListToSelect = "v-machine,v-job-no,v-cust,v-die,v-style,v-rm-item,item.cal,v-vendor,v-wgt,v-shtWid,v-shtLen," + 
                            "v-sheets,v-msf,v-ton,v-mlf,v-costTon,v-total,v-total-mach-hrs"
       cFieldLength = "8,9,8,15,6," + "15,7,20,10,10,10," + "12,15,15,15,15,15,15"
       cFieldType   = "c,c,c,c,c," + "c,i,c,i,i,i," + "i,i,i,i,i,i,i"
       .


DO TRANSACTION:
   {sys/inc/ttRptSel.i}

ASSIGN cTextListToDefault  = "JOB#,Machine,Customer,Vendor," +
                           "RM Item,Caliper,Weight,Style,Sheets,Sht Wid,Sht Len," +
                           "MSF,Tons,MLF,Cost/Ton,Total Value,Total Hours" .

   {sys/inc/tspost.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_dept end_dept begin_mach ~
end_mach sl_avail sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest ~
lv-ornt lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file ~
btn-ok btn-cancel Btn_Def
&Scoped-Define DISPLAYED-OBJECTS begin_dept end_dept begin_mach end_mach ~
sl_avail sl_selected rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
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

DEFINE VARIABLE begin_dept AS CHARACTER FORMAT "X(4)" 
     LABEL "Beginning Department" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_mach AS CHARACTER FORMAT "X(6)" 
     LABEL "Beginning Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_dept AS CHARACTER FORMAT "X(4)" INITIAL "zzzz" 
     LABEL "Ending Department" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_mach AS CHARACTER FORMAT "X(6)" INITIAL "zzzzzz" 
     LABEL "Ending Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-wipjob.csv" 
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
     SIZE 95 BY 8.81.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 5.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 34 BY 6.43 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 35 BY 6.43 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_dept AT ROW 2.67 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Department"
     end_dept AT ROW 2.67 COL 68 COLON-ALIGNED HELP
          "Enter Ending Department"
     begin_mach AT ROW 3.62 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Machine"
     end_mach AT ROW 3.62 COL 68 COLON-ALIGNED HELP
          "Enter Ending Machine"
     sl_avail AT ROW 7.19 COL 3 NO-LABEL WIDGET-ID 26
     sl_selected AT ROW 7.19 COL 61.4 NO-LABEL WIDGET-ID 28
     Btn_Def AT ROW 7.43 COL 41 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     Btn_Add AT ROW 8.62 COL 41 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 9.81 COL 41 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 11 COL 41 WIDGET-ID 40
     btn_down AT ROW 12.19 COL 41 WIDGET-ID 42
     rd-dest AT ROW 16 COL 5 NO-LABEL
     lv-ornt AT ROW 16.71 COL 30 NO-LABEL
     lines-per-page AT ROW 16.71 COL 83 COLON-ALIGNED
     lv-font-no AT ROW 18.38 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 19.33 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 20.52 COL 30
     tb_excel AT ROW 21.48 COL 69.2 RIGHT-ALIGNED
     tb_runExcel AT ROW 21.48 COL 93 RIGHT-ALIGNED
     fi_file AT ROW 22.57 COL 47 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 24.57 COL 23
     btn-cancel AT ROW 24.57 COL 61
     "Available Columns" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 6.24 COL 3 WIDGET-ID 38
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 6.24 COL 60 WIDGET-ID 44
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 15.29 COL 2
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 15.05 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 98.8 BY 25.52.


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
         TITLE              = "WIP Value of Sheeted Inventory"
         HEIGHT             = 26
         WIDTH              = 99.6
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
       begin_dept:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* WIP Value of Sheeted Inventory */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* WIP Value of Sheeted Inventory */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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

  RUN GetSelectionList.
  run run-report. 
  STATUS DEFAULT "Processing Complete".

  SESSION:SET-WAIT-STATE ("").

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


&Scoped-define SELF-NAME end_dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_dept C-Win
ON LEAVE OF end_dept IN FRAME FRAME-A /* Ending Department */
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
    APPLY "entry" TO begin_dept.
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
  DISPLAY begin_dept end_dept begin_mach end_mach sl_avail sl_selected rd-dest 
          lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_dept end_dept begin_mach end_mach sl_avail 
         sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest lv-ornt 
         lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file 
         btn-ok btn-cancel Btn_Def
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
/*{sys/form/r-topw.f}*/
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

DEF VAR v-wgt AS DEC NO-UNDO.
DEF VAR v-shtWid AS DEC NO-UNDO.
DEF VAR v-shtLen AS DEC NO-UNDO.
DEF VAR v-sheets AS DEC NO-UNDO.
DEF VAR v-msf AS DEC NO-UNDO.
DEF VAR v-ton AS DEC NO-UNDO.
DEF VAR v-mlf AS DEC NO-UNDO.
DEF VAR v-costTon AS DEC NO-UNDO.
DEF VAR v-total AS DEC NO-UNDO.
DEF VAR v-lf-qty AS DEC NO-UNDO.
DEF VAR v-vendor AS cha NO-UNDO.
DEF VAR v-sheetTotal AS DEC NO-UNDO.
DEF VAR v-totalTot AS DEC NO-UNDO.
DEF VAR v-totalTon AS DEC NO-UNDO.
DEF VAR vCost AS DEC DECIMALS 4 NO-UNDO.
def var rate like mach.run-rate no-undo.
DEF VAR v-rm-item LIKE ITEM.i-no NO-UNDO.
{sys/form/r-top5DL3.f} 

cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

def var hdr-tit3 as char no-undo.
DEF VAR v-line AS CHAR NO-UNDO.
DEF VAR v-total-mach-hrs AS DEC NO-UNDO.
DEF VAR v-total-hrs AS DEC NO-UNDO.
DEF VAR v-cust AS CHAR NO-UNDO.
v-line = FILL("-",80).
DEF VAR excelheader AS CHARACTER  NO-UNDO.
DEF VAR v-pct AS INT NO-UNDO.
DEF VAR v-cost AS DEC NO-UNDO.
DEF VAR v-qty AS INT NO-UNDO.
DEF VAR li-seq AS INT NO-UNDO.
/*FORM HEADER
     hdr-tit3 format "x(142)"
    WITH FRAME r-top WIDTH 180.
*/

SESSION:SET-WAIT-STATE ("general").

/*IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).
   EXPORT STREAM excel DELIMITER ","
       "Machine"
       "Job#"
       "Cust#"
       "Die#"
       "Style"
       "Total Hours"
       SKIP.
END.
*/
assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}
 .
/* hdr-tit3 = fill("-", 142).*/


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
END.
{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).
   PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.  

display "" with frame r-top.

  EMPTY TEMP-TABLE tt-job-mch.
  EMPTY TEMP-TABLE mch.
  EMPTY TEMP-TABLE x-mch.
  EMPTY TEMP-TABLE tt-mat.

FOR EACH job /*FIELDS(job job-no job-no2 est-no)*/ WHERE
      job.company EQ cocode AND
      job.opened EQ YES
      NO-LOCK:

     {custom/statusMsg.i " 'Processing Job#  '  + job.job-no "}

      {jcrep/r-wipjobN2.i}  /* for tt-mat */

      FOR EACH job-mch WHERE
          job-mch.company EQ cocode and
          job-mch.job     EQ job.job AND
          job-mch.job-no  eq job.job-no and
          job-mch.job-no2 eq job.job-no2 AND
          job-mch.m-code  GE begin_mach AND
          job-mch.m-code  LE end_mach AND
          job-mch.dept    GE begin_dept AND
          job-mch.dept    LE end_dept AND
          job-mch.run-complete EQ NO
          USE-INDEX job
          NO-LOCK:

          create x-mch.
          assign x-mch.job-no   = job-mch.job-no
                 x-mch.job-no2  = job-mch.job-no2
                 x-mch.form-no  = job-mch.frm
                 x-mch.line     = job-mch.line
                 x-mch.blank-no = job-mch.blank-no
                 x-mch.m-code   = job-mch.m-code
                 x-mch.i-no     = job-mch.i-no
                 x-mch.est-speed = job-mch.speed
                 x-mch.est-no   = job.est-no.
          IF job-mch.j-no EQ 0 THEN
             ASSIGN
                x-mch.run-hr = job-mch.run-hr
                x-mch.mr-hr  = job-mch.mr-hr.

          RELEASE x-mch.
      END.

      for each mch-act where
          mch-act.company = cocode and
          mch-act.job = job.job AND
          mch-act.m-code GE begin_mach AND
          mch-act.m-code LE end_mach AND
          mch-act.dept   GE begin_dept AND
          mch-act.dept   LE end_dept AND
          CAN-FIND(FIRST job-mch WHERE
          job-mch.company EQ cocode and
          job-mch.job     EQ mch-act.job AND
          job-mch.job-no  eq mch-act.job-no and
          job-mch.job-no2 eq mch-act.job-no2 AND
          job-mch.m-code  EQ mch-act.m-code AND
          job-mch.frm     EQ mch-act.frm AND
          (job-mch.blank-no EQ mch-act.blank-no OR
          job-mch.blank-no EQ 0) AND
          job-mch.run-complete EQ NO)
          no-lock:


          find first x-mch where
               x-mch.job-no   = mch-act.job-no AND
               x-mch.job-no2  = mch-act.job-no2 AND
               x-mch.form-no  = mch-act.frm and
               x-mch.blank-no = mch-act.blank-no and
               x-mch.m-code   = mch-act.m-code
               no-error.
           if not available x-mch THEN do:
             create x-mch.
             assign x-mch.job-no   = mch-act.job-no
                    x-mch.job-no2  = mch-act.job-no2
                    x-mch.form-no  = mch-act.frm
                    x-mch.line     = x
                    x-mch.blank-no = mch-act.blank-no
                    x-mch.m-code   = mch-act.m-code
                    x-mch.i-no     = mch-act.i-no
                    x-mch.est-no   = job.est-no
                    x = x - 1.
           end.

           find FIRST job-code where
                job-code.code = mch-act.code
                no-lock no-error.

           if not available job-code then next.

           if job-code.cat = "MR" then
              x-mch.mr-act = x-mch.mr-act + mch-act.hours.
           else if job-code.cat = "RUN" THEN DO:

               rate = mch-act.fixoh + mch-act.varoh.
               IF v-tspost-val EQ "Actual" THEN
             DO i = 1 TO mch-act.crew :
               ASSIGN
                rate    = rate + mch-act.rate[i].                   
             END.            
             ELSE ASSIGN rate = mch-act.rate[int(mch-act.crew)] .
             IF rate = 0 THEN rate = 1.
             IF v-pct = 0 THEN v-pct = 1.
             ASSIGN x-mch.run-act = x-mch.run-act + mch-act.hours
                    /*x-mch.act-qty = x-mch.act-qty + mch-act.qty + mch-act.waste*/
                    x-mch.cst-std = x-mch.cst-std + x-mch.run-hr * rate * v-pct
                    x-mch.cst-act = x-mch.cst-act + ((mch-act.hours * rate) * v-pct).
           END.
           x-mch.act-qty = x-mch.act-qty + mch-act.qty + mch-act.waste.
           RELEASE x-mch.
      end.

      /*{jcrep/r-wipjobN2.i}  /* for tt-mat */*/
END. /*end job*/

for each x-mch by x-mch.line:


      create mch.
      BUFFER-COPY x-mch TO mch.


      IF x-mch.run-act > 0 and x-mch.est-speed <> 0 then
      DO:
         IF CAN-FIND(FIRST mach WHERE
            mach.company EQ cocode AND
            mach.loc     EQ locode AND
            mach.m-code  EQ x-mch.m-code AND
            mach.therm   EQ YES AND
            (mach.p-type EQ "R" OR mach.dept[1] EQ "LM")) THEN
            FOR EACH job-mat /*FIELDS(i-no len)*/ WHERE
                job-mat.company eq cocode AND
                job-mat.job-no = x-mch.job-no AND
                job-mat.job-no2 = x-mch.job-no2 AND
                job-mat.frm EQ x-mch.form-no AND
                job-mat.frm GT 0 AND
                job-mat.len GT 0
                no-lock,
                first ITEM FIELDS(mat-type) WHERE
                      item.company eq cocode AND
                      item.i-no eq x-mch.i-no AND ITEM.mat-type = "B"
                      no-lock
                      BY job-mat.frm
                      BY item.mat-type
                      BY job-mat.j-no
                      BY job-mat.rec_key:
              /*
                IF NOT can-FIND(FIRST tt-mat WHERE tt-mat.job-no = job-mat.job-no
                                 AND tt-mat.job-no2 = job-mat.job-no2
                                 AND tt-mat.form-no = job-mat.frm
                                 AND tt-mat.blank-no = job-mat.blank-no 
                                 AND tt-mat.rm-i-no = job-mat.rm-i-no)
                THEN NEXT.
                create mch.
                BUFFER-COPY x-mch TO mch.
            */    
                IF x-mch.est-speed NE 0 THEN
                   mch.run-hr = (x-mch.act-qty * job-mat.len / 12) / x-mch.est-speed.
                /*LEAVE. */
         END.

         ELSE IF x-mch.est-speed NE 0 THEN
            mch.run-hr = x-mch.act-qty / x-mch.est-speed.

         mch.cst-std = (x-mch.cst-std / x-mch.run-hr) *
                                     ROUND(((x-mch.act-qty) / x-mch.est-speed),2).


      END.





      RELEASE mch.
END.

FOR EACH mch:

     {custom/statusMsg.i " 'Processing Job#  '  + mch.job-no "}

      RELEASE job-hdr.

      FIND FIRST job-hdr WHERE
           job-hdr.company EQ cocode AND
           job-hdr.job-no EQ mch.job-no AND
           job-hdr.job-no2 EQ mch.job-no2 AND
           job-hdr.frm EQ mch.form-no AND
           (job-hdr.blank-no EQ mch.blank-no OR
            mch.blank-no EQ 0)
           NO-LOCK NO-ERROR.

      IF NOT AVAIL job-hdr THEN
         FIND FIRST job-hdr WHERE
              job-hdr.company EQ cocode AND
              job-hdr.job-no EQ mch.job-no AND
              job-hdr.job-no2 EQ mch.job-no2
              NO-LOCK NO-ERROR.

      IF AVAIL job-hdr THEN
         v-cust = job-hdr.cust-no.
      ELSE
         v-cust = "".      

      FIND FIRST tt-job-mch WHERE
           tt-job-mch.job-no EQ mch.job-no AND
           tt-job-mch.job-no2 EQ mch.job-no2 AND
           tt-job-mch.m-code  EQ mch.m-code /*AND
           tt-job-mch.die = */
           NO-ERROR.

      IF NOT AVAIL tt-job-mch THEN
      DO:
         CREATE tt-job-mch.
         ASSIGN
            tt-job-mch.job-no  = mch.job-no
            tt-job-mch.job-no2 = mch.job-no2
            tt-job-mch.m-code  = mch.m-code
            tt-job-mch.cust-no  = v-cust.

         FIND FIRST eb WHERE
              eb.company EQ cocode AND
              eb.est-no EQ mch.est-no AND
              eb.form-no EQ mch.form-no AND
              (eb.blank-no EQ mch.blank-no OR
               mch.blank-no EQ 0)
              NO-LOCK NO-ERROR.

         IF AVAIL eb THEN
            ASSIGN
               tt-job-mch.style = eb.style
               tt-job-mch.die = eb.die-no.

         ELSE IF NOT AVAIL eb THEN
         DO:
            FIND FIRST eb WHERE
                 eb.company EQ cocode AND
                 eb.est-no EQ mch.est-no AND
                 eb.form-no EQ mch.form-no
                 NO-LOCK NO-ERROR.

            IF AVAIL eb THEN
               ASSIGN
                  tt-job-mch.style = eb.style
                  tt-job-mch.die = eb.die-no.
         END.

         IF AVAIL eb AND
            eb.die-no EQ "" THEN
            FIND FIRST eb WHERE
                 eb.company EQ cocode AND
                 eb.est-no EQ mch.est-no AND
                 eb.die-no NE "" AND
                 eb.form-no NE 0
                 NO-LOCK NO-ERROR.

          IF AVAIL eb THEN
             tt-job-mch.die = eb.die-no.

          RELEASE eb.
      END.

   /*FOR each job-mat /*FIELDS(i-no basis-w wid len qty std-cost)*/ WHERE
                job-mat.company eq cocode AND
                job-mat.job-no = tt-job-mch.job-no AND
                job-mat.job-no2 = tt-job-mch.job-no2 AND
                job-mat.frm GT 0 AND
                job-mat.len GT 0
            AND can-FIND(FIRST tt-mat WHERE tt-mat.job-no = job-mat.job-no
            AND tt-mat.job-no2 = job-mat.job-no2
            AND tt-mat.form-no = job-mat.frm
            AND tt-mat.blank-no = job-mat.blank-no
            AND tt-mat.rm-i-no = job-mat.rm-i-no AND tt-mat.cst-act > 0)  NO-LOCK:
   */
      ASSIGN tt-job-mch.total-hrs = tt-job-mch.total-hrs + mch.mr-hr + mch.run-hr
             tt-job-mch.total-qty = tt-job-mch.total-qty + mch.act-qty
             tt-job-mch.cst-std = tt-job-mch.cst-std + mch.cst-std
             tt-job-mch.cst-act = tt-job-mch.cst-act + mch.cst-act.
      /* LEAVE.
   END.
   */

END.

for each tt-job-mch
      break by tt-job-mch.m-code
            BY tt-job-mch.job-no
            BY tt-job-mch.job-no2:

    {custom/statusMsg.i " 'Processing Job#  '  + tt-job-mch.job-no "}

     IF FIRST-OF(tt-job-mch.m-code) THEN
     DO:
        v-total-mach-hrs = 0.
/*         DISPLAY v-line FORMAT "X(80)" SKIP                                 */
/*                 "Machine: " + tt-job-mch.m-code FORMAT "X(17)"             */
/*                 SKIP(1)                                                    */
/*            WITH FRAME mach-head STREAM-IO width 180 no-labels no-box down. */
     END.

     v-total-mach-hrs = v-total-mach-hrs + tt-job-mch.total-hrs.

     FOR each job-mat /*FIELDS(i-no basis-w wid len qty std-cost)*/ WHERE
                job-mat.company eq cocode AND
                job-mat.job-no = tt-job-mch.job-no AND
                job-mat.job-no2 = tt-job-mch.job-no2 AND
                job-mat.frm GT 0 AND
                job-mat.len GT 0
            AND can-FIND(FIRST tt-mat WHERE tt-mat.job-no = job-mat.job-no
            AND tt-mat.job-no2 = job-mat.job-no2
            AND tt-mat.form-no = job-mat.frm
            AND tt-mat.blank-no = job-mat.blank-no
            AND tt-mat.rm-i-no = job-mat.rm-i-no /*AND tt-mat.cst-act > 0*/)  NO-LOCK,
        first ITEM /*FIELDS(mat-type)*/ WHERE
                     item.company eq cocode AND
                      item.i-no eq job-mat.rm-i-no AND ITEM.mat-type = "B"
                      NO-LOCK:

         {custom/statusMsg.i " 'Processing Job#  '  + job-mat.job-no "}

        {jcrep/r-wipjobN1.i}
        LEAVE.
     END.

     IF LAST-OF(tt-job-mch.m-code) THEN
     DO:
/*         DISPLAY SKIP(1)                                                                                               */
/*            SPACE(18)                                                                                                  */
/*            "Machine " + FILL(" ",6 - LENGTH(tt-job-mch.m-code)) + tt-job-mch.m-code + " Total Hours: " FORMAT "X(29)" */
/*            v-total-mach-hrs FORMAT "ZZZ,ZZ9.99"                                                                       */
/*            WITH FRAME tot-job STREAM-IO width 180 NO-LABELS no-box down.                                              */
/*                                                                                                                       */
        v-total-hrs = v-total-hrs + v-total-mach-hrs.
/*                                                                                                                       */
/*         IF tb_excel THEN                                                                                              */
/*         DO:                                                                                                           */
/*            EXPORT STREAM excel DELIMITER ","                                                                          */
/*                   ""                                                                                                  */
/*                   ""                                                                                                  */
/*                   ""                                                                                                  */
/*                   ""                                                                                                  */
/*                   "Total Time:"                                                                                       */
/*                   v-total-hrs                                                                                         */
/*                   SKIP.                                                                                               */
/*                                                                                                                       */
/*            PUT STREAM excel SKIP(1).                                                                                  */
/*         END.                                                                                                          */
     END.



END.


/*{jcrep/r-wipjobN.i}*/

  /*
DISPLAY v-line FORMAT "X(80)" SKIP(1)
          SPACE(21)
          "All Machines Total Hours: " FORMAT "X(26)"
          v-total-hrs FORMAT "ZZZ,ZZ9.99"
     WITH FRAME mach-end STREAM-IO width 180 no-labels no-box down.

IF tb_excel THEN
  DO:
     PUT STREAM excel SKIP.

     EXPORT STREAM excel DELIMITER ","
            ""
            ""
            ""
            ""
            "All Machines Total Hours:"
            v-total-hrs.
END.
*/
RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

IF tb_excel THEN DO:
     OUTPUT STREAM excel CLOSE.
     IF tb_runExcel THEN
         OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

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

