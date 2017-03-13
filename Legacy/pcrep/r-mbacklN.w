&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*----------------------------------------------------------------------*/
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
   field dept as char format 'xx'
   field m-code like mach.m-code
   field i-no like job-hdr.i-no
   field start-date as date
   field due-date as date
   FIELD both-date AS DATE
   field job like job.job
   field job-no like job.job-no
   field job-no2 like job.job-no2
   field r-std-hrs as dec format '>>>9.99'
   field r-act-hrs as dec format '>>>9.99'
   field m-std-hrs as dec format '>>>9.99'
   field m-act-hrs as dec format '>>>9.99'
   field qty-rem as dec format '>>>>>>>>9'.

DEF VAR v-print-fmt AS CHARACTER.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF STREAM excel.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR cColumnInit AS LOG INIT YES NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.

ASSIGN cTextListToSelect = "MACH CODE,CUSTOMER NAME,MACHINE DESCRIPTION,ITEM NUMBER," +
                                               "DUE DATE,START DATE,JOB #,QUANTITY REMAINING,STNDRD RUN HOURS,STNDRD MR HOURS," +
                                               "ACTUAL RUN HOURS,ACTUAL MR HOURS,REMAIN RUN HOURS,REMAIN MR HOURS,TOTAL HOURS REMAIN," +
                                                "WEEKLY HOURS REMAIN,ACCUM HOURS REMAIN"
       cFieldListToSelect = "mch-cd,cust-name,mch-dscr,i-no," +
                                        "due-dt,strt-dt,job,qty-rmn,st-run,st-mr," +
                                        "acl-run,acl-mr,rmn-run,rmn-mr,ttl-hrs," +
                                        "wk-hrs,ac-hrs"
       cFieldLength = "5,30,30,15," + "10,10,10,9,7,7," + "7,7,7,7,7," + "7,7" 
       cFieldType = "c,c,c,c," + "c,c,c,i,i,i," + "i,i,i,i,i," + "i,i"
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  =  "MACH CODE,CUSTOMER NAME,MACHINE DESCRIPTION,ITEM NUMBER," +
                                               "DUE DATE,START DATE,JOB #,QUANTITY REMAINING,STNDRD RUN HOURS,STNDRD MR HOURS," +
                                               "ACTUAL RUN HOURS,ACTUAL MR HOURS,REMAIN RUN HOURS,REMAIN MR HOURS,TOTAL HOURS REMAIN," +
                                                "WEEKLY HOURS REMAIN,ACCUM HOURS REMAIN"  .

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
end_mach begin_date sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up ~
btn_down rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel ~
tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_dept end_dept begin_mach end_mach ~
begin_date sl_avail sl_selected rd-dest lv-ornt lines-per-page lv-font-no ~
lv-font-name td-show-parm tb_excel tb_runExcel fi_file 

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
     LABEL "For Job Orders with Start Dates Before" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

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

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-mbackl.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
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
     SIZE 20 BY 6.67 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.29.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 5.95.

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
     begin_dept AT ROW 2.91 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Department"
     end_dept AT ROW 2.91 COL 70 COLON-ALIGNED HELP
          "Enter Ending Department"
     begin_mach AT ROW 3.86 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Machine"
     end_mach AT ROW 3.86 COL 70 COLON-ALIGNED HELP
          "Enter Ending Machine"
     begin_date AT ROW 5.76 COL 52 COLON-ALIGNED
     sl_avail AT ROW 7.91 COL 4 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 7.91 COL 40 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 7.91 COL 59.4 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 8.91 COL 40 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 9.91 COL 40 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 10.95 COL 40 WIDGET-ID 40
     btn_down AT ROW 11.95 COL 40 WIDGET-ID 42
     rd-dest AT ROW 14.57 COL 5 NO-LABEL
     lv-ornt AT ROW 14.81 COL 31 NO-LABEL
     lines-per-page AT ROW 14.81 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 17.19 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 18.14 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 19.29 COL 31
     tb_excel AT ROW 20.38 COL 51 RIGHT-ALIGNED
     tb_runExcel AT ROW 20.38 COL 72 RIGHT-ALIGNED
     fi_file AT ROW 21.19 COL 29 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 23.1 COL 19
     btn-cancel AT ROW 23.1 COL 57
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 7.19 COL 59.4 WIDGET-ID 44
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 13.86 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 7.19 COL 4.8 WIDGET-ID 38
     RECT-6 AT ROW 13.38 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 23.91.


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
         TITLE              = "Machine Backlog"
         HEIGHT             = 24.14
         WIDTH              = 96.2
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
ON END-ERROR OF C-Win /* Machine Backlog */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Machine Backlog */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* For Job Orders with Start Dates Before */
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
  SESSION:SET-WAIT-STATE ("general").

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  assign rd-dest.
  IF v-print-fmt EQ "Pacific" OR v-print-fmt EQ "Xprint" OR v-print-fmt = "southpak"
       THEN is-xprint-form = YES.     
  ELSE is-xprint-form = NO.

  RUN GetSelectionList.
  run run-report. 

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
  SESSION:SET-WAIT-STATE("").
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

  begin_date = today.

  RUN DisplaySelectionList.
  RUN enable_UI.

  {methods/nowait.i}

      DO WITH FRAME {&FRAME-NAME}:
          {custom/usrprint.i}
              RUN DisplaySelectionList2.
          APPLY "entry" TO begin_dept .
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
  DISPLAY begin_dept end_dept begin_mach end_mach begin_date sl_avail 
          sl_selected rd-dest lv-ornt lines-per-page lv-font-no lv-font-name 
          td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_dept end_dept begin_mach end_mach begin_date 
         sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down 
         rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel 
         tb_runExcel fi_file btn-ok btn-cancel 
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
/* ------------------------------------------------ pc/rep/mch-bkg.p 8/94 gb */
/* Machine Backlog Report                                            */
/* -------------------------------------------------------------------------- */

/*{sys/form/r-topw.f}*/

def var v-dept as ch format 'x(4)' extent 2 init ["","zzzz"] no-undo.
def var v-mach like mach.m-code extent 2 init ["","zzzzzz"] no-undo.
def var v-name as log format "Cust/Mach" init NO no-undo.
def var v-due-d as log format "Due/Start" init NO no-undo.
def var v-date as date format "99/99/9999" no-undo.
def var v-left as log format "Y/N" init no no-undo.

def var qty AS INT no-undo.
def var mr-hr like job-mch.mr-hr no-undo.
def var run-hr like job-mch.run-hr no-undo.
def var job-mr-hrs as dec format '>>>9.99' no-undo.
def var job-run-hrs as dec format '>>>9.99' no-undo.
def var tot-hrs as dec format '>>>9.99' no-undo.
def var mr-flg as log no-undo.
def var run-flg as log no-undo.
def var v-stats as char init "A,R,W,L,P" NO-UNDO.
def var v-tot-hrs as dec extent 2 format ">,>>9.99" NO-UNDO.
def var v-cum-hrs as dec extent 4 format ">,>>9.99" NO-UNDO.
def var v-saturday as date init 01/01/0001 NO-UNDO.
def var v-hdr as char format "x(131)" extent 4 NO-UNDO.

DEF VAR cDisplay AS cha NO-UNDO.
DEF VAR cExcelDisplay AS cha NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR cTmpField AS CHA NO-UNDO.
DEF VAR cVarValue AS cha NO-UNDO.
DEF VAR cExcelVarValue AS cha NO-UNDO.
DEF VAR cSelectedList AS cha NO-UNDO.
DEF VAR cFieldName AS cha NO-UNDO.
DEF VAR str-tit4 AS cha FORM "x(300)" NO-UNDO.
DEF VAR str-tit5 AS cha FORM "x(300)" NO-UNDO.
DEF VAR str-tit6 AS cha FORM "x(300)" NO-UNDO.
DEF VAR str-line AS cha FORM "x(300)" NO-UNDO.

DEF VAR cCustomerName AS cha FORM "x(25)" NO-UNDO.
DEF VAR cPrepDscr AS cha FORM "x(25)" NO-UNDO.
{sys/form/r-top5L3.f} 
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

DEF VAR excelheader AS CHAR NO-UNDO.
DEF VAR viLoop AS INT NO-UNDO.

/*form header skip(1) v-hdr with frame r-top.*/

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 v-dept[1] = begin_dept
 v-dept[2] = end_dept
 v-mach[1] = begin_mach
 v-mach[2] = end_mach
 /*v-name    = rd_show1 EQ "Customer"
 v-due-d   = rd_show2 EQ "Due" */
 v-date    = begin_date
 /*v-left    = tb_show3*/

 /*v-hdr[1] = fill(" ",73) + if v-left then " TOTAL   WEEKLY    ACCUM" else
                                          "STNDRD  STNDRD  ACTUAL  ACTUAL  REMAIN  REMAIN   TOTAL"
 v-hdr[2] = "MACH" + fill(" ",40) + (if v-due-d then "DUE  " else "START") +
            fill(" ",14) +
            "QUANTITY   " + if v-left then "HOURS    HOURS    HOURS" else
                            ("  RUN      MR     RUN      MR     RUN      MR   HOURS")
 v-hdr[3] = "CODE   " + (if v-name then "CUSTOMER NAME      "
                                   else "MACHINE DESCRIPTION") +
            "  ITEM NUMBER     DATE     JOB#     REMAINING  " +
            if v-left then "REMAIN   REMAIN   REMAIN" else
                           " HOURS   HOURS   HOURS   HOURS   HOURS   HOURS  REMAIN"
 v-hdr[4] = fill("-",97) + if v-left then "" else fill("-",30) */
 v-tot-hrs = 0
 v-cum-hrs = 0.

EMPTY TEMP-TABLE work-rep.

FORM HEADER
    str-tit6 SKIP 
    with frame r-top.

DEF VAR cslist AS cha NO-UNDO.
 FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

   IF ttRptSelected.TextList = "MACH CODE"  THEN
         ASSIGN
         str-tit3  = str-tit3 + "     " + " "
         str-tit4  = str-tit4 + " MACH" + " "
         str-tit5 = str-tit5 +  " CODE" + " "
               str-tit6 = str-tit6 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + ","  .

     ELSE IF  ttRptSelected.TextList = "QUANTITY REMAINING"  THEN ASSIGN
         str-tit3  = str-tit3 + "         " + " "
         str-tit4  = str-tit4 + " QUANTITY" + " "
         str-tit5 = str-tit5 +  "REMAINING" + " "
               str-tit6 = str-tit6 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + ","  .

     ELSE IF  ttRptSelected.TextList = "STNDRD RUN HOURS"  THEN ASSIGN
         str-tit3  = str-tit3 + " STNDRD" + " "
         str-tit4 = str-tit4 +  "    RUN" + " "
         str-tit5 = str-tit5 +  "  HOURS" + " "
               str-tit6 = str-tit6 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + ","  .

     ELSE IF  ttRptSelected.TextList = "STNDRD MR HOURS"  THEN ASSIGN
         str-tit3  = str-tit3 + " STNDRD" + " "
         str-tit4 = str-tit4 +  "     MR" + " "
         str-tit5 = str-tit5 +  "  HOURS" + " "
               str-tit6 = str-tit6 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + ","  .

     ELSE IF  ttRptSelected.TextList = "ACTUAL RUN HOURS"  THEN ASSIGN
         str-tit3  = str-tit3 + " ACTUAL" + " "
         str-tit4 = str-tit4 +  "    RUN" + " "
         str-tit5 = str-tit5 +  "  HOURS" + " "
               str-tit6 = str-tit6 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + ","  .

     ELSE IF  ttRptSelected.TextList = "ACTUAL MR HOURS"  THEN ASSIGN
         str-tit3  = str-tit3 + " ACTUAL" + " "
         str-tit4 = str-tit4 +  "     MR" + " "
         str-tit5 = str-tit5 +  "  HOURS" + " "
               str-tit6 = str-tit6 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + ","  .

     ELSE IF  ttRptSelected.TextList = "REMAIN RUN HOURS"  THEN ASSIGN
         str-tit3  = str-tit3 + " REMAIN" + " "
         str-tit4 = str-tit4 +  "    RUN" + " "
         str-tit5 = str-tit5 +  "  HOURS" + " "
               str-tit6 = str-tit6 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + ","  .

     ELSE IF  ttRptSelected.TextList = "REMAIN MR HOURS"  THEN ASSIGN
         str-tit3  = str-tit3 + " REMAIN" + " "
         str-tit4 = str-tit4 +  "     MR" + " "
         str-tit5 = str-tit5 +  "  HOURS" + " "
               str-tit6 = str-tit6 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + ","  .

     ELSE IF  ttRptSelected.TextList = "TOTAL HOURS REMAIN"  THEN ASSIGN
         str-tit3  = str-tit3 + "  TOTAL" + " "
         str-tit4 = str-tit4 +  "  HOURS" + " "
         str-tit5 = str-tit5 +  " REMAIN" + " "
               str-tit6 = str-tit6 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + ","  .

     ELSE IF  ttRptSelected.TextList = "WEEKLY HOURS REMAIN"  THEN ASSIGN
         str-tit3  = str-tit3 + " WEEKLY" + " "
         str-tit4 = str-tit4 +  "  HOURS" + " "
         str-tit5 = str-tit5 +  " REMAIN" + " "
               str-tit6 = str-tit6 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + ","  .

     ELSE IF  ttRptSelected.TextList = "ACCUM HOURS REMAIN"  THEN ASSIGN
         str-tit3  = str-tit3 + "  ACCUM" + " "
         str-tit4 = str-tit4 +  "  HOURS" + " "
         str-tit5 = str-tit5 +  " REMAIN" + " "
               str-tit6 = str-tit6 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + ","  .


   ELSE DO:
       IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
           THEN ASSIGN str-tit5 = str-tit5 + ttRptSelected.TextList + " "
           str-tit4 = str-tit4 + FILL(" ",ttRptSelected.FieldLength) + " "
           str-tit3 = str-tit3 + FILL(" ",ttRptSelected.FieldLength) + " "
           str-tit6 = str-tit6 + FILL("-",ttRptSelected.FieldLength) + " "
           excelheader = excelHeader + ttRptSelected.TextList + "," .        
       ELSE 
           ASSIGN str-tit5 = str-tit5 + 
            (IF ttRptSelected.HeadingFromLeft THEN
                ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
            ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
          str-tit4 = str-tit4 + FILL(" ",ttRptSelected.FieldLength) + " "
          str-tit3 = str-tit3 + FILL(" ",ttRptSelected.FieldLength) + " "
          str-tit6 = str-tit6 + FILL("-",ttRptSelected.FieldLength) + " " 
          excelheader = excelHeader + ttRptSelected.TextList + ","  .        
   END.  
   cSlist = cSlist + ttRptSelected.FieldList + ",".
   IF LOOKUP(ttRptSelected.TextList, "REMAIN RUN HOURS,REMAIN MR HOURS,TOTAL HOURS REMAIN,WEEKLY HOURS REMAIN,ACCUM HOURS REMAIN") <> 0    THEN
       ASSIGN
       str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
   ELSE
       str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
 /* IF v-left THEN
     excelheader = "MACH CODE,"
                 + (IF v-name THEN "CUSTOMER NAME,"
                   ELSE "MACHINE DESCRIPTION,")
                 + "ITEM NUMBER,"
                 + (IF v-due-d then "DUE " ELSE "START ")
                 + "DATE,JOB #,QUANTITY REMAINING,TOTAL HOURS REMAIN,"
                 + "WEEKLY HOURS REMAIN,ACCUM HOURS REMAIN".
  ELSE
     excelheader = "MACH CODE,"
                 + (IF v-name THEN "CUSTOMER NAME,"
                   ELSE "MACHINE DESCRIPTION,")
                 + "ITEM NUMBER,"
                 + (IF v-due-d then "DUE " ELSE "START ")
                 + "DATE,JOB #,QUANTITY REMAINING,STNDRD RUN HOURS,"
                 + "STNDRD MR HOURS,ACTUAL RUN HOURS,ACTUAL MR HOURS,"
                 + "REMAIN RUN HOURS,REMAIN MR HOURS,TOTAL HOURS REMAIN".*/

  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.


 SESSION:SET-WAIT-STATE ("general").

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

display "" with frame r-top.

   do i = 1 to num-entries(v-stats):
   for each job
       where job.company    eq cocode
         and job.stat       eq entry(i,v-stats)
         and job.start-date le v-date
       use-index stat-idx no-lock,

       first est
       where est.est-no  eq job.est-no
         and est.company eq job.company
       no-lock:

      _mat:
      for each job-mch
          where job-mch.company eq cocode
            and job-mch.job     eq job.job
          use-index job no-lock,

          first job-hdr
          where job-hdr.company   eq cocode
            and job-hdr.job       eq job.job
            and job-hdr.job-no    eq job.job-no
            and job-hdr.job-no2   eq job.job-no2
            and (job-hdr.frm      eq job-mch.frm or
                 est.est-type eq 2 or est.est-type eq 6)
            and (job-hdr.blank-no eq job-mch.blank-no or job-mch.blank-no eq 0)
          no-lock:

         if job-mch.m-code lt v-mach[1] or job-mch.m-code gt v-mach[2] or
            job-mch.dept  lt v-dept[1] or job-mch.dept   gt v-dept[2] then next.

         assign run-hr = 0
                qty    = 0
                mr-hr  = 0
                mr-flg = no
                run-flg = no.

         if job-mch.mr-hr gt 0 then do:
            for each mch-act where mch-act.company = cocode and
                                   mch-act.m-code  = job-mch.m-code and
                                   mch-act.job-no  = job-mch.job-no and
                                   mch-act.job-no2 = job-mch.job-no2 and
                                   mch-act.frm     = job-mch.frm and
                                   (mch-act.blank-no = job-mch.blank-no
                                                     or est.est-type eq 1
                                                     or est.est-type eq 5) and
                                   mch-act.pass     = job-mch.pass
                                   use-index operation
                                   no-lock:
               find job-code where job-code.code    = mch-act.code
                                   no-lock no-error.
               if not avail job-code then next.

               if job-code.cat ne "MR" then next.

               if mch-act.complete then do:
                  assign mr-flg = yes.
                  leave.
               end.
            end.
         end.
         else
            assign mr-flg = yes.

         if job-mch.run-hr gt 0 then do:
            for each mch-act where mch-act.company = cocode and
                                   mch-act.m-code  = job-mch.m-code and
                                   mch-act.job-no  = job-mch.job-no and
                                   mch-act.job-no2 = job-mch.job-no2 and
                                   mch-act.frm     = job-mch.frm and
                                   (mch-act.blank-no = job-mch.blank-no
                                                     or est.est-type eq 1
                                                     or est.est-type eq 5) and
                                   mch-act.pass     = job-mch.pass
                                   use-index operation
                                   no-lock:
               find job-code where job-code.code    = mch-act.code
                                   no-lock no-error.
               if not avail job-code then next.

               if job-code.cat ne "RUN" then next.

               if mch-act.complete then do:
                  assign run-flg = yes.
                  leave.
               end.
            end.
         end.
         else
            assign run-flg = yes.

         if run-flg and mr-flg then next.

         for each mch-act where mch-act.company = cocode and
                                mch-act.m-code  = job-mch.m-code and
                                mch-act.job-no  = job-mch.job-no and
                                mch-act.job-no2 = job-mch.job-no2 and
                                mch-act.frm     = job-mch.frm and
                                (mch-act.blank-no = job-mch.blank-no
                                                   or est.est-type eq 1
                                                   or est.est-type eq 5) and
                                mch-act.pass     = job-mch.pass
                                use-index operation
                                no-lock:

            find job-code where job-code.code    = mch-act.code
                                no-lock no-error.
            if not avail job-code then next.
            if job-code.cat = "RUN" and not run-flg then
            do:
               run-hr    = run-hr   + mch-act.hours.
               qty       = qty      +
                           IF mch-act.qty EQ ? THEN 0 ELSE mch-act.qty.
            end.
            else if job-code.cat = "MR" and not mr-flg then
               mr-hr     = mr-hr    + mch-act.hours.
         end.
         find first work-rep where work-rep.m-code = job-mch.m-code and
                                   work-rep.job-no = job-mch.job-no and
                                   work-rep.job-no2 = job-mch.job-no2
                                   no-error.
         if not avail work-rep then do:
           find first oe-ordl
               where oe-ordl.company eq cocode
                 and oe-ordl.job-no  eq job-hdr.job-no
                 and oe-ordl.job-no2 eq job-hdr.job-no2
                 and oe-ordl.ord-no  eq job-hdr.ord-no
                 and oe-ordl.i-no    eq job-hdr.i-no
               use-index job no-lock no-error.

            create work-rep.
            assign
             work-rep.m-code     = job-mch.m-code
             work-rep.i-no       = job-hdr.i-no
             work-rep.start-date = job.start-date
             work-rep.due-date = IF avail oe-ordl then oe-ordl.prom-date ELSE ?
             work-rep.both-date = if avail oe-ordl then oe-ordl.prom-date else job.start-date
             work-rep.job        = job-mch.job
             work-rep.job-no     = job-mch.job-no
             work-rep.job-no2    = job-mch.job-no2.
         end.
         if not run-flg then
            assign work-rep.r-std-hrs = work-rep.r-std-hrs + job-mch.run-hr
                work-rep.qty-rem   = work-rep.qty-rem + (job-mch.run-qty - qty).

         if not mr-flg then
            assign work-rep.m-std-hrs = work-rep.m-std-hrs + job-mch.mr-hr.

         assign work-rep.r-act-hrs = work-rep.r-act-hrs + run-hr
                work-rep.m-act-hrs = work-rep.m-act-hrs + mr-hr.
      end.
   end.
   end.

   for each work-rep,

          first job-hdr NO-LOCK
          where job-hdr.company eq cocode
            and job-hdr.job     eq work-rep.job
            and job-hdr.job-no  eq work-rep.job-no
            and job-hdr.job-no2 eq work-rep.job-no2

          break by work-rep.m-code
                by work-rep.both-date
                by work-rep.job-no
                by work-rep.job-no2:

         if first-of(work-rep.m-code) then v-cum-hrs = 0.

         if first-of(work-rep.both-date) then do:
           if /*v-left and*/ not first(work-rep.both-date) then do:

          /*   put "-------"                    to 79
                 /*"TOTAL"                      to 70*/
                 v-tot-hrs[1] + v-tot-hrs[2]  to 79 format ">,>>9.99".

              IF tb_excel THEN
              DO:
                 PUT STREAM excel UNFORMATTED SKIP(1).

                 DO viLoop = 1 TO 5:
                    PUT STREAM excel UNFORMATTED
                        '"' "" '",'.
                 END.

                 PUT STREAM excel UNFORMATTED
                    '"' "TOTAL" '",'
                    '"' STRING(v-tot-hrs[1] + v-tot-hrs[2],">,>>9.99") '",'.
              END.  */

             if work-rep.start-date gt v-saturday then do:
             /*  put space(1) v-cum-hrs[1] format ">,>>9.99"
                   space(1) v-cum-hrs[2] format ">,>>9.99".

               IF tb_excel THEN
                  PUT STREAM excel UNFORMATTED
                      '"' STRING(v-cum-hrs[1],">,>>9.99") '",'
                      '"' STRING(v-cum-hrs[2],">,>>9.99") '",'.  */

               v-cum-hrs[1] = 0.
             end.

             put skip(1).

           /*  IF tb_excel THEN
                PUT STREAM excel UNFORMATTED
                    SKIP(1).*/
           end.

           v-tot-hrs = 0.
         end.

         find first cust
             where cust.company eq cocode
               and cust.cust-no eq job-hdr.cust-no
             no-lock no-error.

         find first mach
             where mach.company eq cocode
               and mach.m-code  eq work-rep.m-code
             no-lock no-error.

         ASSIGN
            job-mr-hrs = work-rep.m-std-hrs - work-rep.m-act-hrs
            job-run-hrs = work-rep.r-std-hrs - work-rep.r-act-hrs.

         if job-mr-hrs  lt 0 then job-mr-hrs  = 0.
         if job-run-hrs lt 0 then job-run-hrs = 0.

         if work-rep.qty-rem lt 0 then work-rep.qty-rem = 0.

         tot-hrs = job-mr-hrs + job-run-hrs.

       /*  display work-rep.m-code
                    when first-of(work-rep.m-code)
                 mach.m-dscr
                    when first-of(work-rep.m-code) and avail mach
                 cust.name
                    when v-name and avail cust @ mach.m-dscr
                 work-rep.i-no
                 work-rep.start-date
                    when first-of(work-rep.start-date)
                 trim(work-rep.job-no) + "-" + string(work-rep.job-no2,"99")
                 work-rep.qty-rem
                 work-rep.r-std-hrs     when not v-left
                 tot-hrs                when v-left
                                        @ work-rep.r-std-hrs
                 work-rep.m-std-hrs     when not v-left
                 work-rep.r-act-hrs     when not v-left
                 work-rep.m-act-hrs     when not v-left
                 job-run-hrs            when not v-left
                 job-mr-hrs             when not v-left
                 tot-hrs                when not v-left
                 with frame det STREAM-IO width 132 no-box no-attr-space no-labels down.

         down with frame det.

         IF tb_excel THEN
            PUT STREAM excel UNFORMATTED
               '"' (IF FIRST-OF(work-rep.m-code) THEN
                       work-rep.m-code ELSE "")                '",'
               '"' (IF v-name and avail cust THEN
                       cust.name
                    ELSE IF FIRST-OF(work-rep.m-code) AND
                         avail mach THEN mach.m-dscr ELSE "")  '",'
               '"' work-rep.i-no                               '",'
               '"' (IF FIRST-OF(work-rep.start-date)
                       THEN STRING(work-rep.start-date)
                       ELSE "")                                '",'
               '"' trim(work-rep.job-no) + "-" +
                   string(work-rep.job-no2,"99")               '",'
               '"' work-rep.qty-rem                            '",'
               '"' (IF not v-left THEN
                       STRING(work-rep.r-std-hrs)
                       ELSE STRING(tot-hrs))                   '",'
               '"' (IF not v-left THEN
                       STRING(work-rep.m-std-hrs) ELSE "")     '",'
               '"' (IF not v-left THEN
                       STRING(work-rep.r-act-hrs) ELSE "")     '",'
               '"' (IF not v-left THEN
                       STRING(work-rep.m-act-hrs) ELSE "")     '",'
               '"' (IF not v-left THEN STRING(job-run-hrs)
                    ELSE "")                                   '",'
               '"' (IF not v-left THEN STRING(job-mr-hrs)
                    ELSE "")                                   '",'
               '"' (IF not v-left THEN STRING(tot-hrs)
                    ELSE "")                                   '",'
               SKIP. */

         ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "mch-cd"    THEN cVarValue =  (IF FIRST-OF(work-rep.m-code) THEN work-rep.m-code ELSE "")  .
                         WHEN "cust-name" THEN cVarValue = IF avail cust THEN cust.name ELSE "".
                         WHEN "mch-dscr"  THEN cVarValue = IF FIRST-OF(work-rep.m-code) AND avail mach THEN mach.m-dscr ELSE "" .
                         WHEN "i-no"      THEN cVarValue = work-rep.i-no .
                         WHEN "strt-dt"   THEN cVarValue = (IF FIRST-OF(work-rep.both-date) AND work-rep.start-date NE ? THEN STRING(work-rep.start-date) ELSE "")  . 
                         WHEN "due-dt"   THEN cVarValue = (IF FIRST-OF(work-rep.both-date) AND work-rep.due-date NE ? THEN STRING(work-rep.due-date) ELSE "")  .    
                         WHEN "job"        THEN cVarValue = string(trim(work-rep.job-no) + "-" + string(work-rep.job-no2,"99"))  .
                         WHEN "qty-rmn"       THEN cVarValue = STRING(work-rep.qty-rem,">>>>>>>>9") .
                         WHEN "st-run"       THEN cVarValue =  STRING(work-rep.r-std-hrs,">>>9.99")   .
                         WHEN "st-mr"       THEN cVarValue = STRING(work-rep.m-std-hrs,">>>9.99")  .
                         WHEN "acl-run"     THEN cVarValue = STRING(work-rep.r-act-hrs,">>>9.99")  .
                         WHEN "acl-mr"     THEN cVarValue = STRING(work-rep.m-act-hrs,">>>9.99") .
                         WHEN "rmn-run"     THEN cVarValue = STRING(job-run-hrs,">>>9.99") .
                         WHEN "rmn-mr"            THEN cVarValue = STRING(job-mr-hrs,">>>9.99") .
                         WHEN "ttl-hrs"    THEN cVarValue = STRING(tot-hrs,">>>9.99")  .
                         WHEN "wk-hrs"      THEN cVarValue = "" .
                         WHEN "ac-hrs"            THEN cVarValue = "" .

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
          v-tot-hrs[1] = v-tot-hrs[1] + job-run-hrs
          v-tot-hrs[2] = v-tot-hrs[2] + job-mr-hrs
          v-cum-hrs[1] = v-cum-hrs[1] + job-run-hrs + job-mr-hrs
          v-cum-hrs[2] = v-cum-hrs[2] + job-run-hrs + job-mr-hrs.

       /*  if last-of(work-rep.both-date) then do: */

           /*if not v-left then
           DO:*/
            /* put "-------"                    to 111
                 "-------"                    to 119
                 "-------"                    to 127
                 "TOTAL"                      to 102
                 v-tot-hrs[1]                 to 111
                 v-tot-hrs[2]                 to 119
                 v-tot-hrs[1] + v-tot-hrs[2]  to 127 format ">,>>9.99"
                 skip(1).

             IF tb_excel THEN
              DO:
                 PUT STREAM excel UNFORMATTED SKIP(1).

                 DO viLoop = 1 TO 9:
                    PUT STREAM excel UNFORMATTED
                        '"' "" '",'.
                 END.

                 PUT STREAM excel UNFORMATTED
                    '"' "TOTAL" '",'
                    '"' STRING(v-tot-hrs[1],">,>>9.99")                '",'
                    '"' STRING(v-tot-hrs[2],">,>>9.99")                '",'
                    '"' STRING(v-tot-hrs[1] + v-tot-hrs[2],">,>>9.99") '",'
                    SKIP(1).
              END.*/
         /*  END.

           else*/
          /* if last(work-rep.start-date) then
           DO:
             put "-------"                    to 79
                 "TOTAL"                      to 70
                 v-tot-hrs[1] + v-tot-hrs[2]  to 79 format ">,>>9.99"
                 space(1) v-cum-hrs[1]              format ">,>>9.99"
                 space(1) v-cum-hrs[2]              format ">,>>9.99"
                 skip(1).

             IF tb_excel THEN
              DO:
                 PUT STREAM excel UNFORMATTED SKIP(1).

                 DO viLoop = 1 TO 5:
                    PUT STREAM excel UNFORMATTED
                        '"' "" '",'.
                 END.

                 PUT STREAM excel UNFORMATTED
                    '"' "TOTAL" '",'
                    '"' STRING(v-tot-hrs[1] + v-tot-hrs[2],">,>>9.99") '",'
                    '"' STRING(v-cum-hrs[1],">,>>9.99")                '",'
                    '"' STRING(v-cum-hrs[2],">,>>9.99")                '",'
                    SKIP(1).
              END.
           END.*/



           ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "mch-cd"    THEN cVarValue = "" .
                         WHEN "cust-name" THEN cVarValue = "" .
                         WHEN "mch-dscr"  THEN cVarValue = "" .
                         WHEN "i-no"      THEN cVarValue = "" .
                         WHEN "strt-dt"   THEN cVarValue = "" .
                         WHEN "due-dt"   THEN cVarValue = "" .
                         WHEN "job"        THEN cVarValue = "" .
                         WHEN "qty-rmn"       THEN cVarValue = "" .
                         WHEN "st-run"       THEN cVarValue = "" .
                         WHEN "st-mr"       THEN cVarValue = "" .
                         WHEN "acl-run"     THEN cVarValue = "" .
                         WHEN "acl-mr"     THEN cVarValue = "" .
                         WHEN "rmn-run"     THEN cVarValue = IF last-of(work-rep.both-date) THEN STRING(v-tot-hrs[1],">,>>9.99")  ELSE "".
                         WHEN "rmn-mr"            THEN cVarValue = IF last-of(work-rep.both-date) THEN STRING(v-tot-hrs[2],">,>>9.99")  ELSE "".
                         WHEN "ttl-hrs"    THEN cVarValue = IF last-of(work-rep.both-date) THEN  STRING(v-tot-hrs[1] + v-tot-hrs[2],">,>>9.99") ELSE "" .
                         WHEN "wk-hrs"      THEN cVarValue =  IF first-of(work-rep.both-date) THEN STRING(v-cum-hrs[1],">,>>9.99") ELSE ""  .
                         WHEN "ac-hrs"            THEN cVarValue = IF first-of(work-rep.both-date) THEN STRING(v-cum-hrs[2],">,>>9.99") ELSE ""  .

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT SKIP str-line SKIP.
            PUT UNFORMATTED "TOTAL " substring(cDisplay,8,300) SKIP.
             IF tb_excel THEN DO:
                  PUT STREAM excel UNFORMATTED  
                        "TOTALS " + substring(cExcelDisplay,3,300)  SKIP.
             END.

             IF last-of(work-rep.both-date) THEN
             ASSIGN v-saturday = 7 - weekday(work-rep.both-date) + work-rep.both-date.


        /* end.*/
   end. /* each item */

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2002 Advanced Software, Inc. */

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

