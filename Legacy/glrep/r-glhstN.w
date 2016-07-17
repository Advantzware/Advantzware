&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: glrep\r-glhist.w

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

DEF NEW SHARED VAR udate AS DATE NO-UNDO.
DEF NEW SHARED VAR uperiod AS INT NO-UNDO.
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF STREAM excel.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR v-costM AS DEC FORMAT ">>>,>>9.99<<" NO-UNDO.
DEF VAR v-sellValue LIKE itemfg.sell-price NO-UNDO.
DEF VAR v-counted-date AS DATE NO-UNDO.
DEF VAR v-cust-no LIKE itemfg.cust-no NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.

ASSIGN cTextListToSelect = "Account Number,Account Description,Run #,Journal," +
                           "Reference,AP Description,Date,Amount,Balance"
       cFieldListToSelect = "actnum,actdesc,run,jour," + "ref,ap-desc,date,amt,bal" 
       cFieldLength = "14,40,8,8," + "52,30,10,15,19"
       cFieldType   = "c,c,i,i," + "c,c,c,i,i"
       .
ASSIGN cTextListToDefault  = "Account Number,Account Description,Run #,Journal," +
                           "Reference,Date,Amount,Balance".

{sys/inc/ttRptSel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_date end_date begin_acct ~
end_acct tb_exc-auto tb_detailed sl_avail Btn_Def sl_selected Btn_Add ~
Btn_Remove btn_Up btn_down rd-dest lv-ornt lines-per-page lv-font-no ~
td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date begin_acct end_acct ~
tb_exc-auto tb_detailed sl_avail sl_selected rd-dest lv-ornt lines-per-page ~
lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file 

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

DEFINE VARIABLE begin_acct AS CHARACTER FORMAT "X(25)":U 
     LABEL "Beginning Acct#" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE end_acct AS CHARACTER FORMAT "X(25)":U INITIAL "zzzzzzzzzzzzzzzzzzzzzzzzz" 
     LABEL "Ending Acct#" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-glhist.csv" 
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
     SIZE 19 BY 6.67 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 9.52.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 6.91.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 34 BY 5 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 34 BY 5 NO-UNDO.

DEFINE VARIABLE tb_detailed AS LOGICAL INITIAL yes 
     LABEL "Detailed?" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .95 NO-UNDO.

DEFINE VARIABLE tb_exc-auto AS LOGICAL INITIAL yes 
     LABEL "Exclude Auto Distributions?" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .95 NO-UNDO.

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
     begin_date AT ROW 2.52 COL 19 COLON-ALIGNED
     end_date AT ROW 2.52 COL 63 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     begin_acct AT ROW 3.71 COL 19 COLON-ALIGNED HELP
          "Enter Beginning Account Number"
     end_acct AT ROW 3.71 COL 63 COLON-ALIGNED HELP
          "Enter Ending Account Number"
     tb_exc-auto AT ROW 5.14 COL 38
     tb_detailed AT ROW 6.33 COL 38
     sl_avail AT ROW 9.19 COL 3 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 9.19 COL 40.8 HELP
          "Default Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 9.19 COL 61.2 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 10.19 COL 40.8 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 11.19 COL 40.8 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 12.19 COL 40.8 WIDGET-ID 40
     btn_down AT ROW 13.19 COL 40.8 WIDGET-ID 42
     rd-dest AT ROW 15.67 COL 6 NO-LABEL
     lv-ornt AT ROW 15.91 COL 31 NO-LABEL
     lines-per-page AT ROW 15.91 COL 85 COLON-ALIGNED
     lv-font-no AT ROW 17.57 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 18.76 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 19.95 COL 31
     tb_excel AT ROW 21.38 COL 51 RIGHT-ALIGNED
     tb_runExcel AT ROW 21.38 COL 72 RIGHT-ALIGNED
     fi_file AT ROW 22.19 COL 29 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 24.48 COL 19
     btn-cancel AT ROW 24.48 COL 58
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 14.95 COL 3
          FGCOLOR 9 
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Available Columns" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 8.43 COL 3.4 WIDGET-ID 38
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 8.43 COL 61.2 WIDGET-ID 44
     RECT-6 AT ROW 14.67 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96.6 BY 25.


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
         TITLE              = "G L  Account History"
         HEIGHT             = 25
         WIDTH              = 96.6
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
IF NOT C-Win:LOAD-ICON("Graphics\xRemove.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\xRemove.ico"
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
       begin_acct:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_acct:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_detailed:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_exc-auto:PRIVATE-DATA IN FRAME FRAME-A     = 
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

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* G L  Account History */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* G L  Account History */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_acct C-Win
ON LEAVE OF begin_acct IN FRAME FRAME-A /* Beginning Acct# */
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
           {custom/asifax.i &type= " "
                            &begin_cust= "begin_acct"
                            &END_cust= "begin_acct" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = " "
                             &begin_cust= "begin_acct"
                             &END_cust= "begin_acct"
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = " "
                                  &begin_cust="begin_acct"
                                  &END_cust="begin_acct"
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

  RUN DisplaySelectionDefault.  /* task 04141407 */ 
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


&Scoped-define SELF-NAME end_acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_acct C-Win
ON LEAVE OF end_acct IN FRAME FRAME-A /* Ending Acct# */
DO:
  assign {&self-name}.
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


&Scoped-define SELF-NAME tb_detailed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_detailed C-Win
ON VALUE-CHANGED OF tb_detailed IN FRAME FRAME-A /* Detailed? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_exc-auto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_exc-auto C-Win
ON VALUE-CHANGED OF tb_exc-auto IN FRAME FRAME-A /* Exclude Auto Distributions? */
DO:
  ASSIGN {&self-name}.
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

  assign
   begin_date = date(1,1,year(today))
   end_date   = today.
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
  DISPLAY begin_date end_date begin_acct end_acct tb_exc-auto tb_detailed 
          sl_avail sl_selected rd-dest lv-ornt lines-per-page lv-font-no 
          lv-font-name td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_date end_date begin_acct end_acct tb_exc-auto 
         tb_detailed sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up 
         btn_down rd-dest lv-ornt lines-per-page lv-font-no td-show-parm 
         tb_excel tb_runExcel fi_file btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE excel-acct-proc C-Win 
PROCEDURE excel-acct-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-open-amt AS DEC NO-UNDO.
  
  DEF VAR i AS INT NO-UNDO.
  
  PUT STREAM excel UNFORMATTED
      '"' account.actnum                         '",'
      '"' account.dscr                           '",'.

  DO i = 1 TO 7:
     PUT STREAM excel UNFORMATTED
         '"' ""  '",'.
  END.

  PUT STREAM excel UNFORMATTED    
      '"' STRING(ip-open-amt,"->>>,>>>,>>>,>>9.99") '",'
      '"' ""                                        '",'
      SKIP.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE excel-det-proc C-Win 
PROCEDURE excel-det-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-tmp-dscr AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-tr-num AS INT NO-UNDO.
   DEFINE INPUT PARAMETER ip-jrnl AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-date AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER ip-amt AS DEC NO-UNDO.
   
   DEF VAR v-ref1 AS CHAR NO-UNDO.
   DEF VAR v-ref2 AS CHAR NO-UNDO.
   DEF VAR v-ref3 AS CHAR NO-UNDO.
   DEF VAR inv-index AS INT NO-UNDO.

   ASSIGN
     v-ref1 = ip-tmp-dscr
     v-ref2 = ""
     v-ref3 = ""
     inv-index = INDEX(v-ref1,"Inv#").

   IF inv-index > 0 THEN
   DO:
      ASSIGN
        v-ref1 = SUBSTRING(v-ref1,1,inv-index - 1)
        v-ref2 = SUBSTRING(ip-tmp-dscr,inv-index,13)
        v-ref3 = SUBSTRING(ip-tmp-dscr,inv-index + 13).
   END.

   PUT STREAM excel UNFORMATTED
       '"' ""                                      '",'
       '"' ""                                      '",'
       '"' STRING(ip-tr-num,"9999999")             '",'
       '"' ip-jrnl                                 '",'
       '"' v-ref1                                  '",'
       '"' v-ref2                                  '",'
       '"' v-ref3                                  '",'
       '"' ip-date                                 '",'
       '"' STRING(ip-amt,"(>>,>>>,>>9.99)")        '",'
       '"' ""                                      '",'
       '"' ""                                      '",'
       SKIP.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE excel-total-proc C-Win 
PROCEDURE excel-total-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-text     AS CHAR NO-UNDO.
  DEFINE INPUT PARAMETER ip-tot-act  AS DEC NO-UNDO.
  DEFINE INPUT PARAMETER ip-open-amt AS DEC NO-UNDO.
  
  DEF VAR i AS INT NO-UNDO.

  DO i = 1 TO 8:
     PUT STREAM excel UNFORMATTED
         '"' "" '",'.
  END.
  
  IF ip-text = "" THEN
     PUT STREAM excel UNFORMATTED
         '"' STRING(ip-tot-act,"->>>,>>>,>>>,>>9.99")               '",'
         '"' STRING(ip-tot-act + ip-open-amt,"->>>,>>>,>>>,>>9.99") '",'
         '"' "*"                                                    '",'
      SKIP(1).
  ELSE
     PUT STREAM excel UNFORMATTED
         '"' "TOTAL"                                  '",'
         '"' STRING(ip-tot-act,"->>>,>>>,>>>,>>9.99") '",'
      SKIP(1).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetTransDesc C-Win 
PROCEDURE GetTransDesc :
/*------------------------------------------------------------------------------
  Purpose:  Find an ap-invl description for an ACPAY journal entry
    Notes:  Task 07311302
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcDesc AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipdAmt AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opcDesc AS CHARACTER NO-UNDO.

DEFINE VARIABLE cVendor AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cVendNo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDate AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iLen AS INTEGER     NO-UNDO.

DEFINE BUFFER bf-ap-inv FOR ap-inv.
DEFINE BUFFER bf-ap-invl FOR ap-invl.
DEFINE BUFFER bf-vend FOR vend.


IF ipcDesc NE "" THEN DO:
    cVendor = ipcDesc.
    iLen = LENGTH(cVendor).
    cDate = TRIM(SUBSTRING(cVendor,iLen - 8 , 9)).
    cVendor = TRIM(SUBSTRING(cVendor,1,INDEX(cVendor,cDate) - 1)).
    
    FIND FIRST bf-vend 
        WHERE bf-vend.company EQ cocode
          AND bf-vend.NAME = cVendor 
        NO-LOCK NO-ERROR.
    IF AVAIL bf-vend THEN cVendNo = bf-vend.vend-no.
    
    FOR EACH bf-ap-inv 
        WHERE bf-ap-inv.company EQ cocode
          AND bf-ap-inv.vend-no = cVendNo
          AND bf-ap-inv.inv-date = DATE(cDate) NO-LOCK:
        FIND FIRST bf-ap-invl WHERE bf-ap-invl.i-no EQ bf-ap-inv.i-no
            AND bf-ap-invl.amt = ipdAmt
            NO-LOCK NO-ERROR.
        IF AVAIL bf-ap-invl THEN DO:
            opcDesc = bf-ap-invl.dscr.
            LEAVE.
        END.
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
/* -------------------------------------------------- gl/gl-hist.p 01/98 FWK  */
/* GL History Report by Date and Account                                      */
/* -------------------------------------------------------------------------- */

/*{sys/form/r-top3lw.f}*/

DEF VAR cDisplay AS cha NO-UNDO.
DEF VAR cExcelDisplay AS cha NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR cTmpField AS CHA NO-UNDO.
DEF VAR cVarValue AS cha NO-UNDO.
DEF VAR cExcelVarValue AS cha NO-UNDO.
DEF VAR cSelectedList AS cha NO-UNDO.
DEF VAR cFieldName AS cha NO-UNDO.
DEF BUFFER bitemfg FOR itemfg.
DEF VAR str-tit4 AS cha FORM "x(200)" NO-UNDO.
DEF VAR str-tit5 AS cha FORM "x(200)" NO-UNDO.

{sys/form/r-top5L3.f} 

cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

def var save_id as recid.
def var time_stamp as ch NO-UNDO.
def var tot-all  as dec format "->>>,>>>,>>>,>>9.99" NO-UNDO.
def var tot-tx   like tot-all NO-UNDO.
def var tot-act  like tot-all NO-UNDO.
def var open-amt like tot-all NO-UNDO.
def var pri-amt  like tot-all NO-UNDO.
def var net-inc  as DEC NO-UNDO.
def var tmp-amt like gltrans.tr-amt NO-UNDO.
def var tmp-dscr AS CHAR FORMAT "X(54)" NO-UNDO.
def var ap-dscr AS CHAR FORMAT "X(30)" NO-UNDO.
def buffer xgltrans for gltrans.
def buffer xglhist for glhist.
def buffer xperiod for period.
/*def var str-tit4 as char no-undo.
def var str-tit5 as char no-undo.*/
def var v-s-date as date format "99/99/9999" init 01/01/01 NO-UNDO.
def var v-e-date as date format "99/99/9999" init today NO-UNDO.
def var v-s-yr like period.yr NO-UNDO.
def var v-e-yr like period.yr NO-UNDO.
def var v-sumdet as LOG NO-UNDO.
def var v-answer as LOG NO-UNDO.
DEF VAR li-dscr AS INT NO-UNDO.
DEF VAR ld-per-start AS DATE NO-UNDO.

DEF VAR tacct LIKE gltrans.actnum  LABEL "    To Account Number" NO-UNDO.
DEF VAR facct LIKE gltrans.actnum  LABEL "  From Account Number" NO-UNDO.
DEF VAR op AS CHAR FORMAT "x" INITIAL "S" LABEL "  S)ummary or D)etail?" NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.
DEF VAR acct-hdr-printed AS LOG NO-UNDO.

form  account.actnum format "x(75)" open-amt to 131
      with frame r-cmon down stream-io width 132 no-labels no-box no-underline.
form  account.actnum format "x(230)" 
      with frame r-cmon2 down stream-io width 230 no-labels no-box no-underline.


assign
 str-tit2 = c-win:title 
 {sys/inc/ctrtext.i str-tit2 126}

 facct    = begin_acct
 tacct    = end_acct
 v-s-date = begin_date
 v-e-date = end_date
 v-sumdet = not tb_detailed
    
 str-tit3 = "Date Range: " + STRING(v-s-date,"99/99/9999") + "-" +
                             STRING(v-e-date,"99/99/9999")
 {sys/inc/ctrtext.i str-tit3 146}
 /*hdr-tit  = "Account Number             Journal  Reference                                             Date         Amount                              Balance"
 hdr-tit2 = "                   Run #"
 hdr-tit3 = fill("-",146)*/
 uperiod = period
 udate   = today.

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
   /*lCountedDateSelected = IF lCountedDateSelected = NO AND ttRptSelected.FieldList = "v-counted-date" THEN YES ELSE NO.
   */    
 END.

       
{sys/inc/print1.i}

find first period where period.company = cocode and
                        period.pst <= v-s-date and
                        period.pend >= v-s-date no-lock no-error.
if avail period then
  assign uperiod = period.pnum
         v-s-yr = period.yr
         ld-per-start = period.pst.

find first period where period.company = cocode and
                        period.pst <= v-e-date and
                        period.pend >= v-e-date no-lock no-error.
if avail period then
   v-e-yr = period.yr.
         
{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  /*excelheader = "Account Number,Account Description,Run #,Journal,"
              + "Reference, , ,Date,Amount,Balance, ".*/
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

   DISPLAY "" WITH FRAME r-top.

   for each account where account.company = cocode and
                          account.actnum ge facct and
                          account.actnum le tacct no-lock:

       {custom/statusMsg.i " 'Processing Account #  '  + string(account.actnum) "}

      if line-counter > page-size - 2 then page.

      acct-hdr-printed = NO.

      run gl/gl-open1.p (recid(account), v-s-yr, v-s-date, uperiod,
                         output open-amt).

      FOR EACH glhist fields(tr-amt) NO-LOCK
          WHERE glhist.company EQ account.company
            AND glhist.actnum  EQ account.actnum
            AND glhist.tr-date >= ld-per-start
            AND glhist.tr-date < v-s-date
            AND (glhist.jrnl   NE "AUTODIST" OR NOT tb_exc-auto):
        open-amt = open-amt + glhist.tr-amt.
      END.

      FOR EACH gltrans fields(tr-amt) NO-LOCK
          WHERE gltrans.company EQ account.company
            AND gltrans.actnum  EQ account.actnum
            AND gltrans.tr-date >= ld-per-start
            AND gltrans.tr-date < v-s-date
            AND (gltrans.jrnl   NE "AUTODIST" OR NOT tb_exc-auto):
        open-amt = open-amt + gltrans.tr-amt.
      END.

   /*   display string(account.actnum) + "  " + account.dscr format "x(75)" @
              account.actnum
              open-amt with frame r-cmon.
      down.*/

      ASSIGN cDisplay = ""
          cTmpField = ""
          cVarValue = ""
          cExcelDisplay = ""
          cExcelVarValue = ""
          .

       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
       
            CASE cTmpField:               
                 WHEN "actnum" THEN cVarValue = string(account.actnum) . 
                 WHEN "actdesc" THEN cVarValue =  string(account.dscr) .
                 WHEN "ap-desc" THEN cVarValue =  "" .
                 WHEN "run" THEN cVarValue = /*STRING(v-cust-no)*/ "".
                 WHEN "jour" THEN cVarValue = /*IF AVAIL cust THEN STRING(cust.name) ELSE*/ "".
                 WHEN "ref" THEN cVarValue = /*STRING(w-data.inv-no)*/ "".
                 WHEN "date" THEN cVarValue = /* string(w-data.i-no)*/ "" .
                 WHEN "amt" THEN cVarValue = /*IF AVAIL itemfg THEN STRING(itemfg.procat) ELSE*/ "".
                 WHEN "bal" THEN cVarValue =  STRING(open-amt,"->>,>>>,>>>,>>>.99") .
                     
            END CASE.
            cExcelVarValue = cVarValue.  
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
       
   END.
   /*PUT UNFORMATTED cDisplay SKIP.*/
   display string(cDisplay) format "x(230)" @
              account.actnum
               with frame r-cmon2.
   DOWN.
  
   IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
   END. 
  
 
      tot-all = tot-all + open-amt.

    if not v-sumdet then do:
        
      FOR EACH glhist NO-LOCK
          WHERE glhist.company EQ account.company
            AND glhist.actnum  EQ account.actnum and
                                        glhist.tr-date >= v-s-date and
                                        glhist.tr-date <= v-e-date and
                                        (glhist.jrnl NE "AUTODIST" or
                                         NOT tb_exc-auto)
      break by glhist.tr-date by glhist.jrnl by glhist.tr-num:

           {custom/statusMsg.i " 'Processing Run #  '  + string(glhist.tr-num) "}

         tmp-dscr = "".
         ap-dscr = "".
         /*IF glhist.jrnl = "ACPAY" AND tb_desc THEN DO:*/
            RUN GetTransDesc(INPUT glhist.tr-dscr,
                             INPUT glhist.tr-amt,
                             OUTPUT ap-dscr).  
        /*END.
        IF tmp-dscr EQ "" THEN*/
            assign tmp-dscr = trim(glhist.tr-dscr).

         IF glhist.jrnl EQ "MCSHREC" THEN DO:
           RELEASE ar-mcash.
           li-dscr = INT(tmp-dscr) NO-ERROR.
           IF NOT ERROR-STATUS:ERROR THEN
           FIND FIRST ar-mcash WHERE ar-mcash.m-no EQ li-dscr NO-LOCK NO-ERROR.
           IF AVAIL ar-mcash THEN tmp-dscr = ar-mcash.payer + " Rec# " + tmp-dscr.
         END.

         /*put space(19)
             glhist.tr-num format "9999999" space(1)
             glhist.jrnl space(1)
             tmp-dscr FORMAT "X(52)"
             glhist.tr-date FORMAT "99/99/99" SPACE(1)
             glhist.tr-amt FORMAT "(>>,>>>,>>9.99)" skip.*/
         ASSIGN cDisplay = ""
          cTmpField = ""
          cVarValue = ""
          cExcelDisplay = ""
          cExcelVarValue = ""
          .

       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
       
            CASE cTmpField:               
                 WHEN "actnum" THEN cVarValue = "" . 
                 WHEN "actdesc" THEN cVarValue =  "" .
                 WHEN "ap-desc" THEN cVarValue =  STRING(ap-dscr,"x(30)") .
                 WHEN "run" THEN cVarValue = STRING(glhist.tr-num,"99999999") .
                 WHEN "jour" THEN cVarValue = STRING(glhist.jrnl) .
                 WHEN "ref" THEN cVarValue = STRING(tmp-dscr,"x(52)").
                 WHEN "date" THEN cVarValue = string(glhist.tr-date,"99/99/99") .
                 WHEN "amt" THEN cVarValue = STRING(glhist.tr-amt,"(>>,>>>,>>9.99)") .
                 WHEN "bal" THEN cVarValue =  "" .
                    
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

        /* IF tb_excel THEN
         DO:
            IF NOT acct-hdr-printed THEN
            DO:
               acct-hdr-printed = YES.
               RUN excel-acct-proc(INPUT open-amt).
            END.

            RUN excel-det-proc(INPUT tmp-dscr,
                               INPUT glhist.tr-num,
                               INPUT glhist.jrnl,
                               INPUT glhist.tr-date,
                               INPUT glhist.tr-amt).
         END.*/

         ASSIGN
            tot-all  = tot-all  + glhist.tr-amt
            tot-tx   = tot-tx   + glhist.tr-amt
            tot-act  = tot-act  + glhist.tr-amt.
      end.
      FOR EACH gltrans NO-LOCK
          WHERE gltrans.company EQ account.company
            AND gltrans.actnum  EQ account.actnum and
                                        gltrans.tr-date >= v-s-date and
                                        gltrans.tr-date <= v-e-date and
                                        (gltrans.jrnl NE "AUTODIST" or
                                         NOT tb_exc-auto)
        break by gltrans.tr-date by gltrans.jrnl by gltrans.trnum:

          {custom/statusMsg.i " 'Processing Run #  '  + string(gltrans.trnum) "}

        tmp-dscr = "".
        ap-dscr = "".
        /*IF gltrans.jrnl = "ACPAY" AND tb_desc THEN DO:*/
            RUN GetTransDesc(INPUT gltrans.tr-dscr,
                             INPUT gltrans.tr-amt,
                             OUTPUT ap-dscr). 
       /* END.
        IF tmp-dscr EQ "" THEN*/
            assign tmp-dscr = trim(gltrans.tr-dscr).

         IF gltrans.jrnl EQ "MCSHREC" THEN DO:
           RELEASE ar-mcash.
           li-dscr = INT(tmp-dscr) NO-ERROR.
           IF NOT ERROR-STATUS:ERROR THEN
           FIND FIRST ar-mcash WHERE ar-mcash.m-no EQ li-dscr NO-LOCK NO-ERROR.
           IF AVAIL ar-mcash THEN tmp-dscr = ar-mcash.payer + " Rec# " + tmp-dscr.
         END.

         /*put space(19)
             gltrans.trnum format "9999999" space(1)
             gltrans.jrnl space(1)
             tmp-dscr FORMAT "X(52)"
             gltrans.tr-date FORMAT "99/99/99" SPACE(1)
             gltrans.tr-amt FORMAT "(>>,>>>,>>9.99)" skip.*/

          ASSIGN cDisplay = ""
          cTmpField = ""
          cVarValue = ""
          cExcelDisplay = ""
          cExcelVarValue = ""
          .

       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
       
            CASE cTmpField:               
                 WHEN "actnum" THEN cVarValue = "" . 
                 WHEN "actdesc" THEN cVarValue =  "" .
                 WHEN "ap-desc" THEN cVarValue =  STRING(ap-dscr,"x(30)") .
                 WHEN "run" THEN cVarValue = STRING(gltrans.trnum,"99999999") .
                 WHEN "jour" THEN cVarValue = STRING(gltrans.jrnl) .
                 WHEN "ref" THEN cVarValue = STRING(tmp-dscr,"x(52)").
                 WHEN "date" THEN cVarValue = string(gltrans.tr-date,"99/99/99") .
                 WHEN "amt" THEN cVarValue = STRING(gltrans.tr-amt,"(>>,>>>,>>9.99)") .
                 WHEN "bal" THEN cVarValue =  "" .
                 
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

        /* IF tb_excel THEN
         DO:
            IF NOT acct-hdr-printed THEN
            DO:
               acct-hdr-printed = YES.
               RUN excel-acct-proc(INPUT open-amt).
            END.

            RUN excel-det-proc(INPUT tmp-dscr,
                               INPUT gltrans.trnum,
                               INPUT gltrans.jrnl,
                               INPUT gltrans.tr-date,
                               INPUT gltrans.tr-amt).
         END.*/

         ASSIGN
            tot-all  = tot-all  + gltrans.tr-amt
            tot-tx   = tot-tx   + gltrans.tr-amt
            tot-act  = tot-act  + gltrans.tr-amt.
      end.
    end.
    else
    do:
        
      FOR EACH glhist NO-LOCK
          WHERE glhist.company EQ account.company
            AND glhist.actnum  EQ account.actnum and
                                        glhist.tr-date >= v-s-date and
                                        glhist.tr-date <= v-e-date and
                                        (glhist.jrnl NE "AUTODIST" or
                                         NOT tb_exc-auto)
        break by glhist.tr-date by glhist.jrnl by glhist.tr-num:

           {custom/statusMsg.i " 'Processing Run #  '  + string(glhist.tr-num) "}

        if line-counter > page-size - 2 then page.
        if last-of(glhist.tr-num) then
        do:
        assign tmp-amt = 0.
        for each xglhist FIELDS(tr-amt) WHERE
            xglhist.company = glhist.company and
            xglhist.actnum = glhist.actnum and
            xglhist.period = glhist.period and
            xglhist.tr-date = glhist.tr-date and
            xglhist.tr-num = glhist.tr-num and
            xglhist.jrnl = glhist.jrnl no-lock:

            assign tmp-amt = tmp-amt + xglhist.tr-amt.
        end.

        if glhist.jrnl = "CASHR" then
          assign tmp-dscr = "CASH RECEIPTS                           ".
        else if glhist.jrnl = "APCKR" then
          assign tmp-dscr = "ACCOUNTS PAYABLE CHECK REGISTER         ".
        else if glhist.jrnl = "GENERAL" then
          assign tmp-dscr = "GENERAL                                 ".
        else if glhist.jrnl = "OEINV" then
          assign tmp-dscr = "ORDER ENTRY INVOICE                     ".
        else if glhist.jrnl = "ARINV" then
          assign tmp-dscr = "ACCOUNTS RECEIVABLE INVOICE             ".
        else if glhist.jrnl = "MCSHREC" then
          assign tmp-dscr = "MISC CASH RECEIPTS                      ".
        else if glhist.jrnl = "CDISB" then
          assign tmp-dscr = "CASH DISBURSEMENT                       ".
        else if glhist.jrnl = "APMEM" then
          assign tmp-dscr = "ACCOUNTS PAYABLE MEMO                   ".
        else if glhist.jrnl = "CRMEM" then
          assign tmp-dscr = "CREDIT MEMO                             ".
        else if glhist.jrnl = "ACPAY" THEN DO:
            RUN GetTransDesc(INPUT glhist.tr-dscr,
                             INPUT glhist.tr-amt,
                             OUTPUT tmp-dscr). 
            IF tmp-dscr EQ "" THEN
                assign tmp-dscr = "ACCOUNTS PAYABLE                        ".
        END.
        else if glhist.jrnl = "APVOIDCK" then
          assign tmp-dscr = "ACCOUNTS PAYABLE VOID CHECK             ".
        else if glhist.jrnl = "ADJUST" then
          assign tmp-dscr = "ADJUSTMENT                              ".
        else if glhist.jrnl = "AUTODIST" then
          assign tmp-dscr = "AUTOMATIC DISTRIBUTION                  ".
        else
          assign tmp-dscr = "                                        ".
          ap-dscr = "" .
          RUN GetTransDesc(INPUT glhist.tr-dscr,
                             INPUT glhist.tr-amt,
                             OUTPUT ap-dscr). 
         
        /* put space(19)
                glhist.tr-num format "9999999" space(1)
                glhist.jrnl space(1)
                tmp-dscr FORMAT "X(52)"
                glhist.tr-date FORMAT "99/99/99" SPACE(1)
                tmp-amt FORMAT "(>>,>>>,>>9.99)" skip.*/
           ASSIGN cDisplay = ""
          cTmpField = ""
          cVarValue = ""
          cExcelDisplay = ""
          cExcelVarValue = ""
          .

       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
       
            CASE cTmpField:               
                 WHEN "actnum" THEN cVarValue = "" . 
                 WHEN "actdesc" THEN cVarValue =  "" .
                 WHEN "ap-desc" THEN cVarValue =  STRING(ap-dscr,"x(30)") .
                 WHEN "run" THEN cVarValue = STRING(glhist.tr-num,"99999999") .
                 WHEN "jour" THEN cVarValue = STRING(glhist.jrnl) .
                 WHEN "ref" THEN cVarValue = STRING(tmp-dscr,"x(52)").
                 WHEN "date" THEN cVarValue = string(glhist.tr-date,"99/99/99") .
                 WHEN "amt" THEN cVarValue = STRING(tmp-amt,"(>>,>>>,>>9.99)") .
                 WHEN "bal" THEN cVarValue =  "" .
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

        /* IF tb_excel THEN
         DO:
            IF NOT acct-hdr-printed THEN
               DO:
                  acct-hdr-printed = YES.
                  RUN excel-acct-proc(INPUT open-amt).
               END.
           
            RUN excel-det-proc(INPUT tmp-dscr,
                               INPUT glhist.tr-num,
                               INPUT glhist.jrnl,
                               INPUT glhist.tr-date,
                               INPUT tmp-amt).
         END.*/
        end.
        
        ASSIGN
           tot-all  = tot-all  + glhist.tr-amt
           tot-tx   = tot-tx   + glhist.tr-amt
           tot-act  = tot-act  + glhist.tr-amt.
      end. /* each glhist */

      FOR EACH gltrans NO-LOCK
          WHERE gltrans.company EQ account.company
            AND gltrans.actnum  EQ account.actnum and
                                        gltrans.tr-date >= v-s-date and
                                        gltrans.tr-date <= v-e-date and
                                        (gltrans.jrnl NE "AUTODIST" or
                                         NOT tb_exc-auto)
      break by gltrans.tr-date by gltrans.jrnl by gltrans.trnum:

          {custom/statusMsg.i " 'Processing Run #  '  + string(gltrans.trnum) "}

        if line-counter > page-size - 2 then page.
        if last-of(gltrans.trnum) then
        do:
        assign tmp-amt = 0.
        for each xgltrans FIELDS(tr-amt)
            where xgltrans.company = cocode and
                                xgltrans.actnum = gltrans.actnum and
                                xgltrans.period = gltrans.period and
                                xgltrans.tr-date = gltrans.tr-date and
                                xgltrans.trnum = gltrans.trnum and
                                xgltrans.jrnl = gltrans.jrnl no-lock:

          assign tmp-amt = tmp-amt + xgltrans.tr-amt.
        end.

        if gltrans.jrnl = "CASHR" then
          assign tmp-dscr = "CASH RECEIPTS                           ".
        else if gltrans.jrnl = "APCKR" then
          assign tmp-dscr = "ACCOUNTS PAYABLE CHECK REGISTER         ".
        else if gltrans.jrnl = "GENERAL" then
          assign tmp-dscr = "GENERAL                                 ".
        else if gltrans.jrnl = "OEINV" then
          assign tmp-dscr = "ORDER ENTRY INVOICE                     ".
        else if gltrans.jrnl = "ARINV" then
          assign tmp-dscr = "ACCOUNTS RECEIVABLE INVOICE             ".
        else if gltrans.jrnl = "MCSHREC" then
          assign tmp-dscr = "MISC CASH RECEIPTS                      ".
        else if gltrans.jrnl = "CDISB" then
          assign tmp-dscr = "CASH DISBURSEMENT                       ".
        else if gltrans.jrnl = "APMEM" then
          assign tmp-dscr = "ACCOUNTS PAYABLE MEMO                   ".
        else if gltrans.jrnl = "CRMEM" then
          assign tmp-dscr = "CREDIT MEMO                             ".
        else if gltrans.jrnl = "ACPAY" THEN DO:
            RUN GetTransDesc(INPUT gltrans.tr-dscr,
                             INPUT gltrans.tr-amt,
                             OUTPUT tmp-dscr).
            IF tmp-dscr EQ "" THEN
                assign tmp-dscr = "ACCOUNTS PAYABLE                        ".
        END.
        else if gltrans.jrnl = "APVOIDCK" then
          assign tmp-dscr = "ACCOUNTS PAYABLE VOID CHECK             ".
        else if gltrans.jrnl = "ADJUST" then
          assign tmp-dscr = "ADJUSTMENT                              ".
        else if gltrans.jrnl = "AUTODIST" then
          assign tmp-dscr = "AUTOMATIC DISTRIBUTION                  ".
        else
          assign tmp-dscr = "                                        ".

          ap-dscr = "".
           RUN GetTransDesc(INPUT gltrans.tr-dscr,
                             INPUT gltrans.tr-amt,
                             OUTPUT ap-dscr).
        
        /*put space(19)
            gltrans.trnum format "9999999" space(1)
            gltrans.jrnl space(1)
            tmp-dscr FORMAT "X(52)"
            gltrans.tr-date FORMAT "99/99/99" SPACE(1)
            tmp-amt FORMAT "(>>,>>>,>>9.99)" skip.*/

           ASSIGN cDisplay = ""
          cTmpField = ""
          cVarValue = ""
          cExcelDisplay = ""
          cExcelVarValue = ""
          .

       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
       
            CASE cTmpField:               
                 WHEN "actnum" THEN cVarValue = "" . 
                 WHEN "actdesc" THEN cVarValue =  "" .
                 WHEN "ap-desc" THEN cVarValue =  STRING(ap-dscr,"x(30)") .
                 WHEN "run" THEN cVarValue = STRING(gltrans.trnum,"99999999") .
                 WHEN "jour" THEN cVarValue = STRING(gltrans.jrnl) .
                 WHEN "ref" THEN cVarValue = STRING(tmp-dscr,"x(52)").
                 WHEN "date" THEN cVarValue = string(gltrans.tr-date,"99/99/99") .
                 WHEN "amt" THEN cVarValue = STRING(tmp-amt,"(>>,>>>,>>9.99)") .
                 WHEN "bal" THEN cVarValue =  "" .
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

       /* IF tb_excel THEN
        DO:
           IF NOT acct-hdr-printed THEN
              DO:
                 acct-hdr-printed = YES.
                 RUN excel-acct-proc(INPUT open-amt).
              END.

           RUN excel-det-proc(INPUT tmp-dscr,
                              INPUT gltrans.trnum,
                              INPUT gltrans.jrnl,
                              INPUT gltrans.tr-date,
                              INPUT tmp-amt).
        END.*/
        end.

         ASSIGN
            tot-all  = tot-all  + gltrans.tr-amt
            tot-tx   = tot-tx   + gltrans.tr-amt
            tot-act  = tot-act  + gltrans.tr-amt.
      end. /* each gltrans */
    end.

  /*  IF tb_excel AND NOT acct-hdr-printed AND open-amt NE 0 THEN
       RUN excel-acct-proc(INPUT open-amt). */

    if tot-act ne 0 then
    DO:
    /*  put tot-act to 111
          tot-act + open-amt format "->>>,>>>,>>>,>>9.99" to 131 " *" skip(1).

      IF tb_excel THEN
         RUN excel-total-proc(INPUT "", INPUT tot-act, INPUT open-amt).*/
         ASSIGN cDisplay = ""
          cTmpField = ""
          cVarValue = ""
          cExcelDisplay = ""
          cExcelVarValue = ""
          .

       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
       
            CASE cTmpField:               
                 WHEN "actnum" THEN cVarValue = "" . 
                 WHEN "actdesc" THEN cVarValue =  "" .
                 WHEN "ap-desc" THEN cVarValue =  "" .
                 WHEN "run" THEN cVarValue = "" .
                 WHEN "jour" THEN cVarValue = "" .
                 WHEN "ref" THEN cVarValue = "" .
                 WHEN "date" THEN cVarValue = "" .
                 WHEN "amt" THEN cVarValue = STRING(tot-act,"->>,>>>,>>9.99") .
                 WHEN "bal" THEN cVarValue =  STRING(tot-act + open-amt,"->>,>>>,>>>,>>9.99") + "*" .
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
    else
    if open-amt ne 0 THEN
       put skip(1).

    down.
    tot-act = 0.
   end. /* each account */


  /* display "TOTAL" to 95

           tot-all to 60
           with frame r-cmon3 no-labels no-box stream-io width 146. */

   ASSIGN cDisplay = ""
          cTmpField = ""
          cVarValue = ""
          cExcelDisplay = ""
          cExcelVarValue = ""
          .

       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
       
            CASE cTmpField:               
                 WHEN "actnum" THEN cVarValue = "" . 
                 WHEN "actdesc" THEN cVarValue =  "" .
                 WHEN "ap-desc" THEN cVarValue =  "" .
                 WHEN "run" THEN cVarValue = "" .
                 WHEN "jour" THEN cVarValue = "" .
                 WHEN "ref" THEN cVarValue = "" .
                 WHEN "date" THEN cVarValue = "" .
                 WHEN "amt" THEN cVarValue = "" .
                 WHEN "bal" THEN cVarValue =  STRING(tot-all,"->>,>>>,>>>,>>9.99") + "*" .
            END CASE.
            cExcelVarValue = cVarValue.  
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
       
   END.
   PUT UNFORMATTED "    Total  " substring(cDisplay,12,300) SKIP.
   IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
               "TOTALS" + substring(cExcelDisplay,3,300) SKIP.
   END.

IF tb_excel THEN DO:

 /* RUN excel-total-proc(INPUT "TOTAL", INPUT tot-all, INPUT 0). */

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

