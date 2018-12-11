&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: cerep\r-est.w

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

DEF TEMP-TABLE tt-eb  FIELD m-code LIKE est-op.m-code
                      FIELD row-id AS ROWID
                      INDEX m-code m-code.

DEFINE VARIABLE ls-fax-file AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL NO-UNDO.

/* gdm - 10130804 */
DEF STREAM excel.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO. 
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.

ASSIGN cTextListToSelect  = "Est#,Customer Name,Last used,Part #,Description 1,Description 2,Style," +
                            "Blank size,Item Size,Print,Board,Status,Last Order#,Order Date,Cust#,Sales Rep,Commission %," +
                            "Adder1,Adder2,Adder3,Adder4,Adder5,F/G item,Job#"
       cFieldListToSelect = "v_est-no,v_custnm,v_mod-date,eb.part-no,eb.part-dscr1,eb.part-dscr2,v_stydsc," +
                            "v_blksz,v-blk-dim,eb.i-coldscr,v_i-name,v-booked,eb.ord-no,ord-date,eb.cust-no,eb.sman,comm," +
                            "add1,add2,add3,add4,add5,fg-item,job-no"

       cFieldLength = "5,30,10,15,30,30,25," + "20,27,30,30,6,11,10,8,9,12," + "10,10,10,10,10,15,9"
       .

{sys/inc/ttRptSel.i}
 ASSIGN cTextListToDefault  = "Status,Last used,Est#,Customer Name,Part #,Description 1,Description 2,Style," +
                              "Blank size,Item Size,Print,Board".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_cust-no end_cust-no ~
begin_slsmn end_slsmn begin_est end_est begin_date end_date begin_date-2 ~
end_date-2 begin_mach end_mach tb_break tb_booked rd_book tb_not-booked ~
tb_sort Btn_Def sl_selected sl_avail Btn_Add Btn_Remove btn_Up btn_down ~
rd-dest lines-per-page lv-ornt lv-font-no td-show-parm tb_excel tb_runExcel ~
fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no begin_slsmn ~
end_slsmn begin_est end_est begin_date end_date begin_date-2 end_date-2 ~
begin_mach end_mach tb_break tb_booked lbl-sort rd_book lbl-booked ~
tb_not-booked tb_sort sl_selected sl_avail rd-dest lines-per-page lv-ornt ~
lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file 

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

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Beginning Add Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_date-2 AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Beginning Mod Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_est AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Estimate#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_mach AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Machine#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" 
     LABEL "Beginning Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     LABEL "Ending Add Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date-2 AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     LABEL "Ending Mod Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_est AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Estimate#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_mach AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Machine#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
     LABEL "Ending Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-est.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43.6 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl-booked AS CHARACTER FORMAT "X(256)":U INITIAL "Show?" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE lbl-sort AS CHARACTER FORMAT "X(256)":U INITIAL "Booked?" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .95 NO-UNDO.

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

DEFINE VARIABLE rd_book AS CHARACTER INITIAL "Order" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Order Only", "Order",
"Job Only", "Job",
"Both", "Both"
     SIZE 34.8 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.14.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 10.71.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 34 BY 5.14 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 34 BY 5.14 NO-UNDO.

DEFINE VARIABLE tb_booked AS LOGICAL INITIAL yes 
     LABEL "Booked" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .71 NO-UNDO.

DEFINE VARIABLE tb_break AS LOGICAL INITIAL no 
     LABEL "Page Break by Machine#?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL no 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95 NO-UNDO.

DEFINE VARIABLE tb_not-booked AS LOGICAL INITIAL yes 
     LABEL "Not Booked" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY 1.05 NO-UNDO.

DEFINE VARIABLE tb_sort AS LOGICAL INITIAL no 
     LABEL "Sort by Customer Part#?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust-no AT ROW 2.43 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 2.43 COL 69 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_slsmn AT ROW 3.38 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number"
     end_slsmn AT ROW 3.38 COL 69 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number"
     begin_est AT ROW 4.33 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Estimate"
     end_est AT ROW 4.33 COL 69 COLON-ALIGNED HELP
          "Enter Ending Estimate"
     begin_date AT ROW 5.29 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 5.29 COL 69 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_date-2 AT ROW 6.24 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date-2 AT ROW 6.24 COL 69 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_mach AT ROW 7.19 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_mach AT ROW 7.19 COL 69 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     tb_break AT ROW 8.29 COL 23.8
     tb_booked AT ROW 9.24 COL 23.8
     lbl-sort AT ROW 9.38 COL 46.8 COLON-ALIGNED NO-LABEL WIDGET-ID 58
     rd_book AT ROW 9.38 COL 58.8 NO-LABEL WIDGET-ID 60
     lbl-booked AT ROW 9.48 COL 12.8 COLON-ALIGNED NO-LABEL
     tb_not-booked AT ROW 9.95 COL 23.8
     tb_sort AT ROW 10.67 COL 23.8
     Btn_Def AT ROW 12.48 COL 41.2 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 12.48 COL 60 NO-LABEL WIDGET-ID 28
     sl_avail AT ROW 12.52 COL 4 NO-LABEL WIDGET-ID 26
     Btn_Add AT ROW 13.52 COL 41.2 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 14.52 COL 41.2 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 15.52 COL 41.2 WIDGET-ID 40
     btn_down AT ROW 16.62 COL 41.2 WIDGET-ID 42
     rd-dest AT ROW 18.71 COL 4 NO-LABEL
     lines-per-page AT ROW 19.43 COL 84 COLON-ALIGNED
     lv-ornt AT ROW 19.57 COL 31 NO-LABEL
     lv-font-no AT ROW 20.86 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 21.81 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 23.24 COL 30
     tb_excel AT ROW 24.48 COL 50.2 RIGHT-ALIGNED WIDGET-ID 4
     tb_runExcel AT ROW 24.48 COL 72.8 RIGHT-ALIGNED WIDGET-ID 6
     fi_file AT ROW 25.67 COL 28.2 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 2
     btn-ok AT ROW 27.33 COL 26
     btn-cancel AT ROW 27.33 COL 56
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 11.81 COL 60 WIDGET-ID 44
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 11.81 COL 4.4 WIDGET-ID 38
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 18 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 17.91 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 94.4 BY 28.


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
         TITLE              = "Estimates List"
         HEIGHT             = 28.29
         WIDTH              = 96
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
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "Parm".

ASSIGN 
       begin_date-2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "Parm".

ASSIGN 
       begin_est:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "Parm".

ASSIGN 
       end_date-2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "Parm".

ASSIGN 
       end_est:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl-booked IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl-sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl-sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_book:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_booked:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_not-booked:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Estimates List */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Estimates List */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Add Date */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date-2 C-Win
ON LEAVE OF begin_date-2 IN FRAME FRAME-A /* Beginning Mod Date */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_est C-Win
ON HELP OF begin_est IN FRAME FRAME-A /* Beginning Estimate# */
DO:
     DEF VAR char-val AS cha NO-UNDO.
     DEF var lv-eb-tmpid as recid no-undo.

     run windows/l-est.w (g_company,g_loc,focus:screen-value, output char-val).

     if char-val <> "" then do:                 
            FIND FIRST eb WHERE string(RECID(eb)) = (char-val) NO-LOCK NO-ERROR.
            IF AVAIL eb THEN ASSIGN FOCUS:SCREEN-VALUE = eb.est-no
                                           lv-eb-tmpid = RECID(eb)    
                                begin_est:SCREEN-VALUE = eb.est-no.

            END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_est C-Win
ON LEAVE OF begin_est IN FRAME FRAME-A /* Beginning Estimate# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_mach C-Win
ON LEAVE OF begin_mach IN FRAME FRAME-A /* Beginning Machine# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slsmn C-Win
ON LEAVE OF begin_slsmn IN FRAME FRAME-A /* Beginning Sales Rep# */
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

  CASE rd-dest:
       WHEN 1 THEN RUN output-to-printer.
       WHEN 2 THEN RUN output-to-screen.
       WHEN 3 THEN RUN output-to-file.
       WHEN 4 THEN DO:
           /*run output-to-fax.*/
           {custom/asifax.i &type=" "
                            &begin_cust="begin_cust-no"
                            &end_cust="begin_cust-no" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       WHEN 5 THEN DO:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE=" "
                             &begin_cust="begin_cust-no"
                             &end_cust="begin_cust-no"
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE=" "
                                  &begin_cust="begin_cust-no"
                                  &end_cust="begin_cust-no"
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.
       END. 
       WHEN 6 THEN RUN OUTPUT-to-port.
  END CASE. 

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


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Add Date */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date-2 C-Win
ON LEAVE OF end_date-2 IN FRAME FRAME-A /* Ending Mod Date */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_est C-Win
ON HELP OF end_est IN FRAME FRAME-A /* Ending Estimate# */
DO:
     DEF VAR char-val AS cha NO-UNDO.
     DEF var lv-eb-tmpid as recid no-undo.

     run windows/l-est.w (g_company,g_loc,focus:screen-value, output char-val).

     if char-val <> "" then do:                 
            FIND FIRST eb WHERE string(RECID(eb)) = (char-val) NO-LOCK NO-ERROR.
            IF AVAIL eb THEN ASSIGN FOCUS:SCREEN-VALUE = eb.est-no
                                           lv-eb-tmpid = RECID(eb)    
                                  end_est:SCREEN-VALUE = eb.est-no.

            END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_est C-Win
ON LEAVE OF end_est IN FRAME FRAME-A /* Ending Estimate# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_mach C-Win
ON LEAVE OF end_mach IN FRAME FRAME-A /* Ending Machine# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slsmn C-Win
ON LEAVE OF end_slsmn IN FRAME FRAME-A /* Ending Sales Rep# */
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


&Scoped-define SELF-NAME rd_book
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_book C-Win
ON VALUE-CHANGED OF rd_book IN FRAME FRAME-A
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


&Scoped-define SELF-NAME tb_booked
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_booked C-Win
ON VALUE-CHANGED OF tb_booked IN FRAME FRAME-A /* Booked */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_break
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_break C-Win
ON VALUE-CHANGED OF tb_break IN FRAME FRAME-A /* Page Break by Machine#? */
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


&Scoped-define SELF-NAME tb_not-booked
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_not-booked C-Win
ON VALUE-CHANGED OF tb_not-booked IN FRAME FRAME-A /* Not Booked */
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


&Scoped-define SELF-NAME tb_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sort C-Win
ON VALUE-CHANGED OF tb_sort IN FRAME FRAME-A /* Sort by Customer Part#? */
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

    APPLY "entry" TO begin_cust-no.
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
  DISPLAY begin_cust-no end_cust-no begin_slsmn end_slsmn begin_est end_est 
          begin_date end_date begin_date-2 end_date-2 begin_mach end_mach 
          tb_break tb_booked lbl-sort rd_book lbl-booked tb_not-booked tb_sort 
          sl_selected sl_avail rd-dest lines-per-page lv-ornt lv-font-no 
          lv-font-name td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_cust-no end_cust-no begin_slsmn end_slsmn 
         begin_est end_est begin_date end_date begin_date-2 end_date-2 
         begin_mach end_mach tb_break tb_booked rd_book tb_not-booked tb_sort 
         Btn_Def sl_selected sl_avail Btn_Add Btn_Remove btn_Up btn_down 
         rd-dest lines-per-page lv-ornt lv-font-no td-show-parm tb_excel 
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
    IF NOT AVAIL ttRptList THEN
        MESSAGE "no " i ENTRY(i,ctmplist) SKIP
        ctmplist
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    CREATE ttRptSelected.
    ASSIGN ttRptSelected.TextList =  ENTRY(i,cTmpList)
           ttRptSelected.FieldList = ttRptList.FieldList
           ttRptSelected.FieldLength = int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
           ttRptSelected.DisplayOrder = i
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
 /*    DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
     DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
     DEFINE VARIABLE result AS LOGICAL NO-UNDO.

/*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
     IF NOT printok THEN
     RETURN NO-APPLY.
*/

  /* Use Progress Print. Always use Font#9 in Registry (set above) */
     RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 3, INPUT 1, INPUT 0, INPUT 0, OUTPUT result).
                          /* font #*/ /* use-dialog(1) and landscape(2) */
                          */
 RUN custom/prntproc.p (list-name,int(lv-font-no),lv-ornt).
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
DEF VAR str-tit4 AS cha NO-UNDO.
DEF VAR str-tit5 AS cha NO-UNDO.
DEF VAR str-line AS cha FORM "x(300)" NO-UNDO.
/*{sys/form/r-topw.f}*/
{sys/form/r-topsw.f}

def var fcust like eb.cust-no init "".
def var tcust like fcust init "zzzzzzzz".
def var fsman like eb.sman init "" no-undo.
def var tsman like fsman init "zzz" no-undo.
def var fdate like est.mod-date format "99/99/9999" init 01/01/0001.
def var tdate like fdate init today.
def var fest  like est.est-no init "" no-undo.
def var test  like fest init "zzzzz" no-undo.

def var v-blk-dim as char no-undo.
def var v-booked  as char no-undo.
def var v-qty     as int  no-undo.
def var v-b-qty   as int  no-undo.
DEF VAR v_ord-date AS CHAR NO-UNDO.

def var x as int no-undo.
def var xxx as dec no-undo.
DEF VAR lv-hdr-m-dscr AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.

/* gdm - 10130804 */
DEF VAR v_est-no LIKE eb.est-no    NO-UNDO.
DEF VAR v_custnm LIKE cust.name    NO-UNDO.
DEF VAR v_shpnm  LIKE eb.ship-name NO-UNDO.
DEF VAR v_stydsc LIKE style.dscr   NO-UNDO.
DEF VAR v_i-name LIKE item.i-name  NO-UNDO.
DEF VAR adder AS CHAR FORMAT "x(10)" EXTENT 5 NO-UNDO.

DEF VAR v_blksz    AS CHAR FORMAT 'x(25)'      NO-UNDO.
DEF VAR v_mod-date AS CHAR FORMAT '99/99/9999' NO-UNDO
    .

DEF VAR excelheader AS CHAR NO-UNDO.
DEF VAR cDisplay AS cha NO-UNDO.
DEF VAR cExcelDisplay AS cha NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR cTmpField AS CHA NO-UNDO.
DEF VAR cVarValue AS cha NO-UNDO.
DEF VAR cExcelVarValue AS cha NO-UNDO.
DEF VAR cFieldName AS cha NO-UNDO.
DEF VAR cSelectedList AS cha NO-UNDO.
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEF BUFFER beb FOR eb.
DEFINE VARIABLE iOrder AS INTEGER NO-UNDO.
DEFINE VARIABLE cJob AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLineCount AS INTEGER NO-UNDO .

FORM HEADER
     "Machine:"
     lv-hdr-m-dscr
     SKIP(1)

    WITH FRAME r-top2 NO-LABELS NO-BOX WIDTH 132 STREAM-IO NO-UNDERLINE PAGE-TOP.

assign
 str-tit2 = trim(c-win:title) + " - by Estimate Number"
 {sys/inc/ctrtext.i str-tit2 112}

 fcust    = begin_cust-no
 tcust    = end_cust-no
 fsman    = begin_slsmn
 tsman    = end_slsmn
 fdate    = begin_date
 tdate    = end_date
 fest     = FILL(" ",8 - LENGTH(TRIM(begin_est))) + TRIM(begin_est)
 test     = FILL(" ",8 - LENGTH(TRIM(end_est))) + TRIM(end_est).

FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

    ASSIGN str-tit4 = str-tit4 + 
               ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength + 1 - LENGTH(ttRptSelected.TextList))
            str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
            excelheader = excelHeader + ttRptSelected.TextList + ",".        

END.


{sys/inc/print1.i}

{sys/inc/outprint.i  value(lines-per-page)}

IF tb_excel THEN DO:

        OUTPUT STREAM excel TO VALUE(fi_file) .
END. 

IF tb_excel AND tb_break EQ NO THEN DO:
        PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END. 

SESSION:SET-WAIT-STATE ("general"). 

if td-show-parm then run show-param.

/*VIEW FRAME r-top.

PUT  str-tit4 FORMAT "x(600)"
     SKIP
     str-tit5 FORMAT "x(600)"
     SKIP
    .*/

FOR EACH tt-eb:
  DELETE tt-eb.
END.

FOR EACH est
    WHERE est.company     EQ cocode
      AND est.est-no      GE fest
      AND est.est-no      LE test
      AND est.est-date    GE fdate
      AND est.est-date    LE tdate
      AND est.mod-date    GE begin_date-2
      AND est.mod-date    LE end_date-2
    NO-LOCK,

    FIRST est-qty
    WHERE est-qty.company EQ est.company
      AND est-qty.est-no  EQ est.est-no
    NO-LOCK,

    EACH eb
    WHERE eb.company EQ est.company
      AND eb.est-no  EQ est.est-no
      AND eb.cust-no GE fcust
      AND eb.cust-no LE tcust
      AND eb.sman    GE fsman
      AND eb.sman    LE tsman
      AND ((tb_booked AND
            CAN-FIND(FIRST oe-ordl
                     WHERE oe-ordl.company EQ est.company
                       AND (oe-ordl.est-no  EQ est.est-no OR  oe-ordl.i-no  EQ eb.stock-no )  )) OR
           (tb_not-booked AND
            NOT CAN-FIND(FIRST oe-ordl
                         WHERE oe-ordl.company EQ est.company
                           AND oe-ordl.est-no  EQ est.est-no OR  oe-ordl.i-no  EQ eb.stock-no)))
    NO-LOCK,

    FIRST ef
    WHERE ef.company EQ eb.company
      AND ef.est-no  EQ eb.est-no
      AND ef.form-no EQ eb.form-no
    NO-LOCK

    BREAK BY est.est-no:

    {custom/statusMsg.i " 'Processing Estimate#:  '  + eb.est-no  "}

  IF FIRST-OF(est.est-no) THEN ll = YES.

  FOR EACH est-op
      WHERE est-op.company EQ eb.company
        AND est-op.est-no  EQ eb.est-no
        AND est-op.s-num   EQ eb.form-no
        AND (est-op.b-num  EQ eb.blank-no OR est-op.b-num EQ 0)
        AND est-op.m-code  GE begin_mach
        AND est-op.m-code  LE end_mach
        AND est-op.line    LT 500
      NO-LOCK:

    IF ll THEN DO:
      ASSIGN
       ll    = NO
       v-qty = v-qty + 1.

      IF CAN-FIND(FIRST oe-ordl
                  WHERE oe-ordl.company EQ est.company
                    AND oe-ordl.est-no  EQ est.est-no)
      THEN v-b-qty = v-b-qty + 1.
    END.

    FIND FIRST tt-eb
        WHERE tt-eb.m-code EQ est-op.m-code
          AND tt-eb.row-id EQ ROWID(eb)
        NO-ERROR.
    IF NOT AVAIL tt-eb THEN CREATE tt-eb.
    tt-eb.row-id = ROWID(eb).

    IF tb_break THEN tt-eb.m-code = est-op.m-code.
    ELSE LEAVE.
  END.

  RELEASE tt-eb.
END.

FOR EACH tt-eb,

    FIRST eb WHERE ROWID(eb) EQ tt-eb.row-id NO-LOCK,

    FIRST ef
    WHERE ef.company EQ eb.company
      AND ef.est-no  EQ eb.est-no
      AND ef.form-no EQ eb.form-no
    NO-LOCK,

    FIRST est
    WHERE est.company EQ ef.company
      AND est.est-no  EQ ef.est-no
    NO-LOCK

    BREAK BY tt-eb.m-code
          BY (IF tb_sort THEN "" ELSE eb.est-no)
          BY eb.cust-no
          BY eb.part-no
          BY (IF tb_sort THEN eb.est-no ELSE "")

    WITH FRAME est:

  {custom/statusMsg.i " 'Processing Estimate#:  '  + eb.est-no  "}

  IF tb_break AND FIRST-OF(tt-eb.m-code) THEN DO:
    FIND FIRST mach
        WHERE mach.company EQ eb.company
          AND mach.m-code  EQ tt-eb.m-code
        NO-LOCK NO-ERROR.
    lv-hdr-m-dscr = TRIM(tt-eb.m-code) + " " +
                    (IF AVAIL mach THEN mach.m-dscr ELSE "Not on File...").

   
    PAGE.
    VIEW FRAME r-top.
    VIEW FRAME r-top2.
    PUT  str-tit4 FORMAT "x(600)"
     SKIP
     str-tit5 FORMAT "x(600)"
     SKIP .
     
    iLineCount = 0.


    /* gdm - 10130804 */
    IF tb_excel
      THEN 
        PUT STREAM excel UNFORMATTED
          'Machine:'
          ',"' lv-hdr-m-dscr '"' 
         SKIP
         '"' REPLACE(excelheader,',','","') '"' SKIP.



  END.
  ELSE IF FIRST(tt-eb.m-code) THEN DO:
   VIEW FRAME r-top.
   
    PUT  str-tit4 FORMAT "x(600)"
     SKIP
     str-tit5 FORMAT "x(600)"
     SKIP .
    iLineCount = 0.
  END.

  find first item
      where item.company eq cocode
        and item.i-no    eq ef.board
      no-lock no-error.
  find first cust
      where cust.company eq cocode
        and cust.cust-no eq eb.cust-no
      no-lock no-error.
  find first style
      where style.company eq cocode
        and style.style   eq eb.style
      no-lock no-error.

  v-blk-dim = "".
  {sys/inc/fraction.i eb.len v-blk-dim}
  {sys/inc/fraction.i eb.wid v-blk-dim}
  {sys/inc/fraction.i eb.dep v-blk-dim}

  /* gdm - 10130804 */
  ASSIGN
    v_est-no = trim(eb.est-no)
    v_custnm = IF AVAIL cust THEN cust.name ELSE eb.ship-name   
    v_stydsc = IF avail style THEN style.dscr ELSE ""
    
    v_i-name = IF AVAIL item THEN item.i-name ELSE ""
    v_blksz  = STRING(eb.t-wid) + " x " + STRING(eb.t-len)
    v_mod-date = STRING(est.mod-date,'99/99/9999')
    v_ord-date = STRING(est.ord-date,'99/99/9999') .

    FIND LAST oe-ordl NO-LOCK
        WHERE oe-ordl.company EQ est.company
        AND oe-ordl.est-no  EQ  FILL(" ",8 - LENGTH(TRIM(est.est-no))) + TRIM(est.est-no)
        AND oe-ordl.ord-no NE 0  NO-ERROR .

        iOrder = IF AVAIL oe-ordl THEN oe-ordl.ord-no ELSE 0 .

     FIND LAST job-hdr NO-LOCK
        WHERE job-hdr.company EQ est.company
        AND job-hdr.est-no  EQ est.est-no NO-ERROR .

       cJob = IF AVAIL job-hdr THEN job-hdr.job-no ELSE "" .

    IF rd_book EQ "Order" THEN
        v-booked = IF iOrder NE 0 AND cJob EQ "" 
                    THEN "Booked" ELSE "" .

    ELSE IF rd_book EQ "Job" THEN 
        v-booked = IF iOrder EQ 0 AND cJob NE "" 
                    THEN "Booked" ELSE "" .
    ELSE v-booked = IF iOrder NE 0 OR cJob NE ""
                    THEN "Booked" ELSE "" .

    IF AVAIL ef THEN
        ASSIGN
        adder[1] = ef.adder[1] 
        adder[2] = ef.adder[2]
        adder[3] = ef.adder[3]
        adder[4] = ef.adder[4]
        adder[5] = ef.adder[5].



  if first-of(eb.cust-no) or tb_sort THEN DO: 
    
    IF iLineCount + 9 > lines-per-page THEN DO:
        PAGE.
        VIEW FRAME r-top.
        PUT  str-tit4 FORMAT "x(600)"
            SKIP
            str-tit5 FORMAT "x(600)"
            SKIP .
        iLineCount = 0.
    END.


      BUFFER beb:FIND-BY-ROWID(ROWID(eb), NO-LOCK) .
       ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".    

       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
         cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).

         IF INDEX(cTmpField,".") > 0 THEN DO:
                 cFieldName = cTmpField.
                 cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                 IF cFieldName BEGINS "eb" THEN hField = IF AVAIL beb THEN BUFFER beb:BUFFER-FIELD(cTmpField) ELSE ?.
                 IF hField <> ? THEN DO:                 
                     cTmpField = substring(GetFieldValue(hField),1,int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                     IF ENTRY(i,cSelectedList) = "Job#" THEN
                        cTmpField = cTmpField + IF cTmpField <> "" THEN "-" + string(fg-rcpth.job-no2,"99") ELSE "".   

                     IF ENTRY(i,cSelectedList) = "Last Order#" THEN
                        cTmpField = IF cTmpField <> "0" THEN cTmpField ELSE "".   
                     
                     cDisplay = cDisplay + cTmpField + 
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField))
                               .
                     cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".
                 END.
                 ELSE DO:
                    cTmpField = substring(cFieldName,1,int( entry( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).                  
                    cDisplay = cDisplay + FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).
                    cExcelDisplay = cExcelDisplay + quoter(" ")  /*GetFieldValue(hField))*/ + ",".
                 END.
         END.
         ELSE DO:       
            CASE cTmpField: 
                 WHEN "v_est-no" THEN cVarValue = string(v_est-no). 
                 WHEN "v_custnm" THEN cVarValue = string(v_custnm).
                 WHEN "v_mod-date" THEN cVarValue = string(v_mod-date).
                 WHEN "v_stydsc" THEN cVarValue = STRING(v_stydsc).
                 WHEN "v_blksz" THEN cVarValue = string(v_blksz).
                 WHEN "v-blk-dim" THEN cVarValue = string(v-blk-dim).
                 WHEN "v_i-name" THEN cVarValue = string(v_i-name).
                 WHEN "v-booked" THEN cVarValue = string(v-booked).
                 WHEN "ord-date" THEN cVarValue = IF v_ord-date NE ? THEN STRING(v_ord-date) ELSE "" .
                 WHEN "add1" THEN cVarValue = STRING(adder[1]).
                 WHEN "add2" THEN cVarValue = STRING(adder[2]).
                 WHEN "add3" THEN cVarValue = STRING(adder[3]).
                 WHEN "add4" THEN cVarValue = STRING(adder[4]).
                 WHEN "add5" THEN cVarValue = STRING(adder[5]).
                 WHEN "comm"   THEN cVarValue = STRING(eb.comm,"->>9.99%").
                 WHEN "fg-item" THEN cVarValue = IF AVAIL eb AND eb.stock-no NE "" THEN STRING(eb.stock-no,"x(15)") ELSE "".
                 WHEN "job-no" THEN cVarValue = IF AVAIL job-hdr THEN STRING(job-hdr.job-no + "-" + string(job-hdr.job-no2,"99"),"x(9)") ELSE "".
                 WHEN "eb-ord-no" THEN cVarValue = IF eb.ord-no NE 0 THEN STRING(eb.ord-no) ELSE STRING(iOrder).
                      
            END CASE.

            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
         END.
      END.
      PUT UNFORMATTED cDisplay SKIP.
      iLineCount = iLineCount + 1. 
      IF tb_excel THEN DO:
       cExcelDisplay = cExcelDisplay.
       PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
      END.

  END.
  /*else if first-of(eb.part-no) then do:

      put eb.part-dscr2 at 41.

      /* gdm - 10130804 */
      IF tb_excel THEN 
        PUT STREAM excel UNFORMATTED
            ',,,'
            '"' eb.part-dscr2                '"'
          SKIP.


  END. */

  if last-of(eb.cust-no) or tb_sort then down with frame est.

  /* gdm - 10130804 */
  IF tb_break AND tb_excel AND LAST-OF(tt-eb.m-code)
    THEN PUT STREAM excel UNFORMATTED SKIP(1).   

  if last(tt-eb.m-code) THEN DO:

    put skip(2)
        space(10)
        "Total Estimates:" + " " + trim(string(v-qty,">>>,>>>,>>9"))
                        format "x(30)"
        skip
        space(10)
        "   Total Booked:" + " " + trim(string(v-b-qty,">>>,>>>,>>9"))
                        format "x(30)"
        skip(1).

    /* gdm - 10130804 */
      IF tb_excel THEN 
        PUT STREAM excel UNFORMATTED
          SKIP(2)
          'Total Estimates:' + ',' + 
          trim(string(v-qty,">>>,>>>,>>9"))
         SKIP
          '   Total Booked:' + ',' + 
         trim(string(v-b-qty,">>>,>>>,>>9")) 
          SKIP.  

  END.

end.

/* gdm - 10130804 */
IF tb_excel THEN DO:
    OUTPUT STREAM excel CLOSE.
    IF tb_runExcel THEN
        OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

/* END ---------------------------------- copr. 1992  Advanced Software, Inc. */

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

  put fill("-",80) format "x(80)" SKIP.

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

