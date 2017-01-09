&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-ibtag.w

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

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHARACTER NO-UNDO.
DEF VAR v-roll-multp AS DEC DECIMALS 4 NO-UNDO.

DEF TEMP-TABLE tt-rm-bin NO-UNDO LIKE rm-bin
                                 FIELD trans-date LIKE rm-rcpth.trans-date.
    

DEF STREAM excel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 as-of-date begin_rm-no ~
end_rm-no begin_whs end_whs begin_procat end_procat begin_mat-type ~
end_mat-type begin_date end_date tb_zero-bal tb_tagask tb_subt ~
tb_total-rolls tb_grdt tb_estmat lv-ornt lines-per-page rd-dest lv-font-no ~
td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS as-of-date begin_rm-no end_rm-no begin_whs ~
end_whs begin_procat end_procat begin_mat-type end_mat-type begin_date ~
end_date tb_zero-bal tb_tagask tb_subt tb_total-rolls tb_grdt tb_estmat lv-ornt ~
lines-per-page rd-dest lv-font-no lv-font-name td-show-parm tb_excel ~
tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
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
     SIZE 96 BY 8.33.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 11.91.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_grdt AS LOGICAL INITIAL no 
     LABEL "Print Grand Totals?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .95 NO-UNDO.

DEFINE VARIABLE tb_estmat AS LOGICAL INITIAL no 
     LABEL "Print Est. Mat Only?"  /* Include */
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_subt AS LOGICAL INITIAL yes 
     LABEL "Print Sub Totals?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

DEFINE VARIABLE tb_tagask AS LOGICAL INITIAL no 
     LABEL "Print * on Tag?" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.4 BY .95 NO-UNDO.

DEFINE VARIABLE tb_total-rolls AS LOGICAL INITIAL no 
     LABEL "Print Total Rolls?" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.4 BY .95 NO-UNDO.

DEFINE VARIABLE tb_zero-bal AS LOGICAL INITIAL no 
     LABEL "Include Zero Balances?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     as-of-date AT ROW 1.95 COL 72 COLON-ALIGNED
     begin_rm-no AT ROW 3.14 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Item Number"
     end_rm-no AT ROW 3.14 COL 72 COLON-ALIGNED HELP
          "Enter Ending Item number"
     begin_whs AT ROW 4.33 COL 28 COLON-ALIGNED HELP
          "Enter Beginng Warehouse"
     end_whs AT ROW 4.33 COL 72 COLON-ALIGNED HELP
          "Enter Endng Warehouse"
     begin_procat AT ROW 5.52 COL 28 COLON-ALIGNED HELP
          "Enter Begining Category"
     end_procat AT ROW 5.52 COL 72 COLON-ALIGNED HELP
          "Enter Ending Category"
     begin_mat-type AT ROW 6.71 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Material Type"
     end_mat-type AT ROW 6.71 COL 72 COLON-ALIGNED HELP
          "Enter ending Material Type"
     begin_date AT ROW 7.91 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Receipt Date"
     end_date AT ROW 7.91 COL 72 COLON-ALIGNED HELP
          "Enter Ending Receipt Date"
     tb_zero-bal AT ROW 9.33 COL 30
     tb_tagask AT ROW 9.33 COL 58.6
     tb_subt AT ROW 10.29 COL 30
     tb_total-rolls AT ROW 10.29 COL 58.6
     tb_grdt AT ROW 11.24 COL 30
     tb_estmat AT ROW 11.24 COL 58.6 WIDGET-ID 46
     lv-ornt AT ROW 13.62 COL 31 NO-LABEL
     lines-per-page AT ROW 13.62 COL 84 COLON-ALIGNED
     rd-dest AT ROW 14.33 COL 6 NO-LABEL
     lv-font-no AT ROW 15.05 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 16 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 17.19 COL 31
     tb_excel AT ROW 18.62 COL 67 RIGHT-ALIGNED
     tb_runExcel AT ROW 18.62 COL 90.8 RIGHT-ALIGNED
     fi_file AT ROW 19.57 COL 45 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 21.95 COL 21
     btn-cancel AT ROW 21.95 COL 60
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 13.38 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 12.91 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96.4 BY 22.91.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "RM Inventory By Bin/Tag"
         HEIGHT             = 22.95
         WIDTH              = 96.8
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

ASSIGN 
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

&Scoped-define SELF-NAME C-Win
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


&Scoped-define SELF-NAME as-of-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL as-of-date C-Win
ON LEAVE OF as-of-date IN FRAME FRAME-A /* As of */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Receipt Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_mat-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_mat-type C-Win
ON LEAVE OF begin_mat-type IN FRAME FRAME-A /* Beginning Material Type */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_procat C-Win
ON LEAVE OF begin_procat IN FRAME FRAME-A /* Beginning  Category */
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


&Scoped-define SELF-NAME begin_whs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_whs C-Win
ON LEAVE OF begin_whs IN FRAME FRAME-A /* Beginning Warehouse */
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
                            &begin_cust= "begin_procat"
                            &END_cust= "begin_procat" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = ''
                             &begin_cust= "begin_procat"
                             &END_cust= "begin_procat"
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = ''
                                  &begin_cust="begin_procat"
                                  &END_cust="begin_procat"
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


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Receipt Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_mat-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_mat-type C-Win
ON LEAVE OF end_mat-type IN FRAME FRAME-A /* Ending Material Type */
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


&Scoped-define SELF-NAME end_whs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_whs C-Win
ON LEAVE OF end_whs IN FRAME FRAME-A /* Ending Warehouse */
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


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_grdt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_grdt C-Win
ON VALUE-CHANGED OF tb_grdt IN FRAME FRAME-A /* Print Grand Totals? */
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


&Scoped-define SELF-NAME tb_subt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_subt C-Win
ON VALUE-CHANGED OF tb_subt IN FRAME FRAME-A /* Print Sub Totals? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_tagask
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_tagask C-Win
ON VALUE-CHANGED OF tb_tagask IN FRAME FRAME-A /* Print * on Tag? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_total-rolls
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_total-rolls C-Win
ON VALUE-CHANGED OF tb_total-rolls IN FRAME FRAME-A /* Print Total Rolls? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_zero-bal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_zero-bal C-Win
ON VALUE-CHANGED OF tb_zero-bal IN FRAME FRAME-A /* Include Zero Balances? */
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

  as-of-date = TODAY.

  FIND FIRST uom WHERE
       uom.uom = "ROLL"
       NO-LOCK NO-ERROR.

  IF AVAIL uom THEN
  DO:
     v-roll-multp = uom.mult.
     RELEASE uom.
  END.

  RUN enable_UI.
  
  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
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
          end_procat begin_mat-type end_mat-type begin_date end_date tb_zero-bal 
          tb_tagask tb_subt tb_total-rolls tb_grdt tb_estmat lv-ornt lines-per-page 
          rd-dest lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel 
          fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 as-of-date begin_rm-no end_rm-no begin_whs end_whs 
         begin_procat end_procat begin_mat-type end_mat-type begin_date 
         end_date tb_zero-bal tb_tagask tb_subt tb_total-rolls tb_grdt tb_estmat lv-ornt 
         lines-per-page rd-dest lv-font-no td-show-parm tb_excel tb_runExcel 
         fi_file btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rm-mkbin C-Win 
PROCEDURE rm-mkbin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF BUFFER b-rm-bin FOR rm-bin.

DEF VAR v-r-qty AS   DEC    NO-UNDO.
DEF VAR v-i-qty AS   DEC    NO-UNDO.
DEF VAR v-t-qty AS   DEC    NO-UNDO.
DEF VAR ld-qty  AS   DEC    NO-UNDO.
DEF VAR ld-cst  AS   DEC    NO-UNDO.
DEF VAR lv-uom  AS   CHAR   NO-UNDO.

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
          AND rm-rcpth.i-no         EQ item.i-no
        USE-INDEX r-no
    
        BY rm-rcpth.trans-date
        BY rm-rcpth.r-no:

      tt-rm-bin.trans-date = rm-rcpth.trans-date.
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
      tt-rm-bin.trans-date = DATE(SUBSTR(rm-bin.rec_key,1,8)) NO-ERROR.
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

{sys/form/r-topw.f}

def var save_id   as recid.
def var v-price     as dec  format "->>>>9.99".
def var v-tot-price as dec  format "$->>,>>>,>>9.99".
def var v-cum-qty   as dec  format "->>>>>9.999".
def var v-cum-price as dec  format "->>>,>>9.99".
def var v-cum-qty2   as dec  format "->>>>>9.999".  /* item totals */
DEF VAR v-gt-qty2 AS DEC format "->>>,>>>,>>9.999".
def var v-cum-price2 as dec  format "->>>,>>9.99".  /* item totals */
DEF VAR v-cum-rolls AS INT FORMAT ">>>>>9" NO-UNDO.
DEF VAR v-item-rolls AS INT FORMAT ">>>>>9" NO-UNDO.
DEF VAR v-tot-rolls AS INT FORMAT ">>>>>9" NO-UNDO.
DEF VAR v-rolls-dec AS DEC DECIMALS 4 NO-UNDO.
def var fitm like rm-bin.i-no format "X(10)" init "".
def var titm like fitm init "zzzzzzzzzz".
def var floc like rm-bin.loc init "".
def var tloc like floc init "zzzzz".
def var fcat as ch init "".
def var tcat like fcat init "zzzzzz".
def var type as log format "R/E" init yes.
def var ftyp like item.mat-type init "".
def var ttyp like ftyp init "z".
def var zbal as log format "Y/N" init no.
def var v-fst-loc as logical.
def var v-fst-ino as logical.
def var v-lst-ino as logical.
def var v-prnt-line as int.
def var v-cost like rm-bin.cost.
def var psubtot as log init yes.
def var pgtot as log init no.
DEF VAR excelheader AS CHARACTER  NO-UNDO.
DEF VAR tagask AS LOG NO-UNDO.
DEF VAR v-lf-qty LIKE rm-bin.qty NO-UNDO.

/* rdb 02/06/07 02050701 */
DEFINE VARIABLE chrTotCostVal AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chrRmBinTag   AS CHARACTER FORMAT "x(22)" NO-UNDO.

{custom/statusMsg.i "'Processing...'"} 

find first ce-ctrl where ce-ctrl.company eq cocode no-lock no-error.

assign
 str-tit2 = c-win:title
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
 tagask  = tb_tagask.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}


IF tb_excel THEN DO:

    excelheader = "Whse,Item,Description,Bin,Tag,Rct Date," +
                  "Quantity,Unit Cost,Total Cost Value,Item Name".

    OUTPUT STREAM excel TO VALUE(fi_file).
    PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' skip.
END.

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

  display "" with frame r-top.

  EMPTY TEMP-TABLE tt-rm-bin.

 IF NOT tb_estmat THEN do:
  FOR EACH item
      where item.company           eq cocode
        and item.i-no              ge fitm
        and item.i-no              le titm
        and item.i-no              ne ""
        and item.procat            ge fcat
        and item.procat            le tcat
        and item.mat-type          ge ftyp
        and item.mat-type          le ttyp
        and item.i-code            eq "R"
      no-lock:

       {custom/statusMsg.i "'Processing Item # ' + string(item.i-no)"} 

    RUN rm-mkbin.

    IF zbal AND ITEM.q-onh EQ 0 AND
       NOT CAN-FIND(FIRST tt-rm-bin WHERE
       tt-rm-bin.company EQ ITEM.company AND
       tt-rm-bin.i-no EQ item.i-no) THEN
       DO:
          CREATE tt-rm-bin.
          ASSIGN tt-rm-bin.company = ITEM.company
                 tt-rm-bin.i-no = ITEM.i-no
                 tt-rm-bin.trans-date = TODAY.
          RELEASE tt-rm-bin.
       END.
  END.
 END.
 ELSE DO:
     FOR EACH item
      where item.company           eq cocode
        and item.i-no              ge fitm
        and item.i-no              le titm
        and item.i-no              ne ""
        and item.procat            ge fcat
        and item.procat            le tcat
        and item.mat-type          ge ftyp
        and item.mat-type          le ttyp
        and item.i-code            eq "E"
      no-lock:

          {custom/statusMsg.i "'Processing Item # ' + string(item.i-no)"} 

    RUN rm-mkbin.

    IF zbal AND ITEM.q-onh EQ 0 AND
       NOT CAN-FIND(FIRST tt-rm-bin WHERE
       tt-rm-bin.company EQ ITEM.company AND
       tt-rm-bin.i-no EQ item.i-no) THEN
       DO:
          CREATE tt-rm-bin.
          ASSIGN tt-rm-bin.company = ITEM.company
                 tt-rm-bin.i-no = ITEM.i-no
                 tt-rm-bin.trans-date = TODAY.
          RELEASE tt-rm-bin.
       END.
  END.
 END.
  
  for each tt-rm-bin
      where tt-rm-bin.loc          ge floc
        and tt-rm-bin.loc          le tloc
        AND tt-rm-bin.trans-date   GE begin_date
        AND tt-rm-bin.trans-date   LE end_date
        and (zbal or tt-rm-bin.qty ne 0)
      no-lock,

      FIRST item NO-LOCK
      WHERE item.company EQ tt-rm-bin.company
        AND item.i-no    EQ tt-rm-bin.i-no
     
      break by tt-rm-bin.loc
            by tt-rm-bin.i-no
            by tt-rm-bin.loc-bin
            by tt-rm-bin.tag

      with frame itemx:

       {custom/statusMsg.i "'Processing Item # ' + string(tt-rm-bin.i-no)"} 

    if first-of(tt-rm-bin.loc) or
       line-counter gt page-size - 10 then do:
      IF NOT FIRST(tt-rm-bin.loc) THEN page.
      v-prnt-line = 0.
    end.

    else v-prnt-line = 1.

    v-cost = if ce-ctrl.r-cost then item.avg-cost else tt-rm-bin.cost.

    IF v-cost EQ ? THEN v-cost = 0.
    
    IF tagask AND tt-rm-bin.tag NE "" THEN
      tt-rm-bin.tag = "*" + tt-rm-bin.tag + "*".

    display tt-rm-bin.loc          when first-of(tt-rm-bin.loc)
                                     or v-prnt-line eq 0
                                   label "Whse"
            tt-rm-bin.i-no         when first-of(tt-rm-bin.i-no)
                                     or v-prnt-line eq 0
                                   label "Item"
            ITEM.i-dscr            WHEN FIRST-OF(tt-rm-bin.i-no)
                                     or v-prnt-line eq 0
                                   format "x(30)"
                                   label "Description"
            tt-rm-bin.loc-bin      label "Bin"
            tt-rm-bin.tag          label "Tag" FORM "x(22)"
            tt-rm-bin.trans-date   format "99/99/99"
                                   label "Rct Date"
            tt-rm-bin.qty          format "->>>>>9.999"
                                   label "Quantity"
            v-cost                 format ">>>,>>9.99<<<<"
                                   label "Unit Cost"
            tt-rm-bin.qty * v-cost format "->>>,>>9.99"
                                   COLUMN-LABEL "Total!Cost Value"
            ITEM.i-name            WHEN FIRST-OF(tt-rm-bin.i-no)
                                     OR v-prnt-line EQ 0
                                   FORMAT "x(30)"
                                   LABEL "Item Name"  SKIP 

         WITH FRAME itemx NO-BOX NO-ATTR-SPACE DOWN STREAM-IO WIDTH 164.

    IF tb_excel THEN DO:
     
        
        ASSIGN
          chrRmBinTag = TRIM(tt-rm-bin.tag)
          chrTotCostVal = STRING(tt-rm-bin.qty * v-cost, "->>>>>9.99").
        
        EXPORT STREAM excel DELIMITER ","
          tt-rm-bin.loc tt-rm-bin.i-no ITEM.i-dscr tt-rm-bin.loc-bin
          chrRmBinTag tt-rm-bin.trans-date tt-rm-bin.qty v-cost 
          chrTotCostVal ITEM.i-name.
    
    END.

    assign
     v-cum-qty   = v-cum-qty   + tt-rm-bin.qty
     v-cum-price = v-cum-price + (tt-rm-bin.qty * v-cost).

    IF tb_total-rolls AND item.r-wid > 0 THEN
    DO:
       IF tt-rm-bin.tag NE "" AND tt-rm-bin.qty <> 0 THEN
          ASSIGN
            v-cum-rolls = v-cum-rolls + 1
            v-item-rolls = v-item-rolls + 1.
       ELSE
       DO:
          IF ITEM.cons-uom NE "LF" THEN
             RUN sys/ref/convquom.p(item.cons-uom, "LF", item.basis-w,
                                   (if item.r-wid eq 0 THEN item.s-len
                                                       ELSE 12),
                                   (if item.r-wid eq 0 THEN item.s-wid
                                                       ELSE item.r-wid),
                                   item.s-dep,                    
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

    if last-of(tt-rm-bin.loc-bin) then do:
      if not first-of(tt-rm-bin.loc-bin) and psubtot then
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
         END.
      
      if not last-of(tt-rm-bin.i-no) then put skip(1).

      assign
       v-cum-qty2   = v-cum-qty2   + v-cum-qty
       v-cum-price2 = v-cum-price2 + v-cum-price
       v-cum-qty    = 0
       v-cum-price  = 0
       v-cum-rolls  = 0.
    end.

    if last-of(tt-rm-bin.i-no) then do:
      if psubtot then
      DO:
        IF not first-of(tt-rm-bin.i-no) AND
           NOT(tb_total-rolls AND item.r-wid > 0) THEN
           put "-----------"          to 100
               "-----------"          to 123 skip
               "Item Total"           at 63
               v-cum-qty2             to 100
               v-cum-price2           to 123.
        ELSE
        IF tb_total-rolls AND item.r-wid > 0 THEN
        DO:
           put "------"               TO 58
               "-----------"          to 100
               "-----------"          to 123 skip
               "Item Total Rolls"     AT 36
               v-item-rolls           TO 58
               "Item Total"           at 63
               v-cum-qty2             to 100
               v-cum-price2           to 123.
        END.
      END.

      put skip(1).
      
      assign
       v-tot-price  = v-tot-price + v-cum-price2
       v-tot-rolls = v-tot-rolls + v-item-rolls
       v-gt-qty2 = v-gt-qty2 + v-cum-qty2
       v-cum-qty2   = 0
       v-item-rolls = 0
       v-cum-price2 = 0.
    end.

    if last-of(tt-rm-bin.loc) then do:
      if pgtot then
      DO:
        IF tb_total-rolls THEN
           put skip(1)
               "------"                    TO 58
               "---------------"           to 100
               "--------------"            to 123
               "Grand Totals"              AT 40
               v-tot-rolls                 TO 58
               v-gt-qty2                   TO 100
               v-tot-price                 to 123
               "------"                    TO 58
               "---------------"           to 100
               "--------------"            to 123.
        ELSE
           put skip(1)
               "---------------"           to 100
               "--------------"            to 123
               "Grand Totals"              AT 40
               v-gt-qty2                   TO 100
               v-tot-price                 to 123
               "---------------"           to 100
               "--------------"            to 123.
      END.

      ASSIGN
      v-tot-price = 0
      v-tot-rolls = 0
      v-gt-qty2 = 0.
    end.
  end.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

  IF tb_excel THEN DO:
         OUTPUT STREAM excel CLOSE.
         IF tb_runExcel THEN
             OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
     END.


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

