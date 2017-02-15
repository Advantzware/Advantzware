&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: rmrep\r-consum.w

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
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHARACTER NO-UNDO.

/* AJ 06/25/2008  Added two variables for excel report */
DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
DEFINE STREAM excel.

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
begin_procat end_procat begin_date end_date begin_whs end_whs tg-paper ~
tg-board rd-summary tg-subtot tg-subtot-item rd-dest lv-ornt lines-per-page ~
lv-font-no td-show-parm tb_excel tb_runExcel v-excel-file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_rm-no end_rm-no begin_procat ~
end_procat begin_date end_date begin_whs end_whs tg-paper tg-board ~
rd-summary tg-subtot tg-subtot-item rd-dest lv-ornt lines-per-page ~
lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel v-excel-file 

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_procat AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Category" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_rm-no AS CHARACTER FORMAT "X(10)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_whs AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_procat AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Category" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_rm-no AS CHARACTER FORMAT "X(10)":U INITIAL "zzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_whs AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

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

DEFINE VARIABLE v-excel-file AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\tmp~\r-vprice.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 48 BY 1 NO-UNDO.

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

DEFINE VARIABLE rd-summary AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Summary", "S",
"Detail", "D"
     SIZE 31 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 9.29.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 9.76.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL no 
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

DEFINE VARIABLE tg-board AS LOGICAL INITIAL no 
     LABEL "Board" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY 1 NO-UNDO.

DEFINE VARIABLE tg-paper AS LOGICAL INITIAL yes 
     LABEL "Paper" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE tg-subtot AS LOGICAL INITIAL no 
     LABEL "Subtotal By Category" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE tg-subtot-item AS LOGICAL INITIAL no 
     LABEL "Subtotal by Item" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY 1 NO-UNDO.


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
     begin_whs AT ROW 5.05 COL 28 COLON-ALIGNED HELP
          "Enter Beginng Warehouse"
     end_whs AT ROW 5.05 COL 69 COLON-ALIGNED HELP
          "Enter Endng Warehouse"
     tg-paper AT ROW 6.14 COL 41
     tg-board AT ROW 6.14 COL 53
     rd-summary AT ROW 7.33 COL 41 NO-LABEL
     tg-subtot AT ROW 8.48 COL 41
     tg-subtot-item AT ROW 9.52 COL 41 WIDGET-ID 2
     rd-dest AT ROW 11.95 COL 5 NO-LABEL
     lv-ornt AT ROW 12.91 COL 31 NO-LABEL
     lines-per-page AT ROW 12.91 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 14.19 COL 29.8 COLON-ALIGNED
     lv-font-name AT ROW 15.24 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 16.29 COL 31
     tb_excel AT ROW 17.33 COL 31.8
     tb_runExcel AT ROW 17.33 COL 77 RIGHT-ALIGNED
     v-excel-file AT ROW 18.67 COL 29.4 COLON-ALIGNED
     btn-ok AT ROW 20.29 COL 23
     btn-cancel AT ROW 20.29 COL 62
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Report Type:" VIEW-AS TEXT
          SIZE 16 BY 1 AT ROW 7.33 COL 24
          FONT 6
     "Material Type:" VIEW-AS TEXT
          SIZE 17 BY 1 AT ROW 6.14 COL 23
          FONT 6
     RECT-6 AT ROW 10.76 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96.4 BY 21.43.


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
         TITLE              = "Roll Stock Consumed Report"
         HEIGHT             = 21.52
         WIDTH              = 96.4
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



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
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
       end_procat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_rm-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_whs:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd-summary:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tg-board:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tg-paper:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tg-subtot:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tg-subtot-item:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Roll Stock Consumed Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Roll Stock Consumed Report */
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
ON HELP OF begin_procat IN FRAME FRAME-A /* Beginning Category */
DO:
     DEF VAR char-val AS cha NO-UNDO.
    RUN windows/l-rmcat.w (cocode,SELF:SCREEN-VALUE,OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN SELF:SCREEN-VALUE = ENTRY(1,char-val).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
    {src/WinKit/triggerend.i}
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

  run run-report.
  STATUS DEFAULT "Processing Complete". 
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
    {src/WinKit/triggerend.i}
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
ON HELP OF end_procat IN FRAME FRAME-A /* Ending Category */
DO:
  DEF VAR char-val AS cha NO-UNDO.
    RUN windows/l-rmcat.w (cocode,SELF:SCREEN-VALUE,OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN SELF:SCREEN-VALUE = ENTRY(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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


&Scoped-define SELF-NAME v-excel-file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-excel-file C-Win
ON LEAVE OF v-excel-file IN FRAME FRAME-A /* If Yes, File Name */
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
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i}
END.

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

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO begin_rm-no.
  END.

    {Advantzware/WinKit/embedfinalize-nonadm.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-msf C-Win 
PROCEDURE calc-msf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-msf-qty AS DEC NO-UNDO.

  def var v-len like po-ordl.s-len no-undo.
  def var v-wid like po-ordl.s-len no-undo.
  def var v-dep like po-ordl.s-len no-undo. 
  def var v-bwt like po-ordl.s-len no-undo.
  def var lv-out-qty LIKE rm-rctd.qty no-undo.
  def var lv-out-cost LIKE rm-rctd.cost no-undo.
  DEF VAR lv-qty-uom LIKE rm-rctd.pur-uom NO-UNDO.
  DEF VAR lv-cost-uom LIKE rm-rctd.cost-uom NO-UNDO.

  if avail item then v-dep = item.s-dep.      

  find first po-ordl where po-ordl.company = rm-rcpth.company
                       and po-ordl.po-no = integer(rm-rctd.po-no)
                       and po-ordl.i-no  = rm-rcpth.i-no
                       and po-ordl.job-no = rm-rdtlh.job-no
                       and po-ordl.job-no2 = rm-rdtlh.job-no2
                       and po-ordl.item-type = yes 
                       and po-ordl.s-num = rm-rdtlh.s-num
                           no-lock no-error.
  /*if not avail po-ordl then return.  */

  if avail po-ordl then do:
     assign  v-len = po-ordl.s-len
             v-wid = po-ordl.s-wid
             v-bwt = 0
             lv-qty-uom = po-ordl.cons-uom
             lv-cost-uom = po-ordl.cons-uom.
     {rm/pol-dims.i}
  end.
  else do:
        find first job where job.company eq cocode
                         and job.job-no  eq rm-rdtlh.job-no
                         and job.job-no2 eq rm-rdtlh.job-no2
                no-lock no-error.
        if avail job then do :
             find first job-mat where job-mat.company eq cocode
                                  and job-mat.job     eq job.job
                                  and job-mat.i-no    eq rm-rcpth.i-no
                                  and job-mat.frm     eq rm-rdtlh.s-num
                   no-lock no-error.
             if avail job-mat then assign v-len         = job-mat.len
                                          v-wid         = job-mat.wid
                                          v-bwt         = job-mat.basis-w
                                          .
        end.
        if v-len eq 0 then v-len = if avail item then item.s-len else 0.
        if v-wid eq 0 then v-wid = if avail item and item.r-wid ne 0 then item.r-wid else if avail item then item.s-wid else 0.
        if v-bwt eq 0 then v-bwt = if avail item then item.basis-w else 0.

  end.

  /* convert qty    pr-qty-uom or po-ordl.pr-uom cons-uom*/
 /* run rm/convquom.p(rm-rctd.pur-uom,
                    po-ordl.cons-uom,
                         v-bwt,
                         v-len,
                         input v-wid,
                         input v-dep,
                         input rm-rctd.qty,
                         output lv-out-qty).

  /* convert cost pr-uom*/
  run rm/convcuom.p(rm-rctd.cost-uom, po-ordl.cons-uom,
                    v-bwt, v-len, v-wid, v-dep,
                               rm-rctd.cost, output lv-out-cost).
  */
  run rm/convquom.p("LF",
                    "MSF",
                         v-bwt,
                         v-len,
                         input v-wid,
                         input v-dep,
                         rm-rdtlh.qty,
                         output lv-out-qty).
 op-msf-qty = lv-out-qty.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY begin_rm-no end_rm-no begin_procat end_procat begin_date end_date 
          begin_whs end_whs tg-paper tg-board rd-summary tg-subtot 
          tg-subtot-item rd-dest lv-ornt lines-per-page lv-font-no lv-font-name 
          td-show-parm tb_excel tb_runExcel v-excel-file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_rm-no end_rm-no begin_procat end_procat begin_date 
         end_date begin_whs end_whs tg-paper tg-board rd-summary tg-subtot 
         tg-subtot-item rd-dest lv-ornt lines-per-page lv-font-no td-show-parm 
         tb_excel tb_runExcel v-excel-file btn-ok btn-cancel 
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
/* ---------------------------------------------- rm/rep/rm-trans.p 07/98 JLF */
/* raw materials - transactions edit list                                     */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}

def var v-fitem like rm-rcpth.i-no.
def var v-titem like v-fitem                  init "zzzzzzzzzz".
def var v-fpcat like item.procat.
def var v-tpcat like v-fpcat                  init "zzzzz".
def var v-fdate as   date format "99/99/9999" init 01/01/0001.
def var v-tdate like v-fdate                  init today.
def var v-floc  like rm-rcpth.loc.
def var v-tloc  like v-floc                   initial "zzzzz".
def var v-type  as   char format "x(5)"       init "RITAC".
def var v-code  like rm-rcpth.rita-code.

def var v-value as dec format "->>,>>>,>>9.99".
def var v-job-no as char format "x(9)".
def var v-qty like rm-rdtlh.qty extent 3.
def var v-val like v-value extent 3.

def var v-first as log extent 3.
DEF VAR v-mattype AS cha NO-UNDO.
DEF VAR v-msf-qty AS DEC NO-UNDO.


form item.procat LABEL "Category"
     rm-rcpth.i-no label "ITEM"
     rm-rcpth.i-name format "x(14)" label "DESCRIPTION"
     rm-rdtlh.tag label "TAG#"
     rm-rdtlh.qty format "->>>>>>>>9.99<<" label "QUANTITY"
     v-msf-qty LABEL "MSF"
     ITEM.basis-w LABEL "Weight"
     v-value label "VALUE"
     skip
    with frame itemx no-box down stream-io width 132.

{sa/sa-sls01.i}

find first ap-ctrl where ap-ctrl.company eq cocode no-lock.

assign
 str-tit2 = c-win:TITLE + IF rd-summary = "S" THEN " Summary" ELSE " Detail"
 {sys/inc/ctrtext.i str-tit2 112}

 v-fitem = begin_rm-no
 v-titem = end_rm-no
 v-fpcat = begin_procat
 v-tpcat = end_procat
 v-fdate = begin_date
 v-tdate = end_date
 v-floc  = begin_whs
 v-tloc  = end_whs
 v-type  = "I".
 v-mattype = "".
 IF tg-paper AND tg-board THEN v-mattype = "B,P".
 ELSE IF tg-paper THEN v-mattype = "P".
 ELSE IF tg-board THEN v-mattype = "B".


{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE("general").

display "" with frame r-top.

IF tb_excel THEN 
DO:
   OUTPUT STREAM excel TO VALUE(v-excel-file).
   ASSIGN 
      excelheader = "CATEGORY,ITEM,DESCRIPTION,TAG#,LINEAL FEET,MSF,WEIGHT,COST VALUE".  
   PUT STREAM excel UNFORMATTED excelheader SKIP.
END.

     for each rm-rcpth
         where rm-rcpth.company                 eq cocode
           and rm-rcpth.i-no                    ge v-fitem
           and rm-rcpth.i-no                    le v-titem
           and rm-rcpth.trans-date              ge v-fdate
           and rm-rcpth.trans-date              le v-tdate
           and index(caps(v-type),rm-rcpth.rita-code) gt 0
         use-index i-no no-lock,

         each rm-rdtlh
         where rm-rdtlh.r-no      eq rm-rcpth.r-no
           and rm-rdtlh.rita-code eq rm-rcpth.rita-code
           and rm-rdtlh.loc                     ge v-floc
           and rm-rdtlh.loc                     le v-tloc
         no-lock,

         first item
         where item.company eq cocode
           and item.i-no    eq rm-rcpth.i-no
           and item.procat                      ge v-fpcat
           and item.procat                      le v-tpcat
           AND lookup(ITEM.mat-type,v-mattype) > 0 NO-LOCK
         break by item.procat BY rm-rcpth.i-no BY rm-rdtlh.tag :

         {custom/statusMsg.i "'Processing Item # ' + string(item.i-no)"}

      assign
       v-job-no = fill(" ",6 - length(trim(rm-rdtlh.job-no))) +
                  trim(rm-rdtlh.job-no) + "-" + string(rm-rdtlh.job-no2,"99")
       v-value  = rm-rdtlh.cost * rm-rdtlh.qty.

      if v-job-no begins "-" then v-job-no = "".
      RUN calc-msf (OUTPUT v-msf-qty).

      ACCUMULATE v-msf-qty (TOTAL BY rm-rcpth.i-no).
      ACCUMULATE v-msf-qty (TOTAL BY ITEM.procat).

      IF rd-summary = "D" THEN
         display item.procat
                 rm-rcpth.i-no
                 rm-rcpth.i-name
                 rm-rdtlh.tag FORMAT "X(20)"
                 /*v-job-no*/
                 rm-rdtlh.qty LABEL "Lineal Feet"
                 v-msf-qty LABEL "MSF"
                 ITEM.basis-w LABEL "Weight"
              /*   rm-rdtlh.loc
                 rm-rdtlh.loc-bin                 
                 rm-rdtlh.cost              */
                 v-value LABEL "Cost Value"
          with frame itemx.
      down with frame itemx.

      assign
       v-qty[1] = v-qty[1] + rm-rdtlh.qty
       v-val[1] = v-val[1] + v-value
       v-qty[2] = v-qty[2] + rm-rdtlh.qty
       v-val[2] = v-val[2] + v-value
       v-qty[3] = v-qty[3] + rm-rdtlh.qty
       v-val[3] = v-val[3] + v-value.

      IF  rd-summary = "D" AND tb_excel THEN 
        PUT STREAM excel UNFORMATTED
          item.procat ","
          rm-rcpth.i-no ","
          rm-rcpth.i-name ","
          rm-rdtlh.tag ","
          rm-rdtlh.qty ","
          v-msf-qty ","
          ITEM.basis-w ","
           v-value SKIP .

      IF LAST-OF(rm-rcpth.i-no) AND tg-subtot-item THEN DO:
        IF rd-summary <> "S" THEN
           underline item.procat
                   rm-rcpth.i-no
                   rm-rcpth.i-name
                   rm-rdtlh.qty
                   v-msf-qty
                   v-value
              with frame itemx.
         IF NOT rd-summary = "S" THEN
             down 1 with frame itemx.
         display ITEM.procat 
                  rm-rcpth.i-no
                 "Item Total"  WHEN rd-summary <> "S" @ rm-rcpth.i-name
                  v-qty[1]       @ rm-rdtlh.qty
                  ACCUM TOTAL BY rm-rcpth.i-no v-msf-qty @ v-msf-qty 
                  v-val[1]       @ v-value
              with frame itemx.
        IF NOT rd-summary = "S" THEN
            down 2 with frame itemx.
        IF  rd-summary = "s" AND tb_excel THEN
            PUT STREAM excel UNFORMATTED
              item.procat  ","
               rm-rcpth.i-no ","
                ""         ","
                ""         ","
              v-qty[1]  ","
              ACCUM TOTAL BY rm-rcpth.i-no v-msf-qty   ","
              ""  ","
              v-val[1]   ","
              SKIP .

         ASSIGN 
             v-qty[1] = 0
             v-val[1] = 0.
      END.

      if last-of(item.procat) AND (tg-subtot OR rd-summary = "S")
      then do:
        IF rd-summary <> "S" THEN
           underline item.procat
                   rm-rcpth.i-no
                   rm-rcpth.i-name
                   rm-rdtlh.qty
                   v-msf-qty
                   v-value
              with frame itemx.
         IF NOT rd-summary = "S" THEN
            down 1 with frame itemx.
         display ITEM.procat WHEN rd-summary = "S"
                 "Category Total"  WHEN rd-summary <> "S" @ rm-rcpth.i-name
                  v-qty[2]       @ rm-rdtlh.qty
                  ACCUM TOTAL BY ITEM.procat v-msf-qty @ v-msf-qty 
                  v-val[2]       @ v-value
              with frame itemx.
         IF NOT rd-summary = "S" THEN
            down 2 with frame itemx.
        IF  rd-summary = "s" AND tb_excel THEN
        PUT STREAM excel UNFORMATTED
          item.procat  ","
            ""         ","
            ""         ","
            ""         ","
          v-qty[2]  ","
          ACCUM TOTAL BY ITEM.procat v-msf-qty   ","
          ""  ","
          v-val[2]   ","
          SKIP .


         ASSIGN v-qty[2] = 0
                v-val[2] = 0.


      end.

      if last(item.procat) then do:
        underline item.procat
                   rm-rcpth.i-no
                   rm-rcpth.i-name
                   rm-rdtlh.qty
                   v-msf-qty
                   v-value
              with frame itemx.

        display "GRAND TOTALS" @ rm-rcpth.i-name
                v-qty[3]       @ rm-rdtlh.qty
                ACCUM TOTAL v-msf-qty @ v-msf-qty 
                v-val[3]       @ v-value
              with frame itemx.
      end.
END. /* for each */
OUTPUT CLOSE.

IF tb_excel THEN 
DO:
   OUTPUT STREAM excel CLOSE.
   IF tb_runExcel THEN
   OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(v-excel-file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

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

