&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ce-ctrl.w.w

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
DEF VAR list-name AS cha NO-UNDO.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecdt.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}
DEFINE VARIABLE hdPriceProcs  AS HANDLE.

RUN oe/PriceProcs.p PERSISTENT SET hdPriceProcs.
ASSIGN
 cocode = gcompany
 locode = gloc.

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF NEW SHARED VAR v-fr-tax LIKE oe-ctrl.f-tax.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR security-flag AS LOG NO-UNDO.

DEF STREAM excel.

DEFINE TEMP-TABLE ExtList NO-UNDO
  FIELD ord-no       LIKE oe-ord.ord-no
  FIELD est-no       LIKE oe-ord.est-no 
  FIELD job-no       AS CHARACTER
  FIELD ord-date     LIKE oe-ord.ord-date
  FIELD cust-no      LIKE oe-ord.cust-no 
  FIELD cust-name    LIKE oe-ord.cust-name 
  FIELD i-no         LIKE oe-ordl.i-no     /* misc charge */
  FIELD i-name       LIKE oe-ordl.i-name   /* description */
  FIELD qty-lft      LIKE oe-ordl.qty 
  FIELD cost         LIKE oe-ordl.cost 
  FIELD price        LIKE oe-ordl.price
  FIELD pr-uom       LIKE oe-ordl.pr-uom
  FIELD ext-price    AS CHARACTER  /* price */
  FIELD margin       AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_ord-no end_ord-no ~
begin_cust-no end_cust-no begin_i-no end_i-no begin_ord-date end_ord-date ~
rd_qty tb_contr rd-dest lv-ornt lv-font-no lines-per-page td-show-parm ~
tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_ord-no end_ord-no begin_cust-no ~
end_cust-no begin_i-no end_i-no begin_ord-date end_ord-date lbl_qty rd_qty ~
tb_contr rd-dest lv-ornt lv-font-no lines-per-page lv-font-name ~
td-show-parm tb_excel tb_runExcel fi_file 

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

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Order Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Beginning Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Order Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 
     LABEL "Ending Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-booko#.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_qty AS CHARACTER FORMAT "X(256)":U INITIAL "Print Quantity Ordered or Remaining?" 
     VIEW-AS FILL-IN 
     SIZE 37 BY .95 NO-UNDO.

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

DEFINE VARIABLE rd_qty AS CHARACTER INITIAL "Ordered" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ordered", "Ordered",
"Remaining", "Remaining"
     SIZE 28 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.67.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.76.

DEFINE VARIABLE tb_contr AS LOGICAL INITIAL NO 
     LABEL "Print Contribution?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .95 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL YES 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL NO 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_ord-no AT ROW 2.91 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_ord-no AT ROW 2.91 COL 69 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     begin_cust-no AT ROW 3.86 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 3.86 COL 69 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_i-no AT ROW 4.81 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_i-no AT ROW 4.81 COL 69 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_ord-date AT ROW 5.76 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Order Date"
     end_ord-date AT ROW 5.76 COL 69 COLON-ALIGNED HELP
          "Enter Ending Order Date"
     lbl_qty AT ROW 7.67 COL 13 COLON-ALIGNED NO-LABEL
     rd_qty AT ROW 7.67 COL 52 NO-LABEL
     tb_contr AT ROW 8.86 COL 35
     rd-dest AT ROW 11.95 COL 6 NO-LABEL
     lv-ornt AT ROW 12.19 COL 28 NO-LABEL
     lv-font-no AT ROW 14.1 COL 30 COLON-ALIGNED
     lines-per-page AT ROW 14.1 COL 82 COLON-ALIGNED
     lv-font-name AT ROW 15.29 COL 30 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 16.48 COL 32
     tb_excel AT ROW 17.67 COL 52.2 RIGHT-ALIGNED
     tb_runExcel AT ROW 17.67 COL 74.2 RIGHT-ALIGNED
     fi_file AT ROW 18.62 COL 30.2 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 20.71 COL 19
     btn-cancel AT ROW 20.71 COL 58
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11.24 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 10.76 COL 1
     RECT-7 AT ROW 1.1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 21.57.


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
         TITLE              = "Orders Booked By Order No"
         HEIGHT             = 21.81
         WIDTH              = 95.8
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = YES
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
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
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ord-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ord-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ord-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ord-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_qty IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_qty:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_qty".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_qty:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_contr:PRIVATE-DATA IN FRAME FRAME-A     = 
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
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Orders Booked By Order No */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Orders Booked By Order No */
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
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no C-Win
ON LEAVE OF begin_i-no IN FRAME FRAME-A /* Beginning Item# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-date C-Win
ON LEAVE OF begin_ord-date IN FRAME FRAME-A /* Beginning Order Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-no C-Win
ON LEAVE OF begin_ord-no IN FRAME FRAME-A /* Beginning Order# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
   APPLY "close" TO THIS-PROCEDURE.
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

  RUN run-report.
  STATUS DEFAULT "Processing Complete". 
  CASE rd-dest:
       WHEN 1 THEN RUN output-to-printer.
       WHEN 2 THEN RUN output-to-screen.
       WHEN 3 THEN RUN output-to-file.
       WHEN 4 THEN DO:
           /*run output-to-fax.*/
           {custom/asifax.i &type= "Customer"
                            &begin_cust=begin_cust-no
                            &END_cust= begin_cust-no
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       WHEN 5 THEN DO:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = "Customer"
                             &begin_cust= begin_cust-no
                             &END_cust=begin_cust-no
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Customer"
                                  &begin_cust= begin_cust-no
                                  &END_cust=begin_cust-no
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.
       END. 
      WHEN 6 THEN RUN output-to=port.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no C-Win
ON LEAVE OF end_i-no IN FRAME FRAME-A /* Ending Item# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-date C-Win
ON LEAVE OF end_ord-date IN FRAME FRAME-A /* Ending Order Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-no C-Win
ON LEAVE OF end_ord-no IN FRAME FRAME-A /* Ending Order# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-A /* Lines Per Page */
DO:
  ASSIGN {&self-name}.
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
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_qty C-Win
ON VALUE-CHANGED OF rd_qty IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_contr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_contr C-Win
ON VALUE-CHANGED OF tb_contr IN FRAME FRAME-A /* Print Contribution? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-A /* Show Parameters? */
DO:
    ASSIGN {&self-name}.
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
   begin_ord-date = TODAY
   end_ord-date   = TODAY.

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO begin_ord-no.
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
  DISPLAY begin_ord-no end_ord-no begin_cust-no end_cust-no begin_i-no end_i-no 
          begin_ord-date end_ord-date lbl_qty rd_qty tb_contr rd-dest lv-ornt 
          lv-font-no lines-per-page lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_ord-no end_ord-no begin_cust-no end_cust-no 
         begin_i-no end_i-no begin_ord-date end_ord-date rd_qty tb_contr 
         rd-dest lv-ornt lv-font-no lines-per-page td-show-parm tb_excel 
         tb_runExcel fi_file btn-ok btn-cancel 
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
  /*   DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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

     IF NOT OKpressed THEN  RETURN NO-APPLY.
*/
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
  RUN custom\d-print.w (list-name).

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
  RUN scr-rpt.w (list-name,c-win:TITLE,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------ oe/rep/orders.p 11/99 JLF */
/* Orders Report                                                              */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}

DEF BUFFER b-oe-ordl FOR oe-ordl.

DEF VAR v-cust LIKE oe-ord.cust-no EXTENT 2 INIT ["","zzzzzzzz"].
DEF VAR v-ord-no AS INT FORMAT ">>>>>>" EXTENT 2 INIT [0,999999].
DEF VAR v-date AS DATE FORMAT "99/99/9999" EXTENT 2 INIT [TODAY,TODAY].
DEF VAR v-item AS CHAR FORMAT "x(15)" EXTENT 2 INIT ["","zzzzzzzzzzzzzzz"].
DEF VAR v-ord-qty AS LOG FORMAT "Ordered/Remaining" INIT YES.

DEF VAR v-unline AS CHAR FORMAT "x(80)" INIT
  "--------------- ------------------------- ------- ----------- ---".
DEF VAR v-tot-ord AS DEC FORMAT "->,>>>,>>9.99" EXTENT 2.
DEF VAR v-tax-rate     AS DEC FORMAT ">,>>9.99<<<".
DEF VAR v-inv AS LOG INIT NO.
DEF VAR v-ship AS LOG INIT NO.
DEF VAR v-tot-tax LIKE oe-ord.tax.
DEF VAR v-tot-freight LIKE oe-ord.t-freight.
DEF VAR v-qty-lft LIKE oe-ordl.qty.
DEF VAR v-ext-price LIKE oe-ordl.t-price.
DEF VAR v-prt-cont AS LOG INIT NO.
DEF VAR v-margin AS DEC.
DEF VAR v-margin-tot AS DEC FORMAT "->>>,>>>,>>>,>>9.99".
DEF VAR v-password LIKE sys-ctrl.char-fld LABEL "Please Enter Password".
DEF VAR v-ext-cost AS DEC.
DEF VAR excelheader AS CHAR NO-UNDO.
DEFINE VARIABLE cFileName LIKE fi_file NO-UNDO .

RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .

FORMAT HEADER
  "Order#  Est#         Job#     Date       Cust#    Name" SKIP
  FILL("-",105) FORMAT "x(105)"
  WITH FRAME r-top .

FORMAT  /* frame ord */
   oe-ord.ord-no
   oe-ord.est-no FORMAT "x(8)"
   oe-ord.job-no SPACE(0) "-" SPACE(0)
   oe-ord.job-no2 FORMAT "99"
   SPACE(3) 
   oe-ord.ord-date
   SPACE(3) 
   oe-ord.cust-no 
   oe-ord.cust-name SKIP
  WITH FRAME ord NO-LABELS NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132.

FORMAT  /* frame ord1 */
    oe-ordl.i-no                          LABEL "Item"  AT 10
    oe-ordl.i-name FORMAT "x(25)"         LABEL "Description"
    v-qty-lft      FORMAT "->>>,>>9"      LABEL "Quantity"
    oe-ordl.cost   FORMAT "->>>,>>9.99"   LABEL "Cost/M"
    oe-ordl.price  FORMAT "->>>,>>9.99"   LABEL "Price"
    oe-ordl.pr-uom                        LABEL "UOM"
    v-ext-price    FORMAT "->,>>>,>>9.99" LABEL "Ext Price"
    v-margin       FORMAT "->,>>>,>>9.99" LABEL "Margin$"
  WITH FRAME ordl NO-LABELS NO-BOX DOWN STREAM-IO WIDTH 132.

FORMAT /* frame ordl2 */
    oe-ordl.i-no                          LABEL "Item"  AT 10
    oe-ordl.i-name FORMAT "x(25)"         LABEL "Description"
    v-qty-lft      FORMAT "->>>,>>9"      LABEL "Quantity"
    oe-ordl.cost   FORMAT "->>>,>>9.99"   LABEL "Cost/M"
    oe-ordl.price  FORMAT "->>>,>>9.99"   LABEL "Price"
    oe-ordl.pr-uom                        LABEL "UOM"
    v-ext-price    FORMAT "->,>>>,>>9.99" LABEL "Ext Price"
  WITH FRAME ordl2  NO-LABELS NO-BOX DOWN STREAM-IO WIDTH 132.

FORMAT /* frame ordm */
    oe-ordm.charge                       LABEL "Charge"      AT 10
    oe-ordm.dscr                         LABEL "Description"
    oe-ordm.amt    FORMAT "->>>,>>9.99"  LABEL "Price"       TO 102 SKIP
  WITH FRAME ordm NO-LABELS NO-BOX DOWN STREAM-IO WIDTH 132.

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK.
v-fr-tax = oe-ctrl.f-tax.

ASSIGN
 str-tit2 = c-win:TITLE
 {sys/inc/ctrtext.i str-tit2 112}

 v-ord-no[1] = begin_ord-no
 v-ord-no[2] = end_ord-no
 v-cust[1]   = begin_cust-no
 v-cust[2]   = end_cust-no
 v-item[1]   = begin_i-no
 v-item[2]   = end_i-no
 v-date[1]   = begin_ord-date
 v-date[2]   = end_ord-date
 v-ord-qty   = rd_qty EQ "Ordered"
 v-prt-cont  = tb_contr.

IF v-prt-cont AND NOT security-flag THEN DO:
  RUN sys/ref/d-passwd.w (3, OUTPUT security-flag).
  v-prt-cont = security-flag.
END.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF td-show-parm THEN RUN show-param.

IF tb_excel THEN 
DO:
   excelheader = "Order#,Est#,Job#,Date,Cust,Name,".
   IF v-prt-cont THEN
     excelheader = excelheader + "Item/Misc Chg,Description,Quantity,Cost/M,Price,UOM,Ext Price,Margin$".
   ELSE
     excelheader = excelheader + "Item/Misc Chg,Description,Quantity,Cost/M,Price,UOM,Ext Price".

  OUTPUT STREAM excel TO VALUE(cFileName).

  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

SESSION:SET-WAIT-STATE ("general").

DISPLAY "" WITH FRAME r-top.

EMPTY TEMP-TABLE ExtList.

FOR EACH oe-ord
    WHERE oe-ord.company  EQ cocode
      AND oe-ord.ord-no   GE v-ord-no[1]
      AND oe-ord.ord-no   LE v-ord-no[2]
      AND oe-ord.cust-no  GE v-cust[1]
      AND oe-ord.cust-no  LE v-cust[2]
      AND oe-ord.ord-date GE v-date[1]
      AND oe-ord.ord-date LE v-date[2]
      AND oe-ord.stat     NE "D"
      AND oe-ord.type     NE "T"
    USE-INDEX ord-no NO-LOCK,

    FIRST b-oe-ordl
    WHERE b-oe-ordl.company EQ cocode
      AND b-oe-ordl.ord-no  EQ oe-ord.ord-no
      AND b-oe-ordl.i-no    GE v-item[1]
      AND b-oe-ordl.i-no    LE v-item[2]
    NO-LOCK,

    FIRST cust
    {sys/ref/custW.i}
      AND cust.cust-no EQ oe-ord.cust-no
    NO-LOCK

    BREAK BY oe-ord.ord-no:
     {custom/statusMsg.i "'Processing Order # ' + string(b-oe-ordl.ord-no)"} 
    DISPLAY 
        oe-ord.ord-no
        TRIM(oe-ord.est-no)   @ oe-ord.est-no
        oe-ord.job-no
        oe-ord.job-no2
        oe-ord.ord-date       FORMAT "99/99/9999"
        oe-ord.cust-no
        oe-ord.cust-name
      WITH FRAME ord.

    v-tot-ord[1] = 0.

    FOR EACH oe-ordl
       WHERE oe-ordl.company EQ oe-ord.company
         AND oe-ordl.ord-no  EQ oe-ord.ord-no
         AND oe-ordl.i-no    GE v-item[1]
         AND oe-ordl.i-no    LE v-item[2]
      NO-LOCK
      BREAK BY oe-ordl.ord-no:

        IF FIRST-OF(oe-ordl.ord-no) THEN PUT SKIP(1).

        ASSIGN
          v-ship      = oe-ordl.stat NE "I" AND oe-ordl.stat NE "B"
          v-qty-lft   = oe-ordl.qty - (IF v-ord-qty THEN 0 ELSE oe-ordl.inv-qty)
          v-ext-price = 0.

        IF v-qty-lft LT 0 THEN v-qty-lft = 0.  

         FIND FIRST itemfg {sys/look/itemfgrlW.i} 
                   AND itemfg.i-no EQ oe-ordl.i-no NO-LOCK NO-ERROR.

        RUN Conv_CalcTotalPrice (oe-ordl.company,
                                 oe-ordl.i-no,
                                 oe-ordl.qty,
                                 oe-ordl.price,
                                 oe-ordl.pr-uom,
                                 oe-ordl.disc,
                                 itemfg.case-count,
                                 OUTPUT v-ext-price).

       /** CALCULATE FREIGHT CHARGES **/
        v-tot-freight = v-tot-freight +
                    (ROUND(oe-ordl.t-freight / oe-ordl.qty, 2) * v-qty-lft).

       /** CALCULATE TAX CHARGES **/
        IF oe-ordl.tax AND v-tax-rate GT 0 THEN
          v-tot-tax = v-tot-tax + round((v-ext-price * v-tax-rate) / 100,2).

        IF v-prt-cont THEN 
          ASSIGN
            v-ext-cost = (oe-ordl.cost * oe-ordl.qty) / 1000
            v-margin = v-ext-price - v-ext-cost.

        IF v-prt-cont THEN 
        DO:
            DISPLAY 
                oe-ordl.i-no
                oe-ordl.i-name
                v-qty-lft
                oe-ordl.cost
                oe-ordl.price
                oe-ordl.pr-uom
                v-ext-price
                v-margin 
              WITH FRAME ordl.
            DOWN WITH FRAME ordl.

            IF tb_excel THEN
            DO:

               CREATE ExtList.
               ASSIGN
                 ExtList.ord-no    = oe-ord.ord-no
                 ExtList.est-no    = TRIM(oe-ord.est-no)
                 ExtList.job-no    = TRIM(oe-ord.job-no) + "-" 
                                    + TRIM(STRING(oe-ord.job-no2, "99"))
                 ExtList.ord-date  = oe-ord.ord-date
                 ExtList.cust-no   = oe-ord.cust-no
                 ExtList.cust-name = oe-ord.cust-name
                 ExtList.i-no      = oe-ordl.i-no
                 ExtList.i-name    = oe-ordl.i-name
                 ExtList.qty-lft   = v-qty-lft
                 ExtList.cost      = oe-ordl.cost
                 ExtList.price     = oe-ordl.price
                 ExtList.pr-uom    = oe-ordl.pr-uom
                 ExtList.ext-price = TRIM(STRING(v-ext-price))
                 ExtList.margin    = TRIM(STRING(v-margin)).
            END.
        END.
        ELSE 
        DO:
            DISPLAY 
                oe-ordl.i-no
                oe-ordl.i-name
                v-qty-lft
                oe-ordl.cost
                oe-ordl.price
                oe-ordl.pr-uom
                v-ext-price
              WITH FRAME ordl2.
            DOWN WITH FRAME ordl2.

            IF tb_excel THEN
            DO:
               CREATE ExtList.
               ASSIGN
                 ExtList.ord-no    = oe-ord.ord-no
                 ExtList.est-no    = TRIM(oe-ord.est-no)
                 ExtList.job-no    = TRIM(oe-ord.job-no) + "-" 
                                    + TRIM(STRING(oe-ord.job-no2, "99"))
                 ExtList.ord-date  = oe-ord.ord-date
                 ExtList.cust-no   = oe-ord.cust-no
                 ExtList.cust-name = oe-ord.cust-name
                 ExtList.i-no      = oe-ordl.i-no
                 ExtList.i-name    = oe-ordl.i-name
                 ExtList.qty-lft   = v-qty-lft
                 ExtList.cost      = oe-ordl.cost
                 ExtList.price     = oe-ordl.price
                 ExtList.pr-uom    = oe-ordl.pr-uom
                 ExtList.ext-price = TRIM(STRING(v-ext-price)).
            END.
        END.     

        IF v-prt-cont THEN 
        DO:
            IF v-margin NE ? THEN 
              v-margin-tot = v-margin-tot + v-margin.
        END.

        v-tot-ord[1] = v-tot-ord[1] + v-ext-price.
    END. /* each oe-ordl */

    FOR EACH oe-ordm
       WHERE oe-ordm.company EQ oe-ord.company
         AND oe-ordm.ord-no  EQ oe-ord.ord-no
       NO-LOCK
      BREAK BY oe-ordm.ord-no:

        IF FIRST-OF(oe-ordm.ord-no) THEN PUT SKIP(1) "Miscellaneous" AT 10 SKIP.

        DISPLAY 
            oe-ordm.charge oe-ordm.dscr oe-ordm.amt
          WITH FRAME ordm.

        IF oe-ordm.bill EQ "N" THEN
          DISPLAY "       N/C" @ oe-ordm.amt WITH FRAME ordm.

        DOWN WITH FRAME ordm.

        IF tb_excel THEN
        DO:
            CREATE ExtList.
            ASSIGN
              ExtList.ord-no    = oe-ord.ord-no
              ExtList.est-no    = TRIM(oe-ord.est-no)
              ExtList.job-no    = TRIM(oe-ord.job-no) + "-" 
                                  + TRIM(STRING(oe-ord.job-no2, "99"))
              ExtList.ord-date  = oe-ord.ord-date
              ExtList.cust-no   = oe-ord.cust-no
              ExtList.cust-name = oe-ord.cust-name
              ExtList.i-no      = oe-ordm.charge
              ExtList.i-name    = oe-ordm.dscr
              ExtList.ext-price = IF oe-ordm.bill EQ "N"
                                  THEN "N/C"  
                                  ELSE TRIM(STRING(oe-ordm.amt)).
        END.

        IF oe-ordm.bill EQ "Y" THEN 
        DO:
            v-tot-ord[1] = v-tot-ord[1] + oe-ordm.amt.

            IF oe-ordm.tax AND v-tax-rate EQ 0 THEN
              v-tot-tax = v-tot-tax + round((oe-ordm.amt * v-tax-rate) / 100,2).
        END.
    END. /* each oe-ordm */

    PUT SKIP "------------" TO 102 SKIP.

    IF oe-ord.stat EQ "H" THEN
      PUT "** THIS ORDER IS ON CREDIT HOLD **" TO 50 .
    ELSE IF oe-ord.priceHold THEN
      PUT "** THIS ORDER IS ON PRICE HOLD **" TO 50 .
    ELSE IF oe-ord.stat EQ "D" THEN
      PUT "** THIS ORDER IS DELETED **" TO 50 .
    ELSE IF oe-ord.stat EQ "C" THEN
      PUT "** THIS ORDER IS CLOSED **" TO 50 .

    IF tb_excel AND LOOKUP(oe-ord.stat, "H,D,C") NE 0 OR oe-ord.priceHold THEN
    DO:
        CREATE ExtList.
        ASSIGN
          ExtList.ord-no    = oe-ord.ord-no
          ExtList.est-no    = TRIM(oe-ord.est-no)
          ExtList.job-no    = TRIM(oe-ord.job-no) + "-" 
                              + TRIM(STRING(oe-ord.job-no2, "99"))
          ExtList.ord-date  = oe-ord.ord-date
          ExtList.cust-no   = oe-ord.cust-no
          ExtList.cust-name = oe-ord.cust-name.

        IF oe-ord.priceHold THEN
            ExtList.i-name =  "** THIS ORDER IS ON PRICE HOLD **".
        ELSE 
            CASE oe-ord.stat:
                WHEN "H" THEN
                  ExtList.i-name  = "** THIS ORDER IS ON CREDIT HOLD **".
                WHEN "D" THEN
                   ExtList.i-name = "** THIS ORDER IS DELETED **".
                WHEN "C" THEN
                   ExtList.i-name = "** THIS ORDER IS CLOSED **".
            END CASE.
    END.

    PUT "Total Order" AT 75 v-tot-ord[1] TO 102 SKIP(1).

    v-tot-ord[2] = v-tot-ord[2] + v-tot-ord[1].

    IF LAST(oe-ord.ord-no) THEN 
    DO:
        IF v-prt-cont THEN 
        DO:
            PUT SKIP(2) "Grand Total" AT 75 v-tot-ord[2] TO 112 SKIP.
            PUT "Grand Total Contribution" AT 62 v-margin-tot TO 112 SKIP(1).
        END.
        ELSE 
        DO:
            PUT SKIP(2) "Grand Total" AT 75 v-tot-ord[2] TO 102 SKIP(1).
        END.
    END.
END. /* each oe-ord */

/* if they're not here, they don't go.. */
FOR EACH ExtList:
    EXPORT STREAM excel DELIMITER "," Extlist.
END.

IF tb_excel THEN 
DO:
    OUTPUT STREAM excel CLOSE.

    IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(cFileName)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

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
  DEF VAR lv-frame-hdl AS HANDLE NO-UNDO.
  DEF VAR lv-group-hdl AS HANDLE NO-UNDO.
  DEF VAR lv-field-hdl AS HANDLE NO-UNDO.
  DEF VAR lv-field2-hdl AS HANDLE NO-UNDO.
  DEF VAR parm-fld-list AS cha NO-UNDO.
  DEF VAR parm-lbl-list AS cha NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR lv-label AS cha.

  lv-frame-hdl = FRAME {&frame-name}:handle.
  lv-group-hdl = lv-frame-hdl:FIRST-CHILD.
  lv-field-hdl = lv-group-hdl:FIRST-CHILD .

  DO WHILE TRUE:
     IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
     IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0
        THEN DO:
           IF lv-field-hdl:LABEL <> ? THEN 
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + "," 
                     .
           ELSE DO:  /* radio set */
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     .
              lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
              REPEAT:
                  IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                  IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN DO:
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:SCREEN-VALUE + ",".
                  END.
                  lv-field2-hdl = lv-field2-hdl:NEXT-SIBLING.                 
              END.       
           END.                 
        END.            
     lv-field-hdl = lv-field-hdl:NEXT-SIBLING.   
  END.

  PUT SPACE(28)
      "< Selection Parameters >"
      SKIP(1).

  DO i = 1 TO NUM-ENTRIES(parm-fld-list,","):
    IF ENTRY(i,parm-fld-list) NE "" OR
       entry(i,parm-lbl-list) NE "" THEN DO:

      lv-label = FILL(" ",34 - length(TRIM(ENTRY(i,parm-lbl-list)))) +
                 trim(ENTRY(i,parm-lbl-list)) + ":".

      PUT lv-label FORMAT "x(35)" AT 5
          SPACE(1)
          TRIM(ENTRY(i,parm-fld-list)) FORMAT "x(40)"
          SKIP.              
    END.
  END.

  PUT FILL("-",80) FORMAT "x(80)" SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

