&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-quoprt.w

  Description: Quote Printing

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: JLF

  Created: 09/20/02

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
DEF INPUT PARAMETER ip-mach AS cha NO-UNDO.
DEF INPUT PARAM ip-date AS DATE NO-UNDO.
DEF INPUT PARAM ip-end-date AS DATE NO-UNDO.

/* Local Variable Definitions ---                                       */
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{sys/inc/var.i new shared}
ASSIGN cocode = g_company
       locode = g_loc.

DEF TEMP-TABLE tt-color FIELD i-code AS cha . 

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF TEMP-TABLE tt-report FIELD seq AS INT 
                         FIELD REC-id AS RECID
                         FIELD due-date AS DATE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_mach end_mach begin_date ~
end_date tb_print-po tb_print-times tb_wf-board rd_cust rd-dest ~
lines-per-page lv-ornt lv-font-no td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_mach end_mach begin_date end_date ~
tb_print-po tb_print-times tb_wf-board rd_cust rd-dest lines-per-page ~
lv-ornt lv-font-no lv-font-name td-show-parm 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD cvt-hour-to-string C-Win 
FUNCTION cvt-hour-to-string RETURNS CHARACTER
 ( INPUT ip-hour AS dec  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD cvt-time-to-string C-Win 
FUNCTION cvt-time-to-string RETURNS CHARACTER
   (INPUT ip-type AS CHAR, INPUT ip-stime AS INT, INPUT ip-hour AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD jobboard C-Win 
FUNCTION jobboard RETURNS LOGICAL
  ( ipCompany AS CHARACTER, ipJob AS INTEGER,
                                  ipJobNo AS CHARACTER, ipJobNo2 AS INTEGER,
                                  ipForm AS INTEGER )  FORWARD.

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" 
     LABEL "Beginning Start Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_mach AS CHARACTER FORMAT "X(256)" INITIAL "0" 
     LABEL "Beginning Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999" 
     LABEL "Ending Start Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_mach AS CHARACTER FORMAT "X(256)" INITIAL "99999999" 
     LABEL "Ending Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "10" 
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

DEFINE VARIABLE rd_cust AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Name", 1,
"Code", 2
     SIZE 30 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 93 BY 9.05.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 10.24.

DEFINE VARIABLE tb_print-po AS LOGICAL INITIAL no 
     LABEL "Print Board Vendor, PO#, Qty Rec'd ?" 
     VIEW-AS TOGGLE-BOX
     SIZE 53 BY .81 NO-UNDO.

DEFINE VARIABLE tb_print-times AS LOGICAL INITIAL no 
     LABEL "Print Times?  (Start/End/Run/Setup)" 
     VIEW-AS TOGGLE-BOX
     SIZE 46 BY .81 NO-UNDO.

DEFINE VARIABLE tb_wf-board AS LOGICAL INITIAL no 
     LABEL "Include Jobs W/F Board?" 
     VIEW-AS TOGGLE-BOX
     SIZE 55 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_mach AT ROW 2.91 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Quote Number"
     end_mach AT ROW 2.91 COL 69 COLON-ALIGNED HELP
          "Enter Ending QuoteNumber"
     begin_date AT ROW 4.1 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Department"
     end_date AT ROW 4.1 COL 69 COLON-ALIGNED HELP
          "Enter Endng Department"
     tb_print-po AT ROW 6.24 COL 26
     tb_print-times AT ROW 7.19 COL 26
     tb_wf-board AT ROW 8.38 COL 26
     rd_cust AT ROW 9.33 COL 42 NO-LABEL
     rd-dest AT ROW 12.91 COL 7 NO-LABEL
     lines-per-page AT ROW 12.91 COL 84 COLON-ALIGNED
     lv-ornt AT ROW 13.14 COL 32 NO-LABEL
     lv-font-no AT ROW 15.52 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 16.48 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 18.38 COL 32
     btn-ok AT ROW 21.24 COL 21
     btn-cancel AT ROW 21.24 COL 60
     "Print Customer" VIEW-AS TEXT
          SIZE 15 BY .95 AT ROW 9.33 COL 26
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11.71 COL 6
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 11.48 COL 2
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.6 BY 22.1.


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
         TITLE              = "Print Job Schedule Information"
         HEIGHT             = 22.38
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
                                                                        */
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
       begin_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Print Job Schedule Information */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Print Job Schedule Information */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Start Date */
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

  assign {&displayed-objects}.
  IF v-print-fmt EQ "Pacific" OR v-print-fmt EQ "Xprint" OR v-print-fmt = "southpak"
       THEN is-xprint-form = YES.     
  ELSE is-xprint-form = NO.

  run run-report. 

  SESSION:SET-WAIT-STATE ("").

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type= ''
                            &begin_cust= begin_mach
                            &END_cust= begin_mach
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = ''
                             &begin_cust=begin_mach
                             &END_cust=begin_mach
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = ''
                                  &begin_cust=begin_mach
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


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Start Date */
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

  ASSIGN begin_mach = ip-mach
         END_mach = ip-mach
         begin_date = ip-date
         END_date = ip-end-date.

  RUN enable_UI.
  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    ASSIGN begin_mach:SCREEN-VALUE = ip-mach
           END_mach:SCREEN-VALUE = ip-mach
           begin_date:SCREEN-VALUE = string(ip-date)
           END_date:SCREEN-VALUE = string(ip-end-date).
    APPLY "entry" TO begin_mach.
  END.

  {methods/nowait.i}
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
  DISPLAY begin_mach end_mach begin_date end_date tb_print-po tb_print-times 
          tb_wf-board rd_cust rd-dest lines-per-page lv-ornt lv-font-no 
          lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_mach end_mach begin_date end_date tb_print-po 
         tb_print-times tb_wf-board rd_cust rd-dest lines-per-page lv-ornt 
         lv-font-no td-show-parm btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-fax C-Win 
PROCEDURE output-to-fax :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


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

/*
 IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
 END.
 ELSE */
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
  /*IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
  END.
  ELSE    */

  run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* -------------------------------------------- cec/quote/printquo.p 8/94 rd  */
/* print quotes                                                               */
/* -------------------------------------------------------------------------- */
DEF VAR lv-die LIKE eb.die-no NO-UNDO.
DEF VAR lv-plate LIKE eb.plate-no NO-UNDO.
DEF VAR lv-cust-no LIKE eb.cust-no NO-UNDO.
DEF VAR tmp-color AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-start-time LIKE job-mch.start-time NO-UNDO.
DEF VAR lv-seq LIKE job-sch.seq NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR lv-due-date AS DATE NO-UNDO.
DEF VAR lv-po-no LIKE po-ordl.po-no NO-UNDO.
DEF VAR lv-run-hr AS cha NO-UNDO.
DEF VAR lv-run-stime AS cha NO-UNDO.
DEF VAR lv-run-etime AS cha NO-UNDO.
DEF VAR lv-mr-hr AS cha NO-UNDO.
DEF VAR lv-mr-stime AS cha NO-UNDO.
DEF VAR lv-mr-etime AS cha NO-UNDO.

DEF BUFFER bf-job-mch FOR job-mch.
DEF BUFFER bf-job-hdr FOR job-hdr.
DEF BUFFER bf-job FOR job.
DEF VAR lv-board LIKE ef.board NO-UNDO.
DEF VAR li-seq AS INT NO-UNDO.
DEF VAR lv-vend-no LIKE po-ord.vend-no NO-UNDO.
DEF VAR lv-qty-received LIKE po-ordl.t-rec-qty NO-UNDO. /*->>>,>>>,>>9.9<<*/
DEF VAR lv-wf-board AS LOG NO-UNDO.

FOR EACH tt-color.
    DELETE tt-color.
END.
FOR EACH tt-report.
    DELETE tt-report.
END.
DEF VAR lv-machine AS cha NO-UNDO.

FORM HEADER
    "Machine : " lv-machine SKIP(1)
    "Seq#/  Job#    Cust#/                         FG Item#        Due      Board      Vendor   PO#    Qty Received   Start  Setup      End Setup        Start Run         End Run       Run "SKIP
    "Die#           Plate#                                         Date     Colors                                   Date      Time   Date      Time   Date      Time   Date      Time  Hours"SKIP
    "------ ------- ------------------------------ --------------- -------- ---------- -------- ------ ------------ ---------------- ---------------- ---------------- ---------------- ------"
    WITH FRAME pg-hd NO-BOX NO-LABEL PAGE-TOP WIDTH 220 STREAM-IO.

{sys/form/r-top3w.f}

assign
 str-tit2 = c-win:TITLE                    
 {sys/inc/ctrtext.i str-tit2 110}.

{sys/inc/print1.i}
{sys/inc/outprint.i value(lines-per-page)}
if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").
/*
DISPLAY WITH FRAME r-top.
DISP WITH FRAME pg-hd.
*/
VIEW FRAME r-top.
VIEW FRAME pg-hd.

FOR EACH bf-job-mch WHERE bf-job-mch.company = g_company
                         AND bf-job-mch.m-code >= begin_mach
                         AND bf-job-mch.m-code <= end_mach
                         AND bf-job-mch.start-date >= begin_date
                         AND bf-job-mch.start-date <= end_date NO-LOCK,      
      FIRST bf-job-hdr WHERE bf-job-hdr.company = bf-job-mch.company
                          AND bf-job-hdr.job = bf-job-mch.job NO-LOCK ,
      FIRST bf-job WHERE bf-job.company = bf-job-hdr.company
                         AND bf-job.job = bf-job-hdr.job
                       /*  AND (bf-job-mch.start-date >= lv-start-date or lv-start-date = ?) */
                         AND bf-job.opened NO-LOCK      
           /*    BREAK BY bf-job-mch.start-date 
                     BY bf-job-mch.seq-no:*/
               BREAK BY bf-job-mch.seq BY bf-job-mch.m-code BY bf-job-mch.start-date  :
     li-seq = 99999.
     FIND FIRST oe-ord WHERE oe-ord.company EQ bf-job-mch.company
                    AND oe-ord.job-no  EQ bf-job-mch.job-no
                    AND oe-ord.job-no2 EQ bf-job-mch.job-no2 no-lock no-error.
     lv-due-date = IF AVAIL oe-ord THEN oe-ord.due-date ELSE ?.
     FIND FIRST oe-ordl WHERE oe-ordl.company EQ bf-job-mch.company
                    AND oe-ordl.job-no  EQ bf-job-mch.job-no
                    AND oe-ordl.job-no2 EQ bf-job-mch.job-no2 
                    AND oe-ordl.i-no = bf-job-hdr.i-no  no-lock no-error.
     lv-due-date =  IF AVAIL oe-ordl THEN oe-ordl.prom-date ELSE ?. /* oe-ordl.req-date */

     CREATE tt-report.
     ASSIGN tt-report.seq = IF bf-job-mch.seq = 0 THEN li-seq ELSE bf-job-mch.seq
            tt-report.REC-ID = RECID (bf-job-mch)
            tt-report.due-date = lv-due-date.
END.

FOR EACH tt-report,
      EACH bf-job-mch NO-LOCK WHERE /*bf-job-mch.company = g_company
                         AND bf-job-mch.m-code >= begin_mach
                         AND bf-job-mch.m-code <= end_mach
                         AND bf-job-mch.start-date >= begin_date
                         AND bf-job-mch.start-date <= end_date */
                          recid(bf-job-mch) = tt-report.rec-id
                         /*AND (tb_wf-board OR
                              jobBoard(bf-job-mch.company,bf-job-mch.job,bf-job-mch.job-no,bf-job-mch.job-no2,bf-job-mch.frm))*/ ,      

      FIRST bf-job-hdr WHERE bf-job-hdr.company = bf-job-mch.company
                          AND bf-job-hdr.job = bf-job-mch.job NO-LOCK ,
      FIRST bf-job WHERE bf-job.company = bf-job-hdr.company
                         AND bf-job.job = bf-job-hdr.job
                         /*AND (bf-job-mch.start-date >= lv-start-date or lv-start-date = ?) */
                         AND bf-job.opened NO-LOCK      
           /*    BREAK BY bf-job-mch.start-date 
                     BY bf-job-mch.seq-no:*/
               BREAK BY bf-job-mch.m-code BY tt-report.seq  /*BY bf-job-mch.start-date*/ BY tt-report.due-date  :

        IF NOT tb_wf-board AND NOT jobBoard(bf-job-mch.company,bf-job-mch.job,bf-job-mch.job-no,bf-job-mch.job-no2,bf-job-mch.frm)
        THEN NEXT.

      /*
      IF v-board <> "" AND NOT can-find(FIRST job-mat WHERE job-mat.company = mach.company
                                         AND job-mat.job-no = bf-job-hdr.job-no
                                         AND job-mat.job-no2 = bf-job-hdr.job-no2  
                                         AND job-mat.i-no BEGINS v-board )
      THEN NEXT.

      IF v-color <> "" AND NOT can-find(FIRST job-mat WHERE job-mat.company = mach.company
                                         AND job-mat.job-no = bf-job-hdr.job-no
                                         AND job-mat.job-no2 = bf-job-hdr.job-no2  
                                         AND job-mat.i-no BEGINS v-color )
      THEN NEXT.

      IF v-plate <> "" AND NOT can-find(FIRST job-prep WHERE job-prep.company = mach.company
                                         AND job-prep.job-no = bf-job-hdr.job-no
                                         AND job-prep.job-no2 = bf-job-hdr.job-no2  
                                         AND job-prep.code BEGINS v-plate)
      THEN next.     
      IF v-die <> "" AND NOT can-find(FIRST job-prep WHERE job-prep.company = mach.company
                                         AND job-prep.job-no = bf-job-hdr.job-no
                                         AND job-prep.job-no2 = bf-job-hdr.job-no2  
                                         AND job-prep.CODE BEGINS v-die)
      THEN NEXT.
      */
      /*
      FIND FIRST oe-ord WHERE oe-ord.company EQ bf-job-mch.company
                          AND oe-ord.job-no  EQ bf-job-mch.job-no
                          AND oe-ord.job-no2 EQ bf-job-mch.job-no2 no-lock no-error.

      lv-due-date =     IF AVAIL oe-ord THEN oe-ord.due-date ELSE ?.
      FIND FIRST oe-ordl WHERE oe-ordl.company EQ bf-job-mch.company
                          AND oe-ordl.job-no  EQ bf-job-mch.job-no
                          AND oe-ordl.job-no2 EQ bf-job-mch.job-no2 
                          AND oe-ordl.i-no = bf-job-hdr.i-no  no-lock no-error.
      lv-due-date =     IF AVAIL oe-ordl THEN oe-ordl.prom-date ELSE ?. /* oe-ordl.req-date */
      */
      FIND FIRST ef WHERE ef.company = bf-job-hdr.company
                       AND ef.est-no = bf-job-hdr.est-no
                       AND ef.form-no = bf-job-mch.frm
                       NO-LOCK NO-ERROR.
      lv-board = IF AVAIL ef THEN ef.board ELSE ''.
      find first po-ordl where po-ordl.company eq bf-job-hdr.company            
                           and po-ordl.job-no  eq bf-job-mch.job-no
                           and po-ordl.job-no2 eq bf-job-mch.job-no2
                           and po-ordl.s-num   eq bf-job-mch.frm
                           and po-ordl.i-no    eq lv-board /*bf-job-mch.i-no*/
                           NO-LOCK NO-ERROR.
      IF AVAIL po-ordl THEN
         FIND FIRST po-ord WHERE
              po-ord.company EQ po-ordl.company AND
              po-ord.po-no EQ po-ordl.po-no
              NO-LOCK NO-ERROR.

      ASSIGN
       lv-po-no = IF AVAIL po-ordl THEN po-ordl.po-no ELSE 0
       lv-qty-received = IF AVAIL po-ordl THEN po-ordl.t-rec-qty ELSE 0
       lv-vend-no = IF AVAIL po-ord THEN po-ord.vend-no ELSE "".


       FIND FIRST eb WHERE eb.company = bf-job-hdr.company
                  AND eb.est-no = bf-job-hdr.est-no
                  AND eb.form-no = bf-job-mch.frm
                  /*AND eb.blank-no = job-mch.blank-no */
                  NO-LOCK NO-ERROR.

       tmp-color = "".

       IF AVAIL eb THEN do:       
          FOR EACH tt-color:
              DELETE tt-color.
          END.

          DO i = 1 TO eb.i-col:
          /*   tmp-color = tmp-color + IF tmp-color <> "" THEN ("," + eb.i-code[ii] )
                                ELSE eb.i-code[ii].             */
              CREATE tt-color.
              ASSIGN tt-color.i-code = eb.i-code[i].
          END.
          FOR EACH tt-color BREAK BY tt-color.i-code:
              IF first-OF(tt-color.i-code) THEN DO:
                 tmp-color = tmp-color + IF tmp-color <> "" THEN ("," + tt-color.i-code)
                              ELSE tt-color.i-code.             

              END.
          END.

       END.
       FIND FIRST job-sch WHERE job-sch.company = g_company
                           AND job-sch.m-code = bf-job-mch.m-code
                           AND job-sch.m-date = bf-job-mch.start-date
                           AND job-sch.seq = bf-job-mch.seq NO-LOCK NO-ERROR.

       lv-die = IF AVAIL eb THEN eb.die-no ELSE "".
       lv-plate = IF AVAIL eb THEN eb.plate-no ELSE "".
       lv-cust-no = IF AVAIL eb THEN eb.cust-no 
                    ELSE IF AVAIL oe-ord THEN oe-ord.cust-no
                    ELSE "".       
       lv-start-time = bf-job-mch.start-time.      
       lv-seq = IF AVAIL job-sch THEN job-sch.seq ELSE bf-job-mch.seq.
       lv-machine = bf-job-mch.m-code.
       FIND FIRST cust NO-LOCK WHERE cust.company = g_company
                                 AND cust.cust-no = lv-cust-no NO-ERROR.
       lv-cust-no = IF rd_cust = 1 AND AVAIL cust THEN cust.NAME ELSE lv-cust-no.
       /*IF FIRST-OF(bf-job-mch.m-code) AND NOT first(bf-job-mch.m-code) THEN PAGE.*/
        /*  PUT "  Machine: " bf-job-mch.m-code  SKIP. */

       DISP lv-seq  FORMAT ">>>9"
            bf-job-mch.job-no  FORMAT "x(6)"  
            bf-job-mch.job-no2 FORMAT ">9":U 
            lv-cust-no FORM "x(30)"
            bf-job-mch.i-no
            tt-report.due-date 
            lv-board 
            lv-vend-no WHEN tb_print-po FORM "x(8)"
            lv-po-no   WHEN tb_print-po
            lv-qty-received WHEN tb_print-po FORM "->,>>>,>>9.9<<"
            bf-job-mch.start-date-su    FORM "99/99/99" WHEN tb_print-times
            cvt-time-to-string('',bf-job-mch.start-time-su,0.00) WHEN tb_print-times @ lv-mr-stime FORMAT "X(7)" 
            bf-job-mch.end-date-su  FORMAT "99/99/99" WHEN tb_print-times
            /*cvt-hour-to-string(bf-job-mch.mr-hr) @ lv-mr-hr FORMAT "X(7)"*/
            cvt-time-to-string('',bf-job-mch.end-time-su,0.00) WHEN tb_print-times @ lv-mr-etime FORMAT "X(7)"
            bf-job-mch.start-date  FORMAT "99/99/99" WHEN tb_print-times
            cvt-time-to-string('',bf-job-mch.start-time,0.00) WHEN tb_print-times @ lv-run-stime FORMAT "X(7)"
            bf-job-mch.end-date  FORMAT "99/99/99" WHEN tb_print-times
            cvt-time-to-string('END',bf-job-mch.end-time,0.00) WHEN tb_print-times @ lv-run-etime FORMAT "X(7)"
            cvt-hour-to-string(bf-job-mch.run-hr) WHEN tb_print-times @ lv-run-hr 
            SKIP 
            lv-die     FORMAT "X(15)" 
            lv-plate   FORMAT "X(54)" 
            tmp-color  FORMAT "X(55)"
            WITH FRAME det STREAM-IO WIDTH 220 NO-LABELS NO-BOX DOWN .
       PUT "------ ------- ------------------------------ --------------- -------- ---------- -------- ------ ------------ ---------------- ---------------- ---------------- ---------------- ------"
            SKIP(1).

       IF LAST-OF(bf-job-mch.m-code) THEN PAGE.
  END.

SESSION:SET-WAIT-STATE ("").
RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

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
  PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION cvt-hour-to-string C-Win 
FUNCTION cvt-hour-to-string RETURNS CHARACTER
 ( INPUT ip-hour AS dec  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

   RETURN STRING( int(ip-hour * 3600),"HH:MM").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION cvt-time-to-string C-Win 
FUNCTION cvt-time-to-string RETURNS CHARACTER
   (INPUT ip-type AS CHAR, INPUT ip-stime AS INT, INPUT ip-hour AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF ip-type = "END" THEN DO:
     DEF VAR li-end-time AS INT NO-UNDO.
     li-end-time = ip-stime + ip-hour * 3600.
     RETURN STRING(li-end-time,"HH:MMAM").
  END.
  ELSE
  RETURN STRING(ip-stime,"HH:MMAM").   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION jobboard C-Win 
FUNCTION jobboard RETURNS LOGICAL
  ( ipCompany AS CHARACTER, ipJob AS INTEGER,
                                  ipJobNo AS CHARACTER, ipJobNo2 AS INTEGER,
                                  ipForm AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  return yes if board is received, No - still waiting 
    Notes:  
------------------------------------------------------------------------------*/
FOR EACH job-mat NO-LOCK
      WHERE job-mat.company eq ipCompany
        AND job-mat.job EQ ipJob
        AND job-mat.job-no EQ ipJobNo
        AND job-mat.job-no2 EQ ipJobNo2
        AND job-mat.frm EQ ipForm,
      FIRST item NO-LOCK
            WHERE item.company EQ job-mat.company
              AND item.i-no EQ job-mat.i-no
              AND item.mat-type EQ 'B':

    IF CAN-FIND(FIRST mat-act
       WHERE mat-act.company EQ job-mat.company
         AND mat-act.job EQ job-mat.job
         AND mat-act.job-no EQ job-mat.job-no
         AND mat-act.job-no2 EQ job-mat.job-no2
         AND mat-act.i-no EQ job-mat.i-no
         AND mat-act.s-num EQ job-mat.frm
         AND mat-act.b-num EQ job-mat.blank-no USE-INDEX job) THEN
    RETURN YES.
  END. /* each job-mat */
  RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

