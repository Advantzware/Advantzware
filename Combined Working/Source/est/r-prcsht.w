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
DEF INPUT PARAMETER  ip-rowid AS ROWID NO-UNDO.

/* Local Variable Definitions ---                                       */
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

DEF VAR v-program AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEF VAR v-qty-lev AS INT NO-UNDO.
DEF VAR v-print-what AS cha NO-UNDO.

DEFINE VARIABLE retcode AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE lBussFormModle AS LOGICAL NO-UNDO.

 RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.


{custom/xprint.i}

{methods/prgsecur.i}

  find first sys-ctrl where sys-ctrl.company eq gcompany
                      and sys-ctrl.name    eq "QUOSHEET" no-lock no-error.
    if not avail sys-ctrl then do transaction:
        create sys-ctrl.
        ASSIGN sys-ctrl.company  = gcompany
            sys-ctrl.name     = "QUOSHEET"
            sys-ctrl.descrip  = "Print the Price List via Quotes?"
            sys-ctrl.char-fld = "Xprint"
            sys-ctrl.log-fld = YES.
     end.
     v-print-what = sys-ctrl.char-fld.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tb_print-2nd-dscr ~
tb_print-est-name tb_print-3rd-dscr tb_print-est-dscr tb_change_qty lv-ornt ~
lines-per-page rd-dest lv-font-no td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_print-2nd-dscr tb_print-est-name ~
tb_print-3rd-dscr tb_print-est-dscr tb_change_qty lv-ornt lines-per-page ~
rd-dest lv-font-no lv-font-name td-show-parm 

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

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=12 (10 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "15" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE v-cust AS CHARACTER FORMAT "X(256)":U 
     LABEL "Cust" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE v-group-title AS CHARACTER FORMAT "X(8)" INITIAL "EQSheet" 
     LABEL "Send to Title" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

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
     SIZE 106 BY 10.48.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 106 BY 3.81.

DEFINE VARIABLE tb_change_qty AS LOGICAL INITIAL no 
     LABEL "Change Quantity for all Item?" 
     VIEW-AS TOGGLE-BOX
     SIZE 35.4 BY .81 NO-UNDO.

DEFINE VARIABLE tb_print-2nd-dscr AS LOGICAL INITIAL no 
     LABEL "Print 2nd Item Description Line?" 
     VIEW-AS TOGGLE-BOX
     SIZE 35.4 BY .81 NO-UNDO.

DEFINE VARIABLE tb_print-3rd-dscr AS LOGICAL INITIAL no 
     LABEL "Print 3rd Item Description Line?" 
     VIEW-AS TOGGLE-BOX
     SIZE 35.4 BY .81 NO-UNDO.

DEFINE VARIABLE tb_print-est-dscr AS LOGICAL INITIAL no 
     LABEL "Print Est Description ?" 
     VIEW-AS TOGGLE-BOX
     SIZE 35.4 BY .81 NO-UNDO.

DEFINE VARIABLE tb_print-est-name AS LOGICAL INITIAL no 
     LABEL "Print Est Item Name?" 
     VIEW-AS TOGGLE-BOX
     SIZE 35.4 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tb_print-2nd-dscr AT ROW 1.52 COL 60.6 WIDGET-ID 6
     tb_print-est-name AT ROW 1.57 COL 23.6 WIDGET-ID 4
     tb_print-3rd-dscr AT ROW 2.57 COL 61 WIDGET-ID 10
     tb_print-est-dscr AT ROW 2.67 COL 23.6 WIDGET-ID 8
     tb_change_qty AT ROW 3.62 COL 23.6 WIDGET-ID 12
     lv-ornt AT ROW 7 COL 31 NO-LABEL
     lines-per-page AT ROW 7 COL 84 COLON-ALIGNED
     rd-dest AT ROW 7.24 COL 6 NO-LABEL
     lv-font-no AT ROW 8.43 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 9.38 COL 30 COLON-ALIGNED NO-LABEL
     v-group-title AT ROW 11.76 COL 38 COLON-ALIGNED HELP
          "Enter Email Title"
     td-show-parm AT ROW 14.14 COL 4
     btn-ok AT ROW 15.67 COL 29
     btn-cancel AT ROW 15.67 COL 65
     v-cust AT ROW 15.67 COL 90 COLON-ALIGNED
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 5.1 COL 4
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 6.38 COL 5
     RECT-6 AT ROW 4.86 COL 2
     RECT-7 AT ROW 1 COL 2 WIDGET-ID 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SIZE 108.2 BY 16.57.


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
         TITLE              = "Print Price Sheet"
         HEIGHT             = 16.67
         WIDTH              = 108.2
         MAX-HEIGHT         = 16.67
         MAX-WIDTH          = 108.2
         VIRTUAL-HEIGHT     = 16.67
         VIRTUAL-WIDTH      = 108.2
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


/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_change_qty:PRIVATE-DATA IN FRAME FRAME-A     = 
                "Parm".

ASSIGN 
       tb_print-2nd-dscr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "Parm".

ASSIGN 
       tb_print-3rd-dscr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "Parm".

ASSIGN 
       tb_print-est-dscr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "Parm".

ASSIGN 
       tb_print-est-name:PRIVATE-DATA IN FRAME FRAME-A     = 
                "Parm".

/* SETTINGS FOR FILL-IN v-cust IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       v-cust:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN v-group-title IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       v-group-title:HIDDEN IN FRAME FRAME-A           = TRUE
       v-group-title:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Print Price Sheet */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Print Price Sheet */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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
  assign {&DISPLAYED-OBJECTS}
         lv-pdf-file = init-dir + "\PRS" + STRING(quotehd.q-no).

  run run-report. 
  v-cust = quotehd.cust-no.

  case rd-dest:
      when 1 then run output-to-printer.
      when 2 then run output-to-screen.
      when 3 then run output-to-file.
      when 4 then do:
          /*run output-to-fax.*/
          {custom/asifax.i &begin_cust=quotehd.cust-no 
                           &END_cust=v-cust
                           &fax-subject="Price Sheet"
                           &fax-body="Price Sheet"
                           &fax-file=list-name }
      END.
      when 5 then do:
         IF is-xprint-form THEN DO:
             RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
             {custom/asimail2.i &TYPE="CUSTOMER"
                             &group-title=v-prgmname
                             &begin_cust=quotehd.cust-no
                             &END_cust=v-cust
                             &mail-subject="Price Sheet"
                             &mail-body="Price Sheet"
                             &mail-file=lv-pdf-file + ".pdf" }              
          END.
          ELSE DO:
              {custom/asimailr2.i &TYPE="CUSTOMER"
                                 &group-title=v-prgmname
                                 &begin_cust=quotehd.cust-no
                                 &END_cust=v-cust
                                 &mail-subject= "Price Sheet"
                                 &mail-body="Price Sheet"
                                 &mail-file=list-name }
          END.
      END. 
      WHEN 6 THEN run output-to-port.
 end case. 
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
  IF {&self-name} = 5 THEN do:
     v-group-title:SENSITIVE = YES.
  /*   APPLY "entry" TO v-group-title.*/
  END.
  ELSE v-group-title:SENSITIVE = NO.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_change_qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_change_qty C-Win
ON VALUE-CHANGED OF tb_change_qty IN FRAME FRAME-A /* Change Quantity for all Item? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_print-2nd-dscr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_print-2nd-dscr C-Win
ON VALUE-CHANGED OF tb_print-2nd-dscr IN FRAME FRAME-A /* Print 2nd Item Description Line? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_print-3rd-dscr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_print-3rd-dscr C-Win
ON VALUE-CHANGED OF tb_print-3rd-dscr IN FRAME FRAME-A /* Print 3rd Item Description Line? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_print-est-dscr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_print-est-dscr C-Win
ON VALUE-CHANGED OF tb_print-est-dscr IN FRAME FRAME-A /* Print Est Description ? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_print-est-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_print-est-name C-Win
ON VALUE-CHANGED OF tb_print-est-name IN FRAME FRAME-A /* Print Est Item Name? */
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


&Scoped-define SELF-NAME v-group-title
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-group-title C-Win
ON HELP OF v-group-title IN FRAME FRAME-A /* Send to Title */
DO:
    DEF VAR v-title AS cha NO-UNDO.

    RUN windows/l-ttlcod.w (FOCUS:SCREEN-VALUE, OUTPUT v-title).
    IF v-title <> "" THEN SELF:SCREEN-VALUE = entry(1,v-title).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-group-title C-Win
ON LEAVE OF v-group-title IN FRAME FRAME-A /* Send to Title */
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


  FIND FIRST  quotehd WHERE ROWID(quotehd) EQ ip-rowid  NO-LOCK NO-ERROR.
  IF AVAIL quotehd THEN
  FIND FIRST est
      WHERE est.company EQ quotehd.company
        AND est.est-no  EQ quotehd.est-no
      NO-LOCK NO-ERROR.


  is-xprint-form = YES.

  RUN enable_UI.
  {custom/usrprint.i}
  ASSIGN rd-dest.
  IF rd-dest = 5 THEN v-group-title:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
  ELSE v-group-title:SENSITIVE = NO.

  FIND FIRST users WHERE
       users.user_id EQ USERID("NOSWEAT")
       NO-LOCK NO-ERROR.

  IF AVAIL users AND users.user_program[2] NE "" THEN
     init-dir = users.user_program[2].
  ELSE
     init-dir = "c:\tmp".


  DO WITH FRAME {&frame-name}:

   IF LOOKUP(v-print-what,"StClair") > 0 THEN   
     ASSIGN 
     tb_print-2nd-dscr:HIDDEN IN FRAME FRAME-A = NO 
     tb_print-est-name:HIDDEN IN FRAME FRAME-A = NO
     tb_print-3rd-dscr:HIDDEN IN FRAME FRAME-A = NO
     tb_print-est-dscr:HIDDEN IN FRAME FRAME-A = NO
     tb_change_qty:HIDDEN IN FRAME FRAME-A = NO.

   ELSE DO:
       ASSIGN
           tb_change_qty = NO .
       ASSIGN 
           tb_print-2nd-dscr:HIDDEN IN FRAME FRAME-A = YES
           tb_print-est-name:HIDDEN IN FRAME FRAME-A = YES
           tb_print-3rd-dscr:HIDDEN IN FRAME FRAME-A = YES
           tb_print-est-dscr:HIDDEN IN FRAME FRAME-A = YES
           tb_change_qty:HIDDEN IN FRAME FRAME-A = YES . 
   END.

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
  DISPLAY tb_print-2nd-dscr tb_print-est-name tb_print-3rd-dscr 
          tb_print-est-dscr tb_change_qty lv-ornt lines-per-page rd-dest 
          lv-font-no lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tb_print-2nd-dscr tb_print-est-name tb_print-3rd-dscr 
         tb_print-est-dscr tb_change_qty lv-ornt lines-per-page rd-dest 
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

 IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
 END.
 ELSE RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).

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
  IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
  END.
  ELSE run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ 

  /*ELSE run scr-rpt.w (list-name,c-win:title). /* open file-name, title */  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* -------------------------------------------- cec/quote/printquo.p 8/94 rd  */
/* print quotes                                                               */
/* -------------------------------------------------------------------------- */

DEF VAR lv-quo-no LIKE quotehd.q-no NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR lv-cnt AS INT NO-UNDO.
DEF VAR v-part-no AS cha NO-UNDO.
DEF VAR v-price LIKE quoteitm.price NO-UNDO.
DEF VAR v-qty LIKE quoteitm.qty NO-UNDO.
DEF VAR v-uom LIKE quoteitm.uom NO-UNDO.
DEF VAR li-cline AS INT NO-UNDO.


/* gdm 05290908 */
DEF VAR v-descr AS CHAR FORMAT "x(30)" NO-UNDO.
/* gdm 05290908 end */
DEF VAR v-descr2 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-descr3 AS CHAR FORMAT "x(30)" NO-UNDO. 
DEF VAR v-est-name AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-est-dscr AS CHAR FORMAT "x(30)" NO-UNDO.

{sys/form/r-top.i}
{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

IF IS-xprint-form THEN DO:
    CASE rd-dest:
        WHEN 1 THEN PUT  "<PRINTER?>".
        WHEN 2 THEN do:
            IF NOT lBussFormModle THEN
               PUT "<PREVIEW><MODAL=NO>". 
            ELSE
               PUT "<PREVIEW>".      
        END.         
        WHEN  4 THEN do:
              ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".tif".
              PUT UNFORMATTED "<PRINTER?><EXPORT=" Ls-fax-file ",BW>".
        END.
        WHEN 5 THEN PUT "<PREVIEW><PDF-LEFT=5mm><PDF-TOP=10mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf><PDF-LEFT=5mm>" FORM "x(180)".
    END CASE.
END.

/*================== report main section ===========*/

DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
DEF VAR lv-pg-num AS INT INIT 1 FORM ">>9" NO-UNDO.
DEF VAR lv-tot-pg AS INT INIT 1 FORM ">>9" NO-UNDO.
DEF VAR lv-tel AS cha FORM "(xxx)xxx-xxxx" NO-UNDO.
DEF VAR lv-fax AS cha FORM "(xxx)xxx-xxxx" NO-UNDO.
DEF VAR vphone AS CHA FORM "xxxxxxxxxxxx" NO-UNDO.  
DEF VAR vshiphone AS CHA FORM "(xxx)-xxx-xxxx" NO-UNDO.
DEF VAR lv-email AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-soldto AS cha EXTENT 4 FORM "x(30)" NO-UNDO.
DEF VAR v-shipto AS cha EXTENT 4 FORM "x(30)" NO-UNDO.
DEF VAR v-sman AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-cust-id AS cha FORM "x(10)" NO-UNDO.
DEF VAR lv-contact AS cha FORM "x(18)" NO-UNDO.
DEF VAR v-i-no LIKE itemfg.i-no NO-UNDO.
DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-display-comp AS LOG NO-UNDO.
DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add5 AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-comp-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-comp-email AS cha FORM "x(30)" NO-UNDO.


IF v-print-what = "APC" THEN ls-image1 = "images\apc.jpg".
ELSE IF v-print-what = "StClair" THEN ls-image1 = "images\StClairPkg Logo.jpg" .
ELSE IF v-print-what = "Southpak" THEN ls-image1 = "images\southpak4.jpg".

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

FIND first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "LOGOCOLR" no-lock no-error.
IF AVAIL sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".
ASSIGN 
    v-qty-lev = 0
    v-est-name = ""
    v-est-dscr = "" 
    v-descr2   = ""
    v-descr3   = "" .

IF v-print-what = "StClair" THEN do:
     IF tb_change_qty THEN
        message "Change Quantity for all Items on Quote." update v-qty-lev .
END.

IF v-print-what = "Xprint"  THEN DO:
   FIND FIRST cust WHERE cust.company = cocode AND
                         cust.active = "X" NO-LOCK NO-ERROR.
   IF AVAIL cust THEN
     ASSIGN v-comp-add1 = cust.addr[1]
            v-comp-add2 = cust.addr[2]
            v-comp-add3 = cust.city + ", " + cust.state + "  " + cust.zip
            v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
            v-comp-add5 = "Fax     :  " + string(cust.fax,"(999)999-9999") 
            lv-comp-email    = "Email:  " + cust.email 
            lv-comp-name = cust.NAME   
            .
   IF v-comp-add2 = "" THEN
         ASSIGN v-comp-add2 = v-comp-add3
                v-comp-add3 = v-comp-add4
                v-comp-add4 = v-comp-add5
                v-comp-add5 = lv-comp-email
                lv-comp-email = ""
                .   
END.

FIND FIRST cust WHERE cust.company = quotehd.company
                  AND cust.cust-no = quotehd.cust-no NO-LOCK NO-ERROR.

ASSIGN lv-cust-id = cust.cust-no
       lv-contact = quotehd.contact
       lv-email = cust.email
       lv-tel = cust.area-code + cust.phone
       lv-fax = cust.fax
       .

FIND FIRST shipto WHERE shipto.ship-id = quotehd.ship-id
                        AND shipto.company = quotehd.company NO-LOCK NO-ERROR.
IF AVAIL shipto THEN
    ASSIGN vshiphone = shipto.area-code + shipto.phone.

 find first sman where sman.sman = quotehd.sman no-lock no-error.
 v-sman = if avail sman then sman.sname ELSE quotehd.sman.
 IF AVAIL sman THEN
    /* FIND FIRST phone WHERE phone.table_rec_key = sman.rec_key NO-LOCK NO-ERROR.
 IF AVAIL phone THEN
     ASSIGN vphone = phone.phone .
 ELSE vphone = "" .*/

 FIND FIRST notes WHERE notes.rec_key = sman.rec_key NO-LOCK NO-ERROR.
 IF AVAIL notes THEN
     ASSIGN vphone = notes.note_text.
 ELSE vphone = " ".

ASSIGN v-soldto[1] = quotehd.cust-no
       v-soldto[2] = quotehd.billto[1]
       v-soldto[3] = quotehd.billto[2]
       v-soldto[4] = quotehd.billto[3]
       v-shipto[1] = quotehd.ship-id
       v-shipto[2] = quotehd.shipto[1]
       v-shipto[3] = quotehd.shipto[2]
       v-shipto[4] = quotehd.shipto[3]
       .

/* ===================*/
lv-cnt = 0.
FOR EACH quoteitm OF quotehd NO-LOCK :
    lv-cnt = lv-cnt + 1.
END.
lv-tot-pg = truncate(lv-cnt / 33,0).
IF lv-cnt MOD 33 <> 0 THEN lv-tot-pg = lv-tot-pg + 1.

IF v-print-what = "Southpak"  THEN DO:
    {cec/quote/prcsheet.i}
END.
ELSE IF v-print-what = "APC" THEN DO:
     {cec/quote/prcxapc.i}
END.
ELSE IF v-print-what = "StClair" THEN DO:
     {cec/quote/prcstclr.i}
END.
ELSE IF v-print-what = "XPRINT" THEN DO:
     {cec/quote/prcxprnt.i}
END.
ELSE DO:
   {cec/quote/prcxpsht.i}
END.
PUT "<=2><R+17><P9>" SKIP.
lv-cnt = 0.
FOR EACH quoteitm OF quotehd NO-LOCK /*,
    EACH quoteqty OF quoteitm NO-LOCK*/:

/* gdm 05290908 */
    ASSIGN v-descr = quoteitm.part-dscr1. 

    FIND FIRST itemfg 
      WHERE itemfg.company EQ quotehd.company
        AND itemfg.i-no    EQ quoteitm.i-no NO-LOCK NO-ERROR.
    IF NOT AVAIL itemfg THEN 
      FIND FIRST itemfg 
        WHERE itemfg.company EQ quotehd.company
          AND itemfg.i-no    EQ quoteitm.i-no 
          AND itemfg.part-no EQ quoteitm.part-no NO-LOCK NO-ERROR. 
      IF NOT AVAIL itemfg THEN 
        FIND FIRST itemfg 
          WHERE itemfg.company EQ quotehd.company
            AND itemfg.part-no EQ quoteitm.part-no NO-LOCK NO-ERROR.

    IF v-print-what EQ "Southpak" 
      THEN 
       ASSIGN v-descr = IF AVAIL itemfg 
                          THEN itemfg.part-dscr1
                          ELSE "".
/* gdm 05290908 end */

/*                        
    v-i-no = IF AVAIL itemfg THEN itemfg.i-no ELSE quoteitm.i-no /*quoteitm.part-no*/.
*/

    IF AVAIL itemfg AND tb_print-2nd-dscr THEN
        v-descr2 = itemfg.part-dscr2 .
    IF AVAIL itemfg AND tb_print-3rd-dscr THEN
        v-descr3 = itemfg.part-dscr3 .

    FIND FIRST est WHERE est.company = quotehd.company
                   AND est.est-no = quotehd.est-no NO-LOCK NO-ERROR.
  IF AVAIL est THEN
  FIND FIRST eb
      WHERE eb.company EQ est.company
        AND eb.est-no  EQ est.est-no
        AND eb.part-no EQ quoteitm.part-no
        AND eb.form-no NE 0
      NO-LOCK NO-ERROR.
  IF NOT AVAIL eb AND quoteitm.est-no <> "" THEN
  IF AVAIL est THEN FIND FIRST eb
      WHERE eb.company EQ est.company
        AND eb.est-no  EQ est.est-no
        and eb.form-no NE 0
      NO-LOCK NO-ERROR.

  IF AVAIL eb AND tb_print-est-name THEN
      ASSIGN v-est-name = eb.part-dscr1 .
  IF AVAIL eb AND tb_print-est-dscr THEN
      ASSIGN  v-est-dscr = eb.part-dscr2 .

    v-i-no = quoteitm.i-no.
    FIND FIRST cust-part WHERE cust-part.company = quotehd.company
                           AND cust-part.i-no = v-i-no NO-LOCK NO-ERROR.
    v-part-no = IF AVAIL cust-part THEN cust-part.part-no ELSE v-i-no.

    ASSIGN v-price = quoteitm.price
           v-qty = quoteitm.qty
           v-uom = quoteitm.uom.

    FIND FIRST quoteqty WHERE ASI.quoteqty.company = ASI.quoteitm.company
          AND ASI.quoteqty.loc = ASI.quoteitm.loc
          AND ASI.quoteqty.q-no = ASI.quoteitm.q-no
          AND ASI.quoteqty.line = ASI.quoteitm.line  USE-INDEX qt-qty NO-LOCK NO-ERROR.
    IF AVAIL quoteqty THEN 
       ASSIGN v-price = quoteqty.price
              v-qty = quoteqty.qty
              v-uom = quoteqty.uom.
    /*
    IF v-print-what = "Southpak" THEN
       PUT v-part-no AT 3 FORM "x(15)"
           v-i-no /*quoteitm.part-no*/ AT 20 FORM "x(15)" /*v-i-no AT 20*/ 
           quoteitm.part-dscr1 AT 38
           v-qty AT 72
           v-uom AT 95 
           v-price AT 98 FORM ">>>9.999"
           SKIP.
    ELSE PUT v-part-no AT 3 FORM "x(15)"
             quoteitm.part-no AT 20 FORM "x(15)" /*v-i-no AT 20*/ 
             quoteitm.part-dscr1 AT 38
             v-qty AT 72
             v-uom AT 95 
             v-price AT 98 FORM ">>>>9.99"
             SKIP.
    */



    IF v-print-what = "StClair" THEN do:
        IF v-uom = "CS" THEN do:
            IF AVAIL eb THEN
                ASSIGN v-qty = v-qty / eb.tr-cnt .
        END.

       IF tb_change_qty THEN
        ASSIGN v-qty = v-qty-lev .

     PUT quoteitm.part-no AT 3 FORM "x(15)"
        quoteitm.i-no AT 20 FORM "x(15)"

        /* gdm 05290908 */
         v-descr FORMAT "x(30)" AT 38
        /* gdm 05290908 end */

        v-qty AT 72
        v-uom AT 88 
        v-price AT 95 FORM ">>>>9.9999"
        SKIP.

    IF v-est-name <> "" THEN do:
        PUT v-est-name FORMAT "x(30)" AT 38 SKIP.
        lv-cnt = lv-cnt + 1.
    END.
    IF v-est-dscr <> "" THEN do:
        PUT v-est-dscr FORMAT "x(30)" AT 38 SKIP.
        lv-cnt = lv-cnt + 1.
    END.
    IF v-descr2 <> "" THEN do:
        PUT v-descr2 FORMAT "x(30)" AT 38 SKIP.
        lv-cnt = lv-cnt + 1.
    END.
    IF v-descr3 <> "" THEN do:
        PUT v-descr3 FORMAT "x(30)" AT 38 SKIP.
        lv-cnt = lv-cnt + 1.
    END.
    END.
    ELSE DO:
        PUT quoteitm.part-no AT 3 FORM "x(15)"
        quoteitm.i-no AT 20 FORM "x(15)"

        /* gdm 05290908 */
         v-descr FORMAT "x(30)" AT 38
        /* gdm 05290908 end */

        v-qty AT 72
        v-uom AT 95 
        v-price AT 98 FORM ">>>>9.99"
        SKIP.

    END.

    lv-cnt = lv-cnt + 1.
    IF lv-cnt >= 33 THEN DO:
       PAGE.
       lv-pg-num = PAGE-NUM.
       lv-cnt = 0.
       IF v-print-what = "Southpak" THEN DO:
         {cec/quote/prcsheet.i}
       END.
       IF v-print-what = "APC" THEN DO:
         {cec/quote/prcxapc.i}
       END.
       IF v-print-what = "StClair" THEN DO:
           {cec/quote/prcstclr.i}
       END.
       ELSE DO:
           {cec/quote/prcxpsht.i}
       END.
       PUT "<=2><R+17><P9>" SKIP.
    END.


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
  PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

