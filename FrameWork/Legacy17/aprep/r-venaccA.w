&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description:

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:

  Created:

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

DEF TEMP-TABLE tt-report FIELD actnum LIKE account.actnum
                         FIELD vend-no LIKE vend.vend-no
                         FIELD inv-no LIKE ap-inv.inv-no
                         FIELD jrnl LIKE gltrans.jrnl
                         FIELD tr-date LIKE ap-ledger.tr-date
                         FIELD trnum LIKE ap-ledger.trnum
                         FIELD amt AS DEC
                         FIELD dscr LIKE ap-invl.dscr
                         FIELD file-id AS INT
                         FIELD row-id AS ROWID
                         INDEX detail actnum vend-no inv-no jrnl
                         INDEX row-id row-id.

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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_vend end_vend begin_date ~
end_date begin_acct end_acct rd-dest lv-ornt lines-per-page lv-font-no ~
td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_vend end_vend begin_date end_date ~
begin_acct end_acct rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
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

DEFINE VARIABLE begin_acct AS CHARACTER FORMAT "X(20)":U 
     LABEL "Beginning Acct#" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Vend#" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE end_acct AS CHARACTER FORMAT "X(20)":U INITIAL "zzzzzzzzzzzzzzzzzzzz" 
     LABEL "Ending Acct#" 
     VIEW-AS FILL-IN 
     SIZE 26.8 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Vend#" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-accvend.csv" 
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
"To File", 3
     SIZE 23 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 7.62.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.33.

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
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_vend AT ROW 3.38 COL 19 COLON-ALIGNED HELP
          "Enter Beginning Vendor Number"
     end_vend AT ROW 3.38 COL 63 COLON-ALIGNED HELP
          "Enter Ending Vendor Number"
     begin_date AT ROW 4.81 COL 19 COLON-ALIGNED HELP
          "Enter Beginning Vendor Number"
     end_date AT ROW 4.81 COL 63 COLON-ALIGNED HELP
          "Enter Ending Vendor Number"
     begin_acct AT ROW 6.24 COL 19 COLON-ALIGNED HELP
          "Enter Beginning GL Account Number"
     end_acct AT ROW 6.24 COL 63 COLON-ALIGNED HELP
          "Enter Ending GL Account Number"
     rd-dest AT ROW 10.76 COL 6 NO-LABEL
     lv-ornt AT ROW 11 COL 30 NO-LABEL
     lines-per-page AT ROW 11 COL 83 COLON-ALIGNED
     lv-font-no AT ROW 12.67 COL 33 COLON-ALIGNED
     lv-font-name AT ROW 13.62 COL 27 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 14.57 COL 6
     tb_excel AT ROW 14.81 COL 65 RIGHT-ALIGNED
     tb_runExcel AT ROW 14.81 COL 89 RIGHT-ALIGNED
     fi_file AT ROW 15.91 COL 43 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 18.14 COL 21
     btn-cancel AT ROW 18.14 COL 61
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 10.05 COL 4
     RECT-6 AT ROW 9.57 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 18.86.


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
         TITLE              = "AP Accounts by Vendor"
         HEIGHT             = 19.14
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
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_acct:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_acct:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* AP Accounts by Vendor */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* AP Accounts by Vendor */
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


&Scoped-define SELF-NAME begin_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend C-Win
ON LEAVE OF begin_vend IN FRAME FRAME-A /* Beginning Vend# */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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

  run run-report. 
 STATUS DEFAULT "Processing Complete".
  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case.
  SESSION:SET-WAIT-STATE (""). 

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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


&Scoped-define SELF-NAME end_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend C-Win
ON LEAVE OF end_vend IN FRAME FRAME-A /* Ending Vend# */
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
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
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

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    APPLY "entry" TO begin_vend.
  END.

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-void-date C-Win 
PROCEDURE check-void-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ip-rowid AS ROWID.
DEF OUTPUT PARAMETER op-date AS DATE.
DEF BUFFER bf-ap-inv FOR ap-inv.
DEF BUFFER bf-ap-pay FOR ap-pay.
DEF BUFFER bf-ap-payl FOR ap-payl.
DEF BUFFER bf-ap-ledger FOR ap-ledger.
DEF VAR t-dscr AS CHAR.
DEF VAR v-refnum AS CHAR.
op-date = ?.

FIND bf-ap-payl WHERE rowid(bf-ap-payl) = ip-rowid NO-LOCK NO-ERROR.

FOR EACH bf-ap-pay where bf-ap-pay.company EQ cocode
                   and bf-ap-pay.c-no    eq bf-ap-payl.c-no
                 NO-LOCK:
 FIND FIRST bf-ap-inv
        where bf-ap-inv.company  eq cocode
          and bf-ap-inv.vend-no  EQ bf-ap-payl.vend-no
          and bf-ap-inv.inv-no   EQ bf-ap-payl.inv-no
          and bf-ap-inv.posted   eq yes    
        NO-LOCK NO-ERROR.
 IF NOT AVAIL bf-ap-inv THEN
     RETURN.

   t-dscr = "Payment".

   if bf-ap-payl.memo then t-dscr = "CR MEMO".

   if bf-ap-payl.amt-paid            lt 0  and
      bf-ap-payl.memo                eq no and
      bf-ap-inv.net + bf-ap-inv.freight gt 0  then t-dscr = "Void Chk".
   IF TRUE /* t-dscr = "Void Chk" */ THEN DO:

      v-refnum = "VOIDED CHECK"
                 + string(bf-ap-pay.check-no, "zzzzzzz9").
      FIND FIRST bf-ap-ledger WHERE bf-ap-ledger.company EQ bf-ap-pay.company 
                             AND bf-ap-ledger.vend-no EQ bf-ap-pay.vend-no 
                             AND bf-ap-ledger.refnum = v-refnum
                           NO-LOCK NO-ERROR.
   END.
   IF AVAIL bf-ap-ledger THEN
       op-date = bf-ap-ledger.tr-date.
   ELSE
       op-date = ?.

 END.

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
  DISPLAY begin_vend end_vend begin_date end_date begin_acct end_acct rd-dest 
          lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_vend end_vend begin_date end_date begin_acct 
         end_acct rd-dest lv-ornt lines-per-page lv-font-no td-show-parm 
         tb_excel tb_runExcel fi_file btn-ok btn-cancel 
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
/*      DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.      */
/*                                                         */
/*      if init-dir = "" then init-dir = "c:\temp" .       */
/*      SYSTEM-DIALOG GET-FILE list-name                   */
/*          TITLE      "Enter Listing Name to SAVE AS ..." */
/*          FILTERS    "Listing Files (*.rpt)" "*.rpt",    */
/*                     "All Files (*.*)" "*.*"             */
/*          INITIAL-DIR init-dir                           */
/*          ASK-OVERWRITE                                  */
/*     /*     CREATE-TEST-FILE*/                           */
/*          SAVE-AS                                        */
/*          USE-FILENAME                                   */
/*                                                         */
/*          UPDATE OKpressed.                              */
/*                                                         */
/*      IF NOT OKpressed THEN  RETURN NO-APPLY.            */
{custom/out2file.i}

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
{sys/form/r-topw.f}   

DEF VAR lv-jrnl LIKE gltrans.jrnl NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR lv-bank-code LIKE bank.bank-code NO-UNDO.
DEF VAR lv-check-no AS CHAR NO-UNDO.
DEF VAR li-check-no LIKE ap-pay.check-no NO-UNDO.
DEF VAR li-line LIKE ap-payl.line NO-UNDO.
DEF VAR li-lines AS INT NO-UNDO.
DEF VAR lv-amt LIKE tt-report.amt EXTENT 3 NO-UNDO.
DEF VAR lv-excel-descr AS CHAR NO-UNDO.
DEF VAR v-void-date AS DATE NO-UNDO.
DEF VAR v-temp-date AS DATE NO-UNDO.
DEF VAR v-refnum AS CHAR.
DEF BUFFER bf-ap-ledger FOR ap-ledger.

&SCOPED-DEFINE where-ap-pay                             ~
        WHERE ap-pay.company    EQ cocode               ~
          AND ap-pay.c-no       EQ ap-payl.c-no         ~
          AND ap-pay.vend-no    EQ ap-ledger.vend-no    ~
          AND ap-pay.check-date EQ ap-ledger.ref-date   ~
          AND ap-pay.posted     EQ YES                  ~
          AND ap-pay.memo       EQ YES

FORM tt-report.actnum      COLUMN-LABEL "GL Acct#"
     account.dscr          COLUMN-LABEL "Description" FORMAT "x(32)"
     tt-report.vend-no     COLUMN-LABEL "Vendor"      
     /*vend.name             COLUMN-LABEL "Name"        FORMAT "x(20)"*/
     tt-report.inv-no      COLUMN-LABEL "Inv#"
     tt-report.jrnl        COLUMN-LABEL "Journal"
     tt-report.trnum       COLUMN-LABEL "Run#"
     tt-report.tr-date     COLUMN-LABEL "Date"
     tt-report.amt         COLUMN-LABEL "Amount"    FORMAT "->>>,>>>,>>>,>>9.99"

    WITH FRAME detail NO-BOX NO-ATTR-SPACE DOWN STREAM-IO WIDTH 132.


FIND FIRST ap-ctrl WHERE ap-ctrl.company EQ cocode NO-LOCK NO-ERROR.

ASSIGN
 str-tit2 = c-win:TITLE
 {sys/inc/ctrtext.i str-tit2 112}. 

{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF td-show-parm THEN RUN show-param.

SESSION:SET-WAIT-STATE ("general").

FOR EACH tt-report:
  DELETE tt-report.
END.

IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).
   EXPORT STREAM excel DELIMITER ","       
       "GL Acct#"
       "Description"
       "Vendor"
       "Inv#"
       "Journal"
       "Run#"
       "Date"
       "Amount"
       SKIP.
END.

DISPLAY "" WITH FRAME r-top.

FOR EACH ap-ledger
    WHERE ap-ledger.company EQ cocode
      AND ap-ledger.vend-no GE begin_vend
      AND ap-ledger.vend-no LE end_vend
      AND ap-ledger.vend-no NE ""
      AND ap-ledger.tr-date GE begin_date
      AND ap-ledger.tr-date LE end_date
    NO-LOCK:
    {custom/statusMsg.i " 'Processing Vendor#  '  + ap-ledger.vend-no "}
  IF ap-ledger.refnum BEGINS "INV# " THEN DO:
    FIND FIRST ap-inv
        WHERE ap-inv.company EQ ap-ledger.company
          AND ap-inv.vend-no EQ ap-ledger.vend-no
          AND ap-inv.inv-no  EQ SUBSTR(ap-ledger.refnum,6,20)
        NO-LOCK NO-ERROR.

    lv-jrnl = "ACPAY".

    IF AVAIL ap-inv THEN DO:
      CREATE tt-report.
      BUFFER-COPY ap-ledger TO tt-report
      ASSIGN
       tt-report.inv-no = ap-inv.inv-no
       tt-report.jrnl   = lv-jrnl
       tt-report.actnum = ap-ctrl.payables
       tt-report.amt    = (ap-inv.net + ap-inv.freight) * -1.

      CREATE tt-report.
      BUFFER-COPY ap-ledger TO tt-report
      ASSIGN
       tt-report.inv-no = ap-inv.inv-no
       tt-report.jrnl   = lv-jrnl
       tt-report.actnum = ap-ctrl.freight
       tt-report.amt    = ap-inv.freight.

      FOR EACH ap-invl WHERE ap-invl.i-no EQ ap-inv.i-no:
        CREATE tt-report.
        BUFFER-COPY ap-ledger TO tt-report
        ASSIGN
         tt-report.inv-no  = ap-inv.inv-no
         tt-report.jrnl    = lv-jrnl
         tt-report.actnum  = ap-invl.actnum
         tt-report.amt     = ap-invl.amt
         tt-report.dscr    = ap-invl.dscr
         tt-report.file-id = 1
         tt-report.row-id  = ROWID(ap-invl).
      END.
    END.
  END.

  ELSE
  IF ap-ledger.refnum BEGINS "MEMO#" THEN DO:
    lv-jrnl = "APMEM".

    FOR EACH ap-payl
        WHERE ap-payl.inv-no EQ SUBSTR(ap-ledger.refnum,6,20)
          AND NOT CAN-FIND(FIRST tt-report WHERE tt-report.row-id EQ ROWID(ap-payl))
        NO-LOCK,

        FIRST ap-pay {&where-ap-pay} NO-LOCK

        BREAK BY ap-payl.c-no:

      CREATE tt-report.
      BUFFER-COPY ap-ledger TO tt-report
      ASSIGN
       tt-report.inv-no  = ap-payl.inv-no
       tt-report.jrnl    = lv-jrnl
       tt-report.actnum  = ap-ctrl.payables
       tt-report.amt     = ap-payl.amt-paid - ap-payl.amt-disc
       tt-report.file-id = 2
       tt-report.row-id  = ROWID(ap-payl).

      CREATE tt-report.
      BUFFER-COPY ap-ledger TO tt-report
      ASSIGN
       tt-report.inv-no  = ap-payl.inv-no
       tt-report.jrnl    = lv-jrnl
       tt-report.actnum  = ap-payl.actnum
       tt-report.amt     = (ap-payl.amt-paid - ap-payl.amt-disc) * -1
       tt-report.file-id = 2
       tt-report.row-id  = ROWID(ap-payl).
    END.
  END.

  ELSE
  IF ap-ledger.refnum BEGINS "CHK# " AND
     LENGTH(ap-ledger.refnum) GE 11  THEN DO:
    ASSIGN
     lv-jrnl      = "CDISB"
     lv-bank-code = ""
     lv-check-no  = "".

    DO li = 1 TO LENGTH(ap-ledger.refnum):
      IF LENGTH(TRIM(lv-check-no)) GE 4                                AND
         SUBSTR(lv-check-no,LENGTH(TRIM(lv-check-no)) - 3,4) EQ " CD#" THEN
        lv-bank-code = lv-bank-code + SUBSTR(ap-ledger.refnum,li,1).
      ELSE
        lv-check-no  = lv-check-no + SUBSTR(ap-ledger.refnum,li,1).
    END.
    ASSIGN
     lv-check-no = SUBSTR(lv-check-no,6,LENGTH(TRIM(lv-check-no)) - 9)
     li-check-no = INT(lv-check-no) NO-ERROR.

    IF NOT ERROR-STATUS:ERROR THEN
    FOR EACH bank
        WHERE bank.company   EQ ap-ledger.company
          AND bank.bank-code EQ lv-bank-code
        NO-LOCK,

        FIRST ap-pay
        WHERE ap-pay.company   EQ ap-ledger.company
          AND ap-pay.check-act EQ bank.actnum
          AND ap-pay.check-no  EQ li-check-no
          AND ap-pay.vend-no   EQ ap-ledger.vend-no
          AND CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no)
        NO-LOCK,

        EACH ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no NO-LOCK:

        {custom/statusMsg.i " 'Processing Vendor#  '  + ap-pay.vend-no "}

      CREATE tt-report.
      BUFFER-COPY ap-ledger TO tt-report
      ASSIGN
       tt-report.jrnl    = lv-jrnl
       tt-report.actnum  = bank.actnum
       tt-report.amt     = ap-payl.amt-paid * -1
       tt-report.file-id = 2
       tt-report.row-id  = ROWID(ap-payl).

      CREATE tt-report.
      BUFFER-COPY ap-ledger TO tt-report
      ASSIGN
       tt-report.jrnl    = lv-jrnl
       tt-report.actnum  = ap-payl.actnum
       tt-report.amt     = ap-payl.amt-paid
       tt-report.file-id = 2
       tt-report.row-id  = ROWID(ap-payl).
    END.
  END.

  ELSE
  IF ap-ledger.refnum BEGINS "AC" THEN DO:
    ASSIGN
     lv-jrnl     = "APCKR" 
     li-check-no = INT(SUBSTR(ap-ledger.refnum,3,8)) NO-ERROR.

    IF NOT ERROR-STATUS:ERROR THEN
    FOR EACH bank
        WHERE bank.company EQ ap-ledger.company
        NO-LOCK,

        FIRST ap-pay
        WHERE ap-pay.company   EQ ap-ledger.company
          AND ap-pay.check-act EQ bank.actnum
          AND ap-pay.check-no  EQ li-check-no
          AND ap-pay.vend-no   EQ ap-ledger.vend-no
          AND ap-pay.bank-code EQ bank.bank-code
          AND CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no)
        NO-LOCK,

        EACH ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no NO-LOCK:

        {custom/statusMsg.i " 'Processing Vendor#  '  + ap-pay.vend-no "}

      CREATE tt-report.
      BUFFER-COPY ap-ledger TO tt-report
      ASSIGN
       tt-report.inv-no  = ap-payl.inv-no
       tt-report.jrnl    = lv-jrnl
       tt-report.actnum  = bank.actnum
       tt-report.amt     = ap-payl.amt-paid * -1
       tt-report.file-id = 2
       tt-report.row-id  = ROWID(ap-payl).


      IF ap-payl.amt-disc NE 0 THEN DO:
        CREATE tt-report.
        BUFFER-COPY ap-ledger TO tt-report
        ASSIGN
         tt-report.inv-no  = ap-payl.inv-no
         tt-report.jrnl    = lv-jrnl
         tt-report.actnum  = ap-ctrl.discount
         tt-report.amt     = ap-payl.amt-disc * -1
         tt-report.file-id = 2
         tt-report.row-id  = ROWID(ap-payl).
      END.

      CREATE tt-report.
      BUFFER-COPY ap-ledger TO tt-report
      ASSIGN
       tt-report.inv-no  = ap-payl.inv-no
       tt-report.jrnl    = lv-jrnl
       tt-report.actnum  = ap-ctrl.payables
       tt-report.amt     = ap-payl.amt-paid + ap-payl.amt-disc
       tt-report.file-id = 2
       tt-report.row-id  = ROWID(ap-payl).
    END.
  END.

  ELSE
  IF ap-ledger.refnum BEGINS "VOIDED CHECK" THEN DO:
    ASSIGN
     lv-jrnl     = "APVOIDCK" 
     li-check-no = INT(SUBSTR(ap-ledger.refnum,13,8)) NO-ERROR.

    IF NOT ERROR-STATUS:ERROR THEN
    FOR EACH bank
        WHERE bank.company EQ ap-ledger.company
        NO-LOCK,

        FIRST ap-pay
        WHERE ap-pay.company    EQ ap-ledger.company
          AND ap-pay.check-act  EQ bank.actnum
          AND ap-pay.check-no   EQ li-check-no
          AND ap-pay.vend-no    EQ ap-ledger.vend-no
          AND ap-pay.bank-code  EQ bank.bank-code
          AND ap-pay.cleared    EQ YES
          AND ap-pay.reconciled EQ ?
          AND CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no)
        NO-LOCK:

      ASSIGN
       li-lines = 0
       li-line  = 0
       li       = 0.

      FOR EACH ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no NO-LOCK:
        li-lines = li-lines + 1.
      END.
      li-lines = li-lines / 2.

      FOR EACH ap-payl FIELDS(LINE) WHERE
          ap-payl.c-no EQ ap-pay.c-no
          NO-LOCK
          BY ap-payl.line:
        ASSIGN
         li-line = ap-payl.line
         li      = li + 1.
        IF li GE li-lines THEN LEAVE.
      END.

      FOR EACH ap-payl
          WHERE ap-payl.c-no EQ ap-pay.c-no
            AND ap-payl.line LE li-line
          NO-LOCK:

        CREATE tt-report.
        BUFFER-COPY ap-ledger TO tt-report
        ASSIGN
         tt-report.inv-no  = ap-payl.inv-no
         tt-report.jrnl    = lv-jrnl
         tt-report.actnum  = bank.actnum
         tt-report.amt     = ap-payl.amt-paid
         tt-report.file-id = 2
         tt-report.row-id  = ROWID(ap-payl).

        IF ap-payl.amt-disc NE 0 THEN DO:
          CREATE tt-report.
          BUFFER-COPY ap-ledger TO tt-report
          ASSIGN
           tt-report.inv-no  = ap-payl.inv-no
           tt-report.jrnl    = lv-jrnl
           tt-report.actnum  = ap-ctrl.discount
           tt-report.amt     = ap-payl.amt-disc
           tt-report.file-id = 2
           tt-report.row-id  = ROWID(ap-payl).
        END.

        CREATE tt-report.
        BUFFER-COPY ap-ledger TO tt-report
        ASSIGN
         tt-report.inv-no  = ap-payl.inv-no
         tt-report.jrnl    = lv-jrnl
         tt-report.actnum  = ap-ctrl.payables
         tt-report.amt     = (ap-payl.amt-paid + ap-payl.amt-disc) * -1
         tt-report.file-id = 2
         tt-report.row-id  = ROWID(ap-payl).
      END.
    END.
  END.
END.

FOR EACH tt-report
    WHERE tt-report.actnum GE begin_acct
      AND tt-report.actnum LE end_acct
      AND tt-report.actnum NE ""
      AND tt-report.amt    NE 0
    USE-INDEX detail
    BREAK BY tt-report.actnum
          BY tt-report.vend-no
          BY tt-report.inv-no
          BY tt-report.jrnl:

  FIND FIRST account
      WHERE account.company EQ cocode
        AND account.actnum  EQ tt-report.actnum
      NO-LOCK NO-ERROR.

  FIND FIRST vend
      WHERE vend.company EQ cocode
        AND vend.vend-no EQ tt-report.vend-no
      NO-LOCK NO-ERROR.

  IF AVAIL vend THEN
  {custom/statusMsg.i " 'Processing Vendor#  '  + vend.vend-no "}

  lv-amt[1] = lv-amt[1] + tt-report.amt.

  DISPLAY tt-report.actnum WHEN FIRST-OF(tt-report.actnum)
          account.dscr WHEN AVAIL account AND FIRST-OF(tt-report.actnum)
              "Not on File" WHEN NOT AVAIL account AND FIRST-OF(tt-report.actnum)
                @ account.dscr
      WITH FRAME detail.
  lv-excel-descr = account.dscr.
  IF tt-report.dscr NE "" THEN DO WITH FRAME detail:
    IF AVAIL account AND FIRST-OF(tt-report.actnum) THEN DOWN.
    DISPLAY tt-report.dscr @ account.dscr.
    lv-excel-descr = tt-report.dscr.
  END.

  DISPLAY tt-report.vend-no WHEN FIRST-OF(tt-report.vend-no)
          /*vend.name WHEN AVAIL vend AND FIRST-OF(tt-report.vend-no)
              "Not on File" WHEN NOT AVAIL vend AND FIRST-OF(tt-report.vend-no)
                @ vend.name*/
          tt-report.inv-no
          tt-report.jrnl
          tt-report.trnum
          tt-report.tr-date
          tt-report.amt
      WITH FRAME detail.
  DOWN WITH FRAME detail.

  IF tb_excel THEN  
      EXPORT STREAM excel DELIMITER ","
      /*  gdm - 11130906
            (IF FIRST-OF(tt-report.actnum) 
                THEN tt-report.actnum
                ELSE "")

            (IF AVAIL account THEN 
                IF FIRST-OF(tt-report.actnum) 
                   THEN account.dscr
                   ELSE ""
             ELSE 
                "Not on file")         

            (IF FIRST-OF(tt-report.vend-no)
                THEN tt-report.vend-no
                ELSE "")
      */    
            tt-report.actnum
            /* account.dscr */ lv-excel-descr
            tt-report.vend-no
       /* gdm - 11130906 end */

            tt-report.inv-no
            tt-report.jrnl
            tt-report.trnum
            tt-report.tr-date
            tt-report.amt 
            SKIP.

  IF LAST-OF(tt-report.vend-no) THEN DO WITH FRAME detail:
    PUT SKIP(1).

    UNDERLINE tt-report.amt.
    DISPLAY "  Vendor" @ tt-report.vend-no
            "Totals"   @ tt-report.inv-no
            lv-amt[1]  @ tt-report.amt.
    DOWN.

    IF tb_excel THEN  
       EXPORT STREAM excel DELIMITER ","
              " "
              " "
              " "
              "Vendor Totals"
              " "
              " "
              " "                           
              lv-amt[1]
              SKIP.

    ASSIGN
     lv-amt[2] = lv-amt[2] + lv-amt[1]
     lv-amt[1] = 0.

    PUT SKIP(1).
  END.

  IF LAST-OF(tt-report.actnum) THEN DO WITH FRAME detail:
    PUT SKIP(1).

    UNDERLINE tt-report.amt.
    DISPLAY "   Acct#" @ tt-report.vend-no
            "Totals"   @ tt-report.inv-no
            lv-amt[2]  @ tt-report.amt.
    DOWN.

    IF tb_excel THEN  
       EXPORT STREAM excel DELIMITER ","
              " "
              " "
              " "
              "Acct# Totals"
              " "
              " "
              " "                          
              lv-amt[2]
              SKIP.

    ASSIGN lv-amt[3] = lv-amt[3] + lv-amt[2]
           lv-amt[2] = 0.

    PUT SKIP(3).
  END.

  IF LAST(tt-report.actnum) THEN DO WITH FRAME detail:
    PUT SKIP(1).

    UNDERLINE tt-report.amt.
    DISPLAY "   Grand" @ tt-report.vend-no
            "Totals"   @ tt-report.inv-no
            lv-amt[3]  @ tt-report.amt.
    DOWN.

    IF tb_excel THEN  
       EXPORT STREAM excel DELIMITER ","
              " "
              " "
              " "
              "Grand Totals"
              " "
              " "
              " "                            
              lv-amt[3]
              SKIP.    

  END.
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

IF tb_excel THEN DO:
   OUTPUT STREAM excel CLOSE.
   IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2004 Advanced Software, Inc. */

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

