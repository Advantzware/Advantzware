&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: windows/r-prep.w

  Description: Prep code list

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
DEF VAR list-name AS CHAR NO-UNDO.
DEF VAR init-dir AS CHAR NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}
{custom/xprint.i}
{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = gcompany
 locode = gloc.

DEF VAR v-print-fmt AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO INIT YES.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR security-flag AS LOG NO-UNDO.
DEF VAR lv-pdf-file AS CHAR NO-UNDO.
DEF VAR lv-prt-bypass AS LOG NO-UNDO.  /* bypass window's printer driver */

DEF TEMP-TABLE score NO-UNDO
  FIELD flute AS CHAR
  FIELD idx AS INT
  FIELD panelLabel AS CHAR FORMAT 'X(12)'
  FIELD panel AS DEC FORMAT '9.99' LABEL 'Panel'
  FIELD score AS DEC FORMAT '-9.99' EXTENT 20
  INDEX score IS PRIMARY UNIQUE flute idx.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES style

/* Definitions for FRAME FRAME-A                                        */
&Scoped-define QUERY-STRING-FRAME-A FOR EACH style SHARE-LOCK
&Scoped-define OPEN-QUERY-FRAME-A OPEN QUERY FRAME-A FOR EACH style SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-FRAME-A style
&Scoped-define FIRST-TABLE-IN-QUERY-FRAME-A style


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-8 begin_style end_style ~
printScore printDesign rd-dest lv-ornt lines-per-page lv-font-no ~
td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_style end_style printScore ~
printDesign rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
td-show-parm 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fluteValue C-Win 
FUNCTION fluteValue RETURNS DECIMAL
  (ipStyle AS CHAR,ipFluteCode AS CHAR,ipCode AS CHAR,ipIdx AS INT)  FORWARD.

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

DEFINE VARIABLE begin_style AS CHARACTER FORMAT "x(6)" 
     LABEL "Beginning Style No." 
     VIEW-AS FILL-IN 
     SIZE 11.6 BY 1.

DEFINE VARIABLE end_style AS CHARACTER FORMAT "x(6)" INITIAL "zzzzzz" 
     LABEL "Ending Style No." 
     VIEW-AS FILL-IN 
     SIZE 11.6 BY 1.

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

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 5.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 7.86.

DEFINE VARIABLE printDesign AS LOGICAL INITIAL no 
     LABEL "Print 2D Design Image" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE printScore AS LOGICAL INITIAL no 
     LABEL "Print Scores / Dimensions" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE tb_batch AS LOGICAL INITIAL no 
     LABEL "Run In Batch Mode?" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY FRAME-A FOR 
      style SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_style AT ROW 1.48 COL 49 COLON-ALIGNED HELP
          "Enter Begining Style"
     end_style AT ROW 2.76 COL 49 COLON-ALIGNED HELP
          "Enter Ending Style"
     printScore AT ROW 3.86 COL 51 HELP
          "Select to Print Score Values"
     printDesign AT ROW 5.05 COL 51 HELP
          "Select to Print 2D Design Image"
     rd-dest AT ROW 7.43 COL 6 NO-LABEL
     lv-ornt AT ROW 8.14 COL 32 NO-LABEL
     lines-per-page AT ROW 8.14 COL 85 COLON-ALIGNED
     lv-font-no AT ROW 10.05 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 11 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 12.91 COL 31
     tb_batch AT ROW 12.91 COL 60
     btn-ok AT ROW 14.57 COL 20
     btn-cancel AT ROW 14.57 COL 58
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 6.71 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.48 COL 6
          BGCOLOR 2 
     RECT-7 AT ROW 1.24 COL 2
     RECT-8 AT ROW 6.48 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 95.8 BY 15.24.


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
         TITLE              = "Corrugated Style Box Design List"
         HEIGHT             = 15.24
         WIDTH              = 95.8
         MAX-HEIGHT         = 15.24
         MAX-WIDTH          = 95.8
         VIRTUAL-HEIGHT     = 15.24
         VIRTUAL-WIDTH      = 95.8
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
                                                                        */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_batch IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_batch:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _TblList          = "asi.style"
     _Query            is OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Corrugated Style Box Design List */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Corrugated Style Box Design List */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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

  IF g_batch THEN tb_batch = YES.
  IF tb_batch THEN DO:
     RUN run-batch.
     RETURN NO-APPLY.
  END.

  lv-pdf-file = init-dir + "\style_" + STRING(TIME).

  RUN run-report.

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type= " "
                            &begin_cust="begin_style"
                            &end_cust="begin_style" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name}
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &type=" "
                                &begin_cust="begin_style"
                                &end_cust="begin_style"
                                &mail-subject=c-win:title
                                &mail-body=c-win:title
                                &mail-file=list-name}
           END.
           ELSE DO:
              {custom/asimailr.i &type=" "
                                 &begin_cust="begin_style"
                                 &end_cust="begin_style"
                                 &mail-subject=c-win:title
                                 &mail-body=c-win:title
                                 &mail-file=list-name}
           END.
       END. 
       WHEN 6 THEN RUN OUTPUT-to-port.
  end case.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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
  IF SELF:SCREEN-VALUE BEGINS 'L' THEN 
  ASSIGN lv-font-no = '13'
         lines-per-page = 45
         lv-font-name = 'Courier New Size=9 (13CPI)'.
  ELSE ASSIGN lv-font-no = '11'
              lines-per-page = 60
              lv-font-name = 'Courier New Size=7 (17 cpi for 132 CLMN REPORT)'.
  DISPLAY lv-font-no lines-per-page lv-font-name WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME printDesign
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL printDesign C-Win
ON VALUE-CHANGED OF printDesign IN FRAME FRAME-A /* Print 2D Design Image */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME printScore
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL printScore C-Win
ON VALUE-CHANGED OF printScore IN FRAME FRAME-A /* Print Scores / Dimensions */
DO:
  ASSIGN {&SELF-NAME}.
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

  IF g_batch THEN tb_batch = YES.

  FIND FIRST users WHERE
     users.user_id EQ USERID("NOSWEAT")
     NO-LOCK NO-ERROR.

  IF AVAIL users AND users.user_program[2] NE "" THEN
     init-dir = users.user_program[2].
  ELSE
     init-dir = "c:\tmp".

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    APPLY "entry" TO begin_style.
  END.

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buildScore C-Win 
PROCEDURE buildScore :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipStyle AS CHARACTER NO-UNDO.

  DEFINE VARIABLE idx AS INTEGER NO-UNDO.
  DEFINE VARIABLE scoreLabel AS CHARACTER NO-UNDO INIT
    ',Blank Width:,    Glue In:,   Glue Out:,  Stitch In:, Stitch Out:,       Tape:'.

  EMPTY TEMP-TABLE score.
  FOR EACH flute NO-LOCK WHERE flute.company EQ g_company:
    DO idx = 2 TO NUM-ENTRIES(scoreLabel):
      FIND FIRST reftable NO-LOCK
           WHERE reftable.reftable = "STYFLU" 
             AND reftable.company = ipStyle
             AND reftable.loc = flute.code
             AND reftable.code = STRING(idx) NO-ERROR.
      IF AVAILABLE reftable AND reftable.val[13] NE 0 THEN DO:
        CREATE score.
        ASSIGN
          score.flute = flute.code
          score.idx = idx
          score.panelLabel = ENTRY(idx,scoreLabel)
          score.panel = reftable.val[13].
        DO i = 1 TO 12:
          score.score[i] = reftable.val[i].
        END. /* do i */
        FIND FIRST reftable NO-LOCK
             WHERE reftable.reftable = "STYFLU" 
               AND reftable.company = style.style
               AND reftable.loc = flute.code
               AND reftable.code = STRING(idx)
               AND reftable.code2 = '1' NO-ERROR.
        IF AVAILABLE reftable THEN
        DO i = 1 TO 8:
          score.score[i + 12] = reftable.val[i].
        END. /* do i */
      END. /* avail score */
    END. /* do idx */
  END. /* each flute */

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

  {&OPEN-QUERY-FRAME-A}
  GET FIRST FRAME-A.
  DISPLAY begin_style end_style printScore printDesign rd-dest lv-ornt 
          lines-per-page lv-font-no lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-7 RECT-8 begin_style end_style printScore printDesign rd-dest 
         lv-ornt lines-per-page lv-font-no td-show-parm btn-ok btn-cancel 
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
 IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
  END.
  ELSE IF lv-prt-bypass THEN RUN custom/d-print.w (list-name).
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
  ELSE RUN custom/scr-rpt2.w (list-name,c-win:TITLE,INT(lv-font-no),lv-ornt,lv-prt-bypass).
  /* run scr-rpt.w (list-name,c-win:title,INT(lv-font-no),lv-ornt). /* open file-name, title */ */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-batch C-Win 
PROCEDURE run-batch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {BATCH/runbatch.i "windows\r-booked.r"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/***************************************************************************\
*****************************************************************************
**  Program: /u2/fold/all/dev/asi/oe/rep
**       by: Christopher A. Heins, 07.14.95
** Descript: Salesman Performance daily, period and year to date.
**
*****************************************************************************
\***************************************************************************/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE idx AS INTEGER NO-UNDO.
  DEFINE VARIABLE imageFound AS LOGICAL NO-UNDO.

  {sys/form/r-top.f}

  ASSIGN str-tit2 = c-win:TITLE
         {sys/inc/ctrtext.i str-tit2 80}.

  {sys/inc/print1.i}
  {sys/inc/outprint.i VALUE(lines-per-page)}
  IF is-xprint-form THEN DO:
      CASE rd-dest:
          WHEN 1 THEN PUT '<PRINTER?>'.
          WHEN 2 THEN PUT '<PREVIEW>'.        
          WHEN 4 THEN DO:
                ls-fax-file = 'c:\tmp\fax' + STRING(TIME) + '.tif'.
                PUT UNFORMATTED '<PRINTER?><EXPORT=' Ls-fax-file ',BW>'.
          END.
          WHEN 5 THEN DO:
              IF v-print-fmt = 'Century' THEN
                   PUT '<PREVIEW><PDF-EXCLUDE=MS Mincho><PDF-LEFT=5mm><PDF-TOP=10mm><PDF-OUTPUT=' + lv-pdf-file + '.pdf>' FORM 'x(100)'.
              ELSE PUT '<PREVIEW><PDF-OUTPUT=' + lv-pdf-file + '.pdf>' FORM 'x(60)'.
          END.
      END CASE.
  END.
  IF lv-ornt EQ 'L' THEN PUT UNFORMATTED '<OLANDSCAPE>'.
  PUT UNFORMATTED '<P10></PROGRESS>'.
  IF td-show-parm THEN RUN show-param.
  DISPLAY '' WITH FRAME r-top.
  SESSION:SET-WAIT-STATE ('General').
  FOR EACH style NO-LOCK WHERE style.company EQ g_company
                           AND style.industry EQ '2'
                           AND style.style GE begin_style
                           AND style.style LE end_style
      WITH STREAM-IO DOWN NO-BOX WIDTH 100 PAGE-TOP:
    FIND box-design-hdr NO-LOCK
         WHERE box-design-hdr.design-no EQ style.design-no
           AND box-design-hdr.company EQ style.company NO-ERROR.
    DISPLAY
      style.style
      style.dscr
      style.design-no
      box-design-hdr.box-image WHEN AVAIL(box-design-hdr) FORMAT 'X(55)'
        COLUMN-LABEL 'Image File'.
    /* DOWN.
    DISPLAY box-design-hdr.box-3d-image
        WHEN AVAIL(box-design-hdr) @ box-design-hdr.box-image. */
    imageFound = NO.
    IF printDesign AND AVAILABLE box-design-hdr AND
       SEARCH(box-design-hdr.box-image) NE ? THEN DO:
      imageFound = YES.
      PUT UNFORMATTED '<#1><C1><FROM><C80><R+20><RECT><||3>'  SKIP
                      '<=1><R+2><C2><#1><R+17><C+78><IMAGE#1=' box-design-hdr.box-image '>' SKIP.
    END.
    IF printScore THEN DO:
      RUN buildScore (style.style).
      FOR EACH score NO-LOCK BREAK BY score.flute WITH STREAM-IO WIDTH 100 NO-BOX:
        IF FIRST-OF(score.flute) THEN
        DISPLAY
          score.flute LABEL 'Flute'
          fluteValue(style.style,score.flute,'1',13) FORMAT '9.99' LABEL 'Joint Tab Width'
          style.dim-tk FORMAT '9.99' WHEN style.dim-tk NE 0
          style.dim-pan5 FORMAT '9.99' WHEN style.dim-pan5 NE 0
          style.dim-fit FORMAT '9.99' LABEL 'Lock Tab' WHEN style.dim-fit NE 0
            WITH FRAME flute STREAM-IO WIDTH 100 SIDE-LABELS.
        DISPLAY
          score.panelLabel NO-LABEL
          score.panel
          score.score[1] LABEL '1' WHEN score.score[1] NE 0
          score.score[2] LABEL '2' WHEN score.score[2] NE 0
          score.score[3] LABEL '3' WHEN score.score[3] NE 0
          score.score[4] LABEL '4' WHEN score.score[4] NE 0
          score.score[5] LABEL '5' WHEN score.score[5] NE 0
          score.score[6] LABEL '6' WHEN score.score[6] NE 0
          score.score[7] LABEL '7' WHEN score.score[7] NE 0
          score.score[8] LABEL '8' WHEN score.score[8] NE 0
          score.score[9] LABEL '9' WHEN score.score[9] NE 0
          score.score[10] LABEL '10' WHEN score.score[10] NE 0
          score.score[11] LABEL '11' WHEN score.score[11] NE 0
          score.score[12] LABEL '12' WHEN score.score[12] NE 0
          score.score[13] LABEL '13' WHEN score.score[13] NE 0.
        IF LAST-OF(score.flute) AND PAGE-SIZE - 7 LT LINE-COUNTER +
          (IF printDesign AND imageFound THEN 20 ELSE 0) THEN DO:
            PAGE.
            imageFound = NO.
        END. /* page check */
      END. /* each score */
    END. /* if printscore */
    IF printDesign OR printScore THEN PAGE.
  END. /* each style */
  SESSION:SET-WAIT-STATE ('').
  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus C-Win 
PROCEDURE Set-Focus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fluteValue C-Win 
FUNCTION fluteValue RETURNS DECIMAL
  (ipStyle AS CHAR,ipFluteCode AS CHAR,ipCode AS CHAR,ipIdx AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND FIRST reftable NO-LOCK
       WHERE reftable.reftable EQ 'STYFLU'
         AND reftable.company EQ ipStyle
         AND reftable.loc EQ ipFluteCode
         AND reftable.code EQ ipCode NO-ERROR.
  RETURN IF AVAILABLE reftable THEN reftable.val[ipIdx] ELSE 0.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

