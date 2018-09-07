&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: aprep\r-asmast.p

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

DEF TEMP-TABLE tt-report NO-UNDO LIKE report.

DEF VAR v-invalid AS LOG NO-UNDO.
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 fiFromAsset fiToAsset fiFromGL ~
fiToGL fiFromLoc fiToLoc fiSort1 fiSort2 fiSortOrder fiStatus fiByList ~
rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel ~
fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fiFromAsset fiToAsset fiFromGL fiToGL ~
fiFromLoc fiToLoc fiSort1 fiSort2 fiSortOrder fiStatus fiByList rd-dest ~
lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm tb_excel ~
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

DEFINE VARIABLE fiByList AS CHARACTER FORMAT "x":U INITIAL "B" 
     LABEL "By" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE fiFromAsset AS CHARACTER FORMAT "X(256)":U 
     LABEL "Asset Code" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiFromGL AS CHARACTER FORMAT "X(256)":U 
     LABEL "GL Code" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiFromLoc AS CHARACTER FORMAT "X(256)":U 
     LABEL "Location" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiSort1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sort Code 1" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE fiSort2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sort Code 2" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE fiSortOrder AS CHARACTER FORMAT "x":U INITIAL "A" 
     LABEL "Sort Order" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE fiStatus AS CHARACTER FORMAT "X(4)":U INITIAL "AIRZ" 
     LABEL "Status Codes" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE fiToAsset AS CHARACTER FORMAT "X(256)":U INITIAL "zzzzzzzz" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiToGL AS CHARACTER FORMAT "X(256)":U INITIAL "zzzzzzzz" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiToLoc AS CHARACTER FORMAT "X(256)":U INITIAL "zzzzzzzz" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-asmast.csv" 
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
     SIZE 92 BY 9.05.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 11.67.

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
     fiFromAsset AT ROW 2.91 COL 29 COLON-ALIGNED WIDGET-ID 6
     fiToAsset AT ROW 2.91 COL 59 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     fiFromGL AT ROW 4.1 COL 29 COLON-ALIGNED WIDGET-ID 14
     fiToGL AT ROW 4.1 COL 59 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     fiFromLoc AT ROW 5.29 COL 29 COLON-ALIGNED WIDGET-ID 18
     fiToLoc AT ROW 5.29 COL 59 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     fiSort1 AT ROW 6.48 COL 29 COLON-ALIGNED WIDGET-ID 22
     fiSort2 AT ROW 7.67 COL 29 COLON-ALIGNED WIDGET-ID 24
     fiSortOrder AT ROW 8.86 COL 29 COLON-ALIGNED WIDGET-ID 26
     fiStatus AT ROW 10.05 COL 29 COLON-ALIGNED WIDGET-ID 30
     fiByList AT ROW 11.24 COL 29 COLON-ALIGNED WIDGET-ID 34
     rd-dest AT ROW 14.1 COL 6 NO-LABEL
     lv-ornt AT ROW 15.05 COL 31 NO-LABEL
     lines-per-page AT ROW 15.05 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 16.48 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 17.43 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 18.52 COL 30.4
     tb_excel AT ROW 19.81 COL 50.4 RIGHT-ALIGNED
     tb_runExcel AT ROW 19.81 COL 71.4 RIGHT-ALIGNED
     fi_file AT ROW 20.62 COL 28.4 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 22.19 COL 19
     btn-cancel AT ROW 22.19 COL 58
     "(Par,Child,Both,Rel,r-Only)" VIEW-AS TEXT
          SIZE 56 BY .62 AT ROW 11.48 COL 36 WIDGET-ID 36
     "From" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.19 COL 39 WIDGET-ID 10
     "(A,I,R,Z)" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 10.29 COL 42 WIDGET-ID 32
     "(A,L,G,1,2)" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 9.1 COL 36 WIDGET-ID 28
     "To" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.19 COL 70 WIDGET-ID 12
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 13.38 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 12.91 COL 2
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 22.91.


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
         TITLE              = "FA Asset Master Listing"
         HEIGHT             = 23.43
         WIDTH              = 95.8
         MAX-HEIGHT         = 45.05
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 45.05
         VIRTUAL-WIDTH      = 256
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
ON END-ERROR OF C-Win /* FA Asset Master Listing */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* FA Asset Master Listing */
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
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  run run-report. 
  STATUS DEFAULT "Processing Complete".
  SESSION:SET-WAIT-STATE ("").

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type= ''
                            &begin_cust= "fiFromAsset"
                            &END_cust= "fiFromAsset" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = ''
                             &begin_cust=''
                             &END_cust='' 
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = ''
                                  &begin_cust=''
                                  &END_cust='' 
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

  find first period
      where period.company eq cocode
        and period.yr      eq year(today)
        and period.pst     le today
        and period.pend    ge today
        and period.pstat
      no-lock no-error.

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO fiFromAsset.
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
  DISPLAY fiFromAsset fiToAsset fiFromGL fiToGL fiFromLoc fiToLoc fiSort1 
          fiSort2 fiSortOrder fiStatus fiByList rd-dest lv-ornt lines-per-page 
          lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 fiFromAsset fiToAsset fiFromGL fiToGL fiFromLoc fiToLoc 
         fiSort1 fiSort2 fiSortOrder fiStatus fiByList rd-dest lv-ornt 
         lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file 
         btn-ok btn-cancel 
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
/* ---------------------------------------------- ap/rep/taxsched.p 07/99 JLF */
/* Tax Distribution Schedule by Customer                                      */
/* -------------------------------------------------------------------------- */
DEF VAR beg-code like fa-mast.asset-code label "From Asset Code" extent 0 no-undo.
DEF VAR end-code like fa-mast.asset-code label "To Asset code" initial "zzzzzzzz" extent 0 no-undo.
DEF VAR beg-gl like fa-mast.gl-code label "From GL Group" extent 0 no-undo.
DEF VAR end-gl like fa-mast.gl-code label "To GL Group" initial "zzzzzzzz" extent 0 no-undo.
DEF VAR beg-loc like fa-mast.location label "From Location" extent 0 no-undo.
DEF VAR end-loc like fa-mast.location label "To Location" initial "zzzz" extent 0 no-undo.
DEF VAR sort1 as character format "x(45)" label "Sort Code 1" extent 0 no-undo.
DEF VAR sort2 as character format "x(45)" label "Sort Code 2" extent 0 no-undo.
DEF VAR torder as character format "x(1)" label "Sort Order (ALG12)" initial "A" extent 0 no-undo.
DEF VAR t-status as character format "x(4)" label "Status (AIRZ)" initial "AIRZ" extent 0 no-undo.
DEF VAR by-list as character format "x(1)" label "Par,Child,Both,Relation,r-Only" initial "B" extent 0 no-undo.
DEF VAR liste# as character format "x(5)".
DEF VAR acc-book like fa-mast.acc-dep-book.
DEF VAR acc-tax1 like acc-book.
DEF VAR acc-tax2 like acc-book.
DEF BUFFER fa-mast# for fa-mast.

{sys/form/r-top3w.f}

def var v-period        like uperiod init 1 NO-UNDO.
def var v-date          as   date extent 2 format "99/99/9999"
                             init [01/01/0001, today] NO-UNDO.                       
def var v-year          as   INT NO-UNDO.

def var v-tax-gl        as   CHAR NO-UNDO.
def var v-tax-dscr      like stax.tax-dscr NO-UNDO.
def var v-sal-gro       as   dec format "->>,>>>,>>9.99" extent 3 NO-UNDO.
def var v-tax-amt       as   dec format "->>,>>>,>>9.99" extent 3 NO-UNDO.
def var v-freight       as   dec format "->>,>>>,>>9.99" extent 3 NO-UNDO.
def var v-actnum        like ar-cashl.actnum NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.

/* gdm - */
def var v-grtot         as   dec format "->>,>>>,>>9.99" NO-UNDO.
def var v-grfrght       as   dec format "->>,>>>,>>9.99" NO-UNDO.

/* aj */
def var v-rate          as   DEC NO-UNDO.
def var v-frtr          as   DEC NO-UNDO.
def var v-rate-t        as   DEC NO-UNDO.
def var v-frtr-t        as   DEC NO-UNDO.
def var v-inv-tax       as   DEC NO-UNDO.
def var v-frt-tax       as   DEC NO-UNDO.
DEF VAR v-found         AS   logi NO-UNDO.
DEF VAR ld              AS   DEC NO-UNDO.

def buffer b-stax       for stax.

form header "Advantzware Fixed Assets"
    "Page:"                     to 117 space(4)
    page-number  format ">>>>9"
    "Fixed Assets Master Listing"     at 1
    "As-of date:"               to 117
    string(today)
    string(time,"hh:mm") format "x(5)"
    fill("-",132) format "x(132)"

    "Asset"                 AT 1
    "Asset Description"     AT 10
    "Acquired"              AT 42
    "New Used"              AT 54 /* YR2000 */
    "Salvage"               AT 67
    "Life-Bk"               AT 79
    "Cost Book"             AT 91
    "Dep-Basis-Bk"          AT 103
    "Acc Dep Book"        AT 118
    SKIP
    "Code"                  AT 1
    "GL Group"              AT 10
    "Loc"                   AT 19
    "Yr/Depr"               at 23
    "Service"               AT 42
    "Status"                AT 54   
    "Entity"                AT 67   
    "Life-T1"               AT 79
    " "                     at 91
    "Dep-Basis-T1"          AT 103
    "Acc Dep Tax1"          AT 118
    SKIP
    "Parent #"              at 1
    "Book  Tax1    Tax2"    AT 10
    "P.O. No."              AT 42
    "Bus%"                  TO 57
    "ITC Amt"               AT 67
    "Life-T2"               AT 79
    "No. Assets"            TO 99
    "Dep-Basis-T2"          AT 103
    "Acc Dep Tax2"          AT 118
    SKIP
    "Sort1"                 AT 10
    "Sort2   "              AT 25
    "Serial#"               at 34
    "Book Life"             at 79
    "Sl-Conv-Amt"           at 103
    "Last Auto Depr"        at 118
    SKIP
    "Exch. Rate"            at 1     /* rpm 01/95 */
    "Job no"                at 15    /* rpm 01/95 */
    "Adjustment"            at 34    /* rpm 01/95 */
    "Tag-from      Tag-to"  at 54    /* rpm 01/95 */
    fill("-",132) format "x(132)" skip(1)
    with stream-io page-top no-box width 132 frame top no-attr-space.

{sa/sa-sls01.i}

SESSION:SET-WAIT-STATE ("general").

EMPTY TEMP-TABLE tt-report.

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).
   excelheader = "Tax Authority,Cust Name,Date,Invoice,Gross Sales $,"
               + "Tax $,Freight $,Net Sales $".
   PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

if td-show-parm then run show-param.

assign
    beg-code = fiFromAsset:screen-value in frame {&frame-name}
    end-code = fiToAsset:screen-value
    beg-gl = fiFromGL:screen-value
    end-gl = fiToGL:screen-value
    beg-loc = fiFromLoc:screen-value
    end-loc = fiToLoc:screen-value
    sort1 = fiSort1:screen-value
    sort2 = fiSort2:screen-value
    torder = fiSortOrder:screen-value
    t-status = fiStatus:screen-value
    by-list = fiByList:screen-value.

MAIN-LOOP:
FOR EACH FA-MAST WHERE 
    FA-MAST.ASSET-CODE GE BEG-Code AND
    FA-MAST.ASSET-CODE LE END-CODE AND
    fa-mast.fa-entity = fa-control.fa-entity and
    FA-MAST.GL-CODE GE BEG-GL AND FA-MAST.GL-CODE LE END-GL AND
    FA-MAST.LOCATION GE BEG-LOC AND FA-MAST.LOCATION LE END-LOC AND
    (IF SORT1 NE "" THEN CAN-DO(SORT1,SORT-CODE1) ELSE TRUE) AND
    (IF SORT2 NE "" THEN CAN-DO(SORT2,SORT-CODE2) ELSE TRUE) AND
    (INDEX(t-status,FA-MAST.ASSET-STATUS) NE 0) and 
    (index(liste#,string(fa-mast.child-par)) ne 0)
    BY   (IF torder = "l":U THEN location
    ELSE IF torder = "1":U THEN sort-code1
    ELSE IF torder = "2":U THEN sort-code2
    ELSE IF torder = "g":U THEN gl-code
    ELSE asset-code) 
    ON ERROR UNDO, LEAVE MAIN-LOOP WITH stream-io FRAME BODY
    NO-ATTR-SPACE NO-LABELS NO-BOX WIDTH 255 :  /* mod-sh 11/91 */

    if by-list = "O":U then do:
        find first fa-mast# where 
            fa-mast#.par-asset = fa-mast.asset-code and 
            fa-mast#.fa-entity = fa-mast.fa-entity
            no-lock no-error.
        if not available fa-mast# then next.
    end.

    find address1 of fa-mast no-lock no-error.
    if fa-mast.asset-status ne "R":U then do:
        acc-book = fa-mast.acc-dep-book + fa-mast.cy-dep-book.
        acc-tax1 = fa-mast.acc-dep-tax1 + fa-mast.cy-dep-tax-1.
        acc-tax2 = fa-mast.acc-dep-tax2 + fa-mast.cy-dep-tax-2.
    end.
    else do:
        acc-book = fa-mast.acc-dep-book .
        acc-tax1 = fa-mast.acc-dep-tax1 .
        acc-tax2 = fa-mast.acc-dep-tax2 .
    end.

    DISPLAY
        SKIP
        fa-mast.ASSET-CODE              AT 1
        fa-mast.ASSET-DESC              AT 10
        fa-mast.DATE-AQUIRED            AT 42
        fa-mast.NEW-USED                AT 54
        fa-mast.SALVAGE                 AT 60
        fa-mast.LIFE-BOOK               AT 81
        fa-mast.COST-BOOK               AT 86
        fa-mast.DEP-BASIS-BK            AT 101
        ACC-BOOK            AT 116
        SKIP
        fa-mast.child-par               at 1
        fa-mast.GL-CODE                 AT 10
        FA-MAST.LOCATION        AT 19
        fa-mast.yr-of-depr      at 25
        fa-mast.DATE-SERVICE            AT 42
        fa-mast.ASSET-STATUS            AT 54
        fa-mast.entity-code             at 67
        fa-mast.LIFE-TAX-1              AT 83
        fa-mast.DEP-BASIS-T1            AT 101
        ACC-TAX1            AT 116
        SKIP
        fa-mast.par-asset       at 1
        FA-MAST.METHOD-BOOK     AT 10
        FA-MAST.METHOD-TAX-1    AT 18
        FA-MAST.METHOD-TAX-2    AT 25
        fa-mast.PURCH-ORDER#    format "x(8)"        AT 42
        fa-mast.business-%  format "999"  to 57  /* inserted by RAW on 4/9/90. */
        fa-mast.ITC-AMT    format "->,>>>,>>9.99"   to 73
        fa-mast.LIFE-TAX-2              AT 83
        fa-mast.multiple  to 95  format "99"      /* inserted by RAW on 4/9/90. */
        fa-mast.DEP-BASIS-T2   format "->,>>>,>>9.99"   to 114
        ACC-TAX2            AT 116
        SKIP
        fa-mast.sort-code1  format "x(8)"  at 10
        fa-mast.sort-code2  format "x(8)"  at 25
        fa-mast.serial#
        fa-mast.mth-year       at 81
        fa-mast.sl-conv-amt    to 114
        fa-mast.last-autodepr-date to 130
        SKIP                              /* rpm 01/95 */
        fa-mast.exch-rate      at 1       /* rpm 01/95 */
        fa-mast.job-no         at 15      /* rpm 01/95 */
        fa-mast.sec-179        at 25      /* rpm 01/95 */
        with stream-io no-attr-space no-box.

    for each fa-tags no-lock where 
        fa-tags.asset-code eq fa-mast.asset-code and 
        fa-tags.fa-entity eq fa-mast.fa-entity:

        display 
            fa-tags.tag-nof at 53 space (4)
            fa-tags.tag-not
            skip with stream-io no-labels no-attr-space no-box.
    end.

    ACCUM 1 (COUNT).

    if by-list = "R":U  or by-list = "O":U then do:
        for each fa-mast# no-lock where 
            fa-mast#.par-asset = fa-mast.asset-code
            with stream-io NO-ATTR-SPACE NO-LABELS NO-BOX WIDTH 255:  /* mod-sh 11/91 */

            if fa-mast.asset-status ne "R":U then do:
                acc-book = fa-mast#.acc-dep-book + fa-mast#.cy-dep-book.
                acc-tax1 = fa-mast#.acc-dep-tax1 + fa-mast#.cy-dep-tax-1.
                acc-tax2 = fa-mast#.acc-dep-tax2 + fa-mast#.cy-dep-tax-2.
            end.
            else do:
                acc-book = fa-mast#.acc-dep-book .
                acc-tax1 = fa-mast#.acc-dep-tax1 .
                acc-tax2 = fa-mast#.acc-dep-tax2 .
            end.
  
            find address1 of fa-mast# no-lock no-error.

            DISPLAY
                SKIP
                fa-mast#.ASSET-CODE              AT 1
                fa-mast#.ASSET-DESC              AT 10
                fa-mast#.DATE-AQUIRED            AT 42
                fa-mast#.NEW-USED                AT 54
                fa-mast#.SALVAGE                 AT 60
                fa-mast#.LIFE-BOOK               AT 81
                fa-mast#.COST-BOOK               AT 86
                fa-mast#.DEP-BASIS-BK            AT 101
                ACC-BOOK            AT 116
                SKIP
                fa-mast#.child-par               at 1
                fa-mast#.GL-CODE                 AT 10
                fa-mast#.LOCATION        AT 19
                fa-mast#.yr-of-depr      at 25
                fa-mast#.DATE-SERVICE            AT 42
                fa-mast#.ASSET-STATUS            AT 54
                fa-mast#.fa-entity          at 67
                fa-mast#.LIFE-TAX-1              AT 83
                fa-mast#.DEP-BASIS-T1            AT 101
                ACC-TAX1            AT 116
                SKIP
                fa-mast#.par-asset       at 1
                fa-mast#.METHOD-BOOK     AT 10
                fa-mast#.METHOD-TAX-1    AT 18
                fa-mast#.METHOD-TAX-2    AT 25
                fa-mast#.PURCH-ORDER#    format "x(8)"        AT 42
                fa-mast#.business-%  format "999"  to 57  /* inserted by RAW on 4/9/90. */
                fa-mast#.ITC-AMT    format "->,>>>,>>9.99"   to 73
                fa-mast#.LIFE-TAX-2              AT 83
                fa-mast#.multiple  to 95  format "99"      /* inserted by RAW on 4/9/90. */
                fa-mast#.DEP-BASIS-T2   format "->,>>>,>>9.99"   to 114
                ACC-TAX2            AT 116
                SKIP
                fa-mast#.sort-code1  format "x(8)"  at 10
                fa-mast#.sort-code2  format "x(8)"  at 25
                fa-mast#.serial#
                fa-mast#.mth-year       at 81
                fa-mast#.sl-conv-amt    to 114
                fa-mast#.last-autodepr-date to 130
                SKIP                               /* rpm 01/95 */
                fa-mast#.entity-code    at 2       /* rpm 01/95 */
                fa-mast#.job-no         at 15      /* rpm 01/95 */
                fa-mast#.sec-179        at 25      /* rpm 01/95 */
                with stream-io no-attr-space no-box.

            for each fa-tags no-lock where 
                fa-tags.asset-code eq fa-mast#.asset-code and 
                fa-tags.fa-entity eq fa-mast#.fa-entity:
                display 
                    fa-tags.tag-nof at 53 space (4)
                    fa-tags.tag-not
                    skip with stream-io no-labels no-attr-space no-box.
            end.

            ACCUM 1 (COUNT).
        end.   /* OF FOR EACH FA-MAST# */
    end.   /* OF IF SEE-CHILD      */

    display skip(1) with stream-io frame skipline.

END.                       /* END OF MAIN-LOOP */
    VIEW FRAME r-top.


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

