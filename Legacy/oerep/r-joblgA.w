&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oerep\r-joblog.w

  Description: Job Log

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

DEF VAR v-program AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF {1} SHARED var v-print-fmt  as char NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS begin_cust-no end_cust-no RECT-6 ~
begin_ord-no RECT-7 end_ord-no begin_i-no end_i-no begin_ord-date ~
end_ord-date tb_prt tb_sort-by-cust tb_due-date rs-due-date tb_cust-name ~
rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel ~
fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no begin_ord-no ~
end_ord-no begin_i-no end_i-no begin_ord-date end_ord-date tb_prt ~
tb_sort-by-cust tb_due-date rs-due-date tb_cust-name rd-dest lv-ornt ~
lines-per-page lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel ~
fi_file 

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

DEFINE VARIABLE begin_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/01 
     LABEL "From Order Date" 
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
     LABEL "To Order Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 
     LABEL "Ending Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-joblog.csv" 
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
     SIZE 20 BY 6.67 NO-UNDO.

DEFINE VARIABLE rs-due-date AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "View Tab", "View",
"Item Tab", "Item"
     SIZE 28 BY .95 NO-UNDO.

DEFINE VARIABLE tb_cust-name AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Cust. #", "#",
"Cust. Name", "name"
     SIZE 28 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 10.52.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.05.

DEFINE VARIABLE tb_due-date AS LOGICAL INITIAL no 
     LABEL "Print Due Date?" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.8 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_prt AS LOGICAL INITIAL no 
     LABEL "Print Part#, Est#, Die#, CAD#?" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_sort-by-cust AS LOGICAL INITIAL no 
     LABEL "Sort by Customer#/Name?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27.8 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust-no AT ROW 2.33 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 2.33 COL 69 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_ord-no AT ROW 3.29 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_ord-no AT ROW 3.29 COL 69 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     begin_i-no AT ROW 4.24 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_i-no AT ROW 4.24 COL 69 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_ord-date AT ROW 5.19 COL 28 COLON-ALIGNED
     end_ord-date AT ROW 5.19 COL 69 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     tb_prt AT ROW 6.33 COL 30.2
     tb_sort-by-cust AT ROW 7.19 COL 30.2
     tb_due-date AT ROW 8.05 COL 30.2
     rs-due-date AT ROW 8 COL 51 NO-LABEL WIDGET-ID 2
     tb_cust-name AT ROW 8.86 COL 30 NO-LABEL
     rd-dest AT ROW 11.43 COL 6 NO-LABEL
     lv-ornt AT ROW 11.91 COL 31 NO-LABEL
     lines-per-page AT ROW 11.91 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 13.57 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 14.52 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 16.67 COL 31
     tb_excel AT ROW 17.95 COL 31
     tb_runExcel AT ROW 17.95 COL 73 RIGHT-ALIGNED
     fi_file AT ROW 18.91 COL 29 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 20.81 COL 26
     btn-cancel AT ROW 20.81 COL 57
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 10.71 COL 4
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 FGCOLOR 0 
     RECT-6 AT ROW 10 COL 1
     RECT-7 AT ROW 1 COL 1
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
         TITLE              = "Job Log"
         HEIGHT             = 21.81
         WIDTH              = 95.8
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
   FRAME-NAME Custom                                                    */
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

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
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
ON END-ERROR OF C-Win /* Job Log */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Job Log */
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


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no C-Win
ON LEAVE OF begin_i-no IN FRAME FRAME-A /* Beginning Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-date C-Win
ON LEAVE OF begin_ord-date IN FRAME FRAME-A /* From Order Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-no C-Win
ON LEAVE OF begin_ord-no IN FRAME FRAME-A /* Beginning Order# */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 03.28.2017 @ 10:43:02 am */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  ASSIGN {&DISPLAYED-OBJECTS}.

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
           {custom/asifax.i &type= "Customer"
                            &begin_cust=begin_cust-no
                            &END_cust= begin_cust-no
                            &fax-subject=c-win:TITLE
                            &fax-body=c-win:TITLE
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = "Customer"
                             &begin_cust= begin_cust-no
                             &END_cust=begin_cust-no
                             &mail-subject=c-win:TITLE
                             &mail-body=c-win:TITLE
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Customer"
                                  &begin_cust= begin_cust-no
                                  &END_cust=begin_cust-no
                                  &mail-subject=c-win:TITLE
                                  &mail-body=c-win:TITLE
                                  &mail-file=list-name }

           END.
       END. 
      WHEN 6 THEN RUN output-to-port.
  end case. 
  SESSION:SET-WAIT-STATE ("").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 03.28.2017 @ 10:43:02 am */
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


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no C-Win
ON LEAVE OF end_i-no IN FRAME FRAME-A /* Ending Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-date C-Win
ON LEAVE OF end_ord-date IN FRAME FRAME-A /* To Order Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-no C-Win
ON LEAVE OF end_ord-no IN FRAME FRAME-A /* Ending Order# */
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
  IF {&self-name}:SCREEN-VALUE EQ "P" THEN tb_prt:SCREEN-VALUE = "NO".
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


&Scoped-define SELF-NAME tb_due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_due-date C-Win
ON VALUE-CHANGED OF tb_due-date IN FRAME FRAME-A /* Print Due Date? */
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


&Scoped-define SELF-NAME tb_prt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt C-Win
ON VALUE-CHANGED OF tb_prt IN FRAME FRAME-A /* Print Part#, Est#, Die#, CAD#? */
DO:
  ASSIGN {&self-name}.

  RUN set-ornt.
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


&Scoped-define SELF-NAME tb_sort-by-cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sort-by-cust C-Win
ON VALUE-CHANGED OF tb_sort-by-cust IN FRAME FRAME-A /* Sort by Customer#/Name? */
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
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p on 03.28.2017 @ 10:43:02 am */
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

  assign
   begin_ord-date = date(1,1,year(today))
   end_ord-date   = today.

  RUN enable_UI.

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO begin_cust-no.
  END.

  {methods/nowait.i}

    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images.p on 03.28.2017 @ 10:43:45 am */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images.p on 03.28.2017 @ 10:43:45 am */
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p on 03.28.2017 @ 10:43:02 am */
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
  DISPLAY begin_cust-no end_cust-no begin_ord-no end_ord-no begin_i-no end_i-no 
          begin_ord-date end_ord-date tb_prt tb_sort-by-cust tb_due-date 
          rs-due-date tb_cust-name rd-dest lv-ornt lines-per-page lv-font-no 
          lv-font-name td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_cust-no end_cust-no RECT-6 begin_ord-no RECT-7 end_ord-no 
         begin_i-no end_i-no begin_ord-date end_ord-date tb_prt tb_sort-by-cust 
         tb_due-date rs-due-date tb_cust-name rd-dest lv-ornt lines-per-page 
         lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
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
  run scr-rpt.w (list-name,c-win:title,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* -------------------------------------------------oe/rep/joblog.p 04/95 DAR */
/* Job Log Report                                                             */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}

def var v-fcust as char extent 2 init ["", "zzzzzzzz"] NO-UNDO.
def var v-ford-no as int format ">>>>>9" extent 2 init [0, 999999] NO-UNDO.
def var v-fdate as date format "99/99/9999" extent 2 init [01/01/0001, today] NO-UNDO.
def var v-fitem as char format "x(15)" extent 2 init ["", "zzzzzzzzzzzzzzz"] NO-UNDO.
def var v-frst as log init NO NO-UNDO.
def var changed as log init NO NO-UNDO.
def var job-num as char format "x(9)" NO-UNDO.
def var v-i-no like oe-ordl.i-no no-undo.
def var v-i-name like oe-ordl.i-name no-undo.
DEF VAR v-cust AS CHAR FORMAT "X(17)" NO-UNDO.
DEF VAR est-num AS CHAR  NO-UNDO.

DEFINE VARIABLE ExcelHeader AS CHARACTER  NO-UNDO.

SESSION:SET-WAIT-STATE ("general").

FORMAT job-num              COLUMN-LABEL "Job!Number"
       job-hdr.ord-no       COLUMN-LABEL "Order!Number"
       v-cust               COLUMN-LABEL "!Customer" FORMAT "X(17)"
       v-i-no               COLUMN-LABEL "!Item Number"
       v-i-name             COLUMN-LABEL "!Item Name"
       oe-ordl.po-no        COLUMN-LABEL "!Cust PO#"
       job-hdr.qty          COLUMN-LABEL "Quantity!Ordered"
       oe-ord.ord-date      COLUMN-LABEL "Order!Date"
                            FORMAT "99/99/99"
       oe-ord.due-date      COLUMN-LABEL "Due!Date"
                            FORMAT "99/99/99"
       SKIP
    WITH FRAME detail STREAM-IO WIDTH 156 DOWN. /*132*/

FORMAT job-num              COLUMN-LABEL "Job!Number"
       job-hdr.ord-no       COLUMN-LABEL "Order!Number"
       v-cust               COLUMN-LABEL "!Customer" FORMAT "X(17)"
       v-i-no               COLUMN-LABEL "!Item Number"
       v-i-name             COLUMN-LABEL "!Item Name"
       oe-ordl.po-no        COLUMN-LABEL "!Cust PO#"
       job-hdr.qty          COLUMN-LABEL "Quantity!Ordered"
       oe-ord.ord-date      COLUMN-LABEL "Order!Date"
                            FORMAT "99/99/99"
       oe-ord.due-date      COLUMN-LABEL "Due!Date"
                            FORMAT "99/99/99"
       oe-ordl.part-no      COLUMN-LABEL "!Cust Part#"
       est-num            COLUMN-LABEL "!    Est#" FORMAT "X(8)"
       eb.die-no            COLUMN-LABEL "!Die#"
       eb.cad-no            COLUMN-LABEL "!Cad#"                         
       SKIP
    WITH FRAME detail-w STREAM-IO WIDTH 230 DOWN. /*180*/

FORMAT job-num              COLUMN-LABEL "Job!Number"
       job-hdr.ord-no       COLUMN-LABEL "Order!Number"
       v-cust               COLUMN-LABEL "!Customer" FORMAT "X(17)"
       v-i-no               COLUMN-LABEL "!Item Number"
       v-i-name             COLUMN-LABEL "!Item Name"
       oe-ordl.po-no        COLUMN-LABEL "!Cust PO#"
       job-hdr.qty          COLUMN-LABEL "Quantity!Ordered"
       oe-ord.ord-date      COLUMN-LABEL "Order!Date"
                            FORMAT "99/99/99"
       SKIP
    WITH FRAME detail-y STREAM-IO WIDTH 156 DOWN. /*132*/

FORMAT job-num              COLUMN-LABEL "Job!Number"
       job-hdr.ord-no       COLUMN-LABEL "Order!Number"
       v-cust               COLUMN-LABEL "!Customer" FORMAT "X(17)"
       v-i-no               COLUMN-LABEL "!Item Number"
       v-i-name             COLUMN-LABEL "!Item Name"
       oe-ordl.po-no        COLUMN-LABEL "!Cust PO#"
       job-hdr.qty          COLUMN-LABEL "Quantity!Ordered"
       oe-ord.ord-date      COLUMN-LABEL "Order!Date"
                            FORMAT "99/99/99"
       oe-ordl.part-no      COLUMN-LABEL "!Cust Part#"
       est-num            COLUMN-LABEL "!    Est#" FORMAT "X(8)"
       eb.die-no            COLUMN-LABEL "!Die#"
       eb.cad-no            COLUMN-LABEL "!Cad#"                         
       SKIP
    WITH FRAME detail-z STREAM-IO WIDTH 230 DOWN. /*180*/

ASSIGN
 str-tit2 = c-win:TITLE + " by Job#"
 {sys/inc/ctrtext.i str-tit2 112}

 v-fcust[1]   = begin_cust-no
 v-fcust[2]   = end_cust-no
 v-ford-no[1] = begin_ord-no
 v-ford-no[2] = end_ord-no
 v-fitem[1]   = begin_i-no
 v-fitem[2]   = end_i-no
 v-fdate[1]   = begin_ord-date
 v-fdate[2]   = end_ord-date.

changed = v-fdate[1] ne date(01,01,year(today)) or v-fdate[2] ne today.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

IF tb_excel THEN 
DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "Job Num,Order Num,Customer,Itm Num,Itm Name,Cst PO,Ord Amt,Ord Dte,".
  IF tb_due-date THEN
     excelheader = excelheader + "Due Date,".
  IF tb_prt THEN
     excelheader = excelheader + "Cst Part,Est#,Die#,CAD#".

  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

DISPLAY "" WITH FRAME r-top.

    for each job-hdr
       where job-hdr.company eq cocode
         and job-hdr.opened  eq yes
         and job-hdr.ord-no  ge v-ford-no[1]
         and job-hdr.ord-no  le v-ford-no[2]
         and job-hdr.cust-no ge v-fcust[1]
         and job-hdr.cust-no le v-fcust[2]
         and job-hdr.i-no    ge v-fitem[1]
         and job-hdr.i-no    le v-fitem[2]
        use-index opened no-lock,

        first job
        where job.company eq cocode
          and job.job     eq job-hdr.job
          and job.job-no  eq job-hdr.job-no
          and job.job-no2 eq job-hdr.job-no2
        no-lock,
        FIRST cust FIELDS(cust-no NAME) WHERE
              cust.company EQ cocode AND
              cust.cust-no EQ job-hdr.cust-no
              NO-LOCK

        break by (if tb_sort-by-cust AND
                     tb_cust-name EQ "#" then job-hdr.cust-no
                  ELSE IF tb_sort-by-cust AND
                       tb_cust-name EQ "name" THEN cust.NAME
                  else "")
              by job-hdr.job-no
              by job-hdr.job-no2:

    {custom/statusMsg.i "'Processing Order # ' + string(job-hdr.ord-no)"} 

        release oe-ord.
        if job-hdr.ord-no ne 0 then
          find first oe-ord
         where oe-ord.company eq cocode
           and oe-ord.ord-no  eq job-hdr.ord-no
          no-lock no-error.

        if avail oe-ord then
          if oe-ord.ord-date lt v-fdate[1] or
             oe-ord.ord-date gt v-fdate[2] or
             index("CZ",oe-ord.stat) ne 0  then next.
          else.
        else     
          if job.start-date lt v-fdate[1] or
             job.start-date gt v-fdate[2] or
             job.start-date eq ?          then next.

        ASSIGN
          v-i-name = ""
          v-i-no = "".

        RELEASE oe-ordl.

        if avail oe-ord then
        DO:
           find first oe-ordl WHERE
                oe-ordl.company eq cocode AND
                oe-ordl.ord-no  eq job-hdr.ord-no AND
                oe-ordl.i-no    eq job-hdr.i-no AND
                oe-ordl.job-no  EQ job-hdr.job-no AND
                oe-ordl.job-no2 EQ job-hdr.job-no2
                no-lock no-error.

           IF NOT AVAIL oe-ordl THEN
              find first oe-ordl WHERE
                   oe-ordl.company eq cocode AND
                   oe-ordl.ord-no  eq job-hdr.ord-no AND
                   oe-ordl.i-no    eq job-hdr.i-no
                   no-lock no-error.
        END.

        if avail oe-ordl then 
           v-i-name = oe-ordl.i-name.
        else
        do:
           find first itemfg
                where itemfg.company eq cocode
                  and itemfg.i-no eq job-hdr.i-no
               no-lock no-error.
           if avail itemfg then 
              v-i-name = itemfg.i-name.
           else 
              v-i-name = "".
        end.

        assign
          v-i-no  = job-hdr.i-no
          job-num = trim(job-hdr.job-no) + "-" + string(job-hdr.job-no2,"99").

        IF tb_cust-name EQ "#" THEN
           v-cust = job-hdr.cust-no.
        ELSE
           v-cust = cust.NAME.

        RELEASE eb.
        IF job.est-no NE "" THEN
        FOR EACH eb
           WHERE eb.company EQ cocode
             AND eb.est-no  EQ job.est-no
            NO-LOCK
           BREAK BY eb.form-no DESC
                BY eb.blank-no DESC:
            IF LAST(eb.form-no) OR
               (eb.form-no EQ job-hdr.frm AND
                eb.blank-no EQ job-hdr.blank-no) THEN LEAVE.
        END.

        IF AVAIL eb THEN
             ASSIGN est-num =  TRIM( FILL(" ",8 - LENGTH(TRIM(eb.est-no))) + TRIM(eb.est-no))  .
        ELSE ASSIGN est-num = "" .

        IF tb_prt THEN 
        DO:
           IF tb_due-date THEN
           DO:
              display 
                   job-num
                   job-hdr.ord-no
                   v-cust
                   job-hdr.i-no @ v-i-no
                   v-i-name
                   oe-ordl.po-no     when avail oe-ordl
                   job-hdr.qty
                   oe-ord.ord-date   when avail oe-ord
                     job.start       when not avail oe-ord @ oe-ord.ord-date
                   oe-ord.due-date   WHEN tb_due-date AND rs-due-date EQ "View" AND AVAIL oe-ord
                   oe-ordl.req-date WHEN tb_due-date AND rs-due-date EQ "Item" AND AVAIL oe-ordl @ oe-ord.due-date
                   job.due-date      WHEN tb_due-date AND NOT AVAIL oe-ord
                                     @ oe-ord.due-date
                   oe-ordl.part-no   when avail oe-ordl
                   est-num
                   eb.die-no         when avail eb
                   eb.cad-no         when avail eb
                 with FRAME detail-w.
              down with FRAME detail-w.
           END.
           ELSE
              display 
                   job-num
                   job-hdr.ord-no
                   v-cust
                   job-hdr.i-no @ v-i-no
                   v-i-name
                   oe-ordl.po-no     when avail oe-ordl
                   job-hdr.qty
                   oe-ord.ord-date   when avail oe-ord
                     job.start       when not avail oe-ord @ oe-ord.ord-date
                   oe-ord.due-date   WHEN tb_due-date AND rs-due-date EQ "View" AND AVAIL oe-ord
                   oe-ordl.req-date  WHEN tb_due-date AND rs-due-date EQ "Item" AND AVAIL oe-ordl @ oe-ord.due-date
                   oe-ordl.part-no   when avail oe-ordl
                   est-num
                   eb.die-no         when avail eb 
                   eb.cad-no         when avail eb
                 with FRAME detail-z.
              down with FRAME detail-z.

            IF tb_excel THEN 
            DO:
               PUT STREAM excel UNFORMATTED
                   '"' job-num                          '",'
                   '"' job-hdr.ord-no                   '",'
                   '"' v-cust                           '",'
                   '"' job-hdr.i-no                     '",'
                   '"' v-i-name                         '",'
                   '"' IF AVAIL oe-ordl THEN
                          STRING(oe-ordl.po-no) ELSE "" '",'
                   '"' job-hdr.qty                      '",'
                   '"' IF AVAIL oe-ord THEN STRING(oe-ord.ord-date)
                       ELSE STRING(job.START)           '",'.

               IF tb_due-date THEN
                  PUT STREAM excel UNFORMATTED
                      '"' IF AVAIL oe-ord AND rs-due-date EQ "View" THEN
                          STRING(oe-ord.due-date)
                          ELSE IF AVAIL oe-ordl AND rs-due-date EQ "Item" THEN
                          STRING(oe-ordl.req-date)
                          ELSE STRING(job.due-date) '",'.

               PUT STREAM excel UNFORMATTED
                   '"' IF AVAIL oe-ord THEN oe-ordl.part-no
                       ELSE ""                          '",'
                   '"' IF AVAIL eb THEN eb.est-no ELSE "" '",'
                   '"' IF AVAIL eb THEN eb.die-no ELSE "" '",'
                   '"' IF AVAIL eb THEN eb.cad-no ELSE "" '",'
                  SKIP.
            END.
        END.
        ELSE
        DO:
           IF tb_due-date THEN
           DO:
              display 
                  job-num
                  job-hdr.ord-no
                  v-cust
                  job-hdr.i-no @ v-i-no
                  v-i-name
                  oe-ordl.po-no     when avail oe-ordl
                  job-hdr.qty
                  oe-ord.ord-date   when avail oe-ord
                    job.start       when not avail oe-ord @ oe-ord.ord-date
                  oe-ord.due-date   WHEN tb_due-date AND rs-due-date EQ "View" AND AVAIL oe-ord
                  oe-ordl.req-date  WHEN tb_due-date AND rs-due-date EQ "Item" AND AVAIL oe-ordl @ oe-ord.due-date
                  job.due-date      WHEN tb_due-date AND NOT AVAIL oe-ord
                                    @ oe-ord.due-date
                with FRAME detail.
              down with FRAME detail.
           END.
           ELSE
           DO:
              display 
                  job-num
                  job-hdr.ord-no
                  v-cust
                  job-hdr.i-no @ v-i-no
                  v-i-name
                  oe-ordl.po-no     when avail oe-ordl
                  job-hdr.qty
                  oe-ord.ord-date   when avail oe-ord
                  job.start       when not avail oe-ord @ oe-ord.ord-date
                with FRAME detail-y.
              down with FRAME detail-y.
           END.

            IF tb_excel THEN 
            DO:
               PUT STREAM excel UNFORMATTED
                   '"' job-num                          '",'
                   '"' job-hdr.ord-no                   '",'
                   '"' v-cust                           '",'
                   '"' job-hdr.i-no                     '",'
                   '"' v-i-name                         '",'
                   '"' IF AVAIL oe-ordl THEN
                          STRING(oe-ordl.po-no) ELSE "" '",'
                   '"' job-hdr.qty                      '",'
                   '"' IF AVAIL oe-ord THEN STRING(oe-ord.ord-date)
                       ELSE STRING(job.START)           '",'.

               IF tb_due-date THEN
                  PUT STREAM excel UNFORMATTED
                      '"' IF AVAIL oe-ord AND rs-due-date EQ "View" THEN
                          STRING(oe-ord.due-date)
                          ELSE IF AVAIL oe-ordl AND rs-due-date EQ "Item" THEN
                          STRING(oe-ordl.req-date)
                          ELSE STRING(job.due-date) '",'.

               PUT STREAM excel UNFORMATTED SKIP.
            END.
        END.
    end.  /* for each */

    IF tb_excel THEN 
    DO:
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-ornt C-Win 
PROCEDURE set-ornt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF tb_prt:SCREEN-VALUE EQ "YES" THEN
      ASSIGN
       lv-ornt:SCREEN-VALUE = "L"
       lv-ornt:SENSITIVE    = NO.

    ELSE
      ASSIGN
       lv-ornt:SCREEN-VALUE = "P"
       lv-ornt:SENSITIVE    = YES.

    {custom/chgfont.i}
  END.

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

