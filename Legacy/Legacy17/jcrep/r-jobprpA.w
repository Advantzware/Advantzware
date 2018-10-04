&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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


DEF VAR v-print-fmt AS CHARACTER.
DEF VAR is-xprint-form AS LOGICAL.
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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 from_date to_date begin_cust ~
end_cust begin_job-no begin_job-no2 end_job-no end_job-no2 begin_prep ~
end_prep rsGroupBy tb_total rd-dest lv-ornt lines-per-page lv-font-no ~
td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS from_date to_date begin_cust end_cust ~
begin_job-no begin_job-no2 end_job-no end_job-no2 begin_prep end_prep ~
rsGroupBy tb_total rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
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

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "00" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE begin_prep AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Prep Code" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "99" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE end_prep AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Prep Code" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-jobprp.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE from_date AS DATE FORMAT "99/99/9999":U 
     LABEL "Beginning Due Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

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

DEFINE VARIABLE to_date AS DATE FORMAT "99/99/9999":U 
     LABEL "Ending Due Date" 
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
     SIZE 21 BY 6.67 NO-UNDO.

DEFINE VARIABLE rsGroupBy AS CHARACTER INITIAL "Customer" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer", "Customer",
"Prep Code", "Prep"
     SIZE 46 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.57.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.05.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY 1
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_total AS LOGICAL INITIAL yes 
     LABEL "Show Totals?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     from_date AT ROW 2.43 COL 27 COLON-ALIGNED
     to_date AT ROW 2.43 COL 68.4 COLON-ALIGNED
     begin_cust AT ROW 3.43 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 2
     end_cust AT ROW 3.43 COL 68.4 COLON-ALIGNED HELP
          "Enter Ending Customer Number" WIDGET-ID 4
     begin_job-no AT ROW 4.52 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Job Number" WIDGET-ID 30
     begin_job-no2 AT ROW 4.52 COL 39 COLON-ALIGNED HELP
          "Enter Beginning Job Number" WIDGET-ID 32
     end_job-no AT ROW 4.52 COL 68 COLON-ALIGNED HELP
          "Enter Ending Job Number" WIDGET-ID 34
     end_job-no2 AT ROW 4.52 COL 80 COLON-ALIGNED HELP
          "Enter Ending Job Number" WIDGET-ID 36
     begin_prep AT ROW 5.52 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Order Number" WIDGET-ID 26
     end_prep AT ROW 5.52 COL 68.4 COLON-ALIGNED HELP
          "Enter Ending Item Number" WIDGET-ID 28
     rsGroupBy AT ROW 7.19 COL 29 NO-LABEL WIDGET-ID 14
     tb_total AT ROW 8.62 COL 31
     rd-dest AT ROW 11.71 COL 5 NO-LABEL
     lv-ornt AT ROW 11.71 COL 31 NO-LABEL
     lines-per-page AT ROW 11.71 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 13.14 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 14.1 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 15.29 COL 31
     tb_excel AT ROW 16 COL 71 RIGHT-ALIGNED
     tb_runExcel AT ROW 16 COL 93 RIGHT-ALIGNED
     fi_file AT ROW 17.1 COL 49 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 19.33 COL 24
     btn-cancel AT ROW 19.33 COL 59
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 10.76 COL 4
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Group by:" VIEW-AS TEXT
          SIZE 10 BY .95 AT ROW 7.19 COL 18 WIDGET-ID 22
     RECT-6 AT ROW 10.29 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 94.4 BY 25.48.


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
         TITLE              = "Prep Charge Cost Report"
         HEIGHT             = 20.52
         WIDTH              = 96.6
         MAX-HEIGHT         = 53.71
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 53.71
         VIRTUAL-WIDTH      = 384
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
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_prep:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_prep:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       from_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rsGroupBy:PRIVATE-DATA IN FRAME FRAME-A     = 
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

ASSIGN 
       tb_total:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       to_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Prep Charge Cost Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Prep Charge Cost Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Customer# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no C-Win
ON LEAVE OF begin_job-no IN FRAME FRAME-A /* Beginning Job# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no2 C-Win
ON LEAVE OF begin_job-no2 IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_prep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_prep C-Win
ON LEAVE OF begin_prep IN FRAME FRAME-A /* Beginning Prep Code */
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
    ASSIGN {&DISPLAYED-OBJECTS}.
  END.

  run run-report. 

  SESSION:SET-WAIT-STATE("general").

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=from_date
                            &END_cust=to_date
                            &fax-subject= c-win:TITLE 
                            &fax-body= c-win:title 
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = ''
                             &begin_cust=''
                             &END_cust=''
                             &mail-subject= c-win:TITLE 
                             &mail-body= c-win:TITLE 
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = ''
                                  &begin_cust=''
                                  &END_cust=''
                                  &mail-subject= c-win:TITLE 
                                  &mail-body= c-win:TITLE 
                                  &mail-file=list-name }

           END.

       END. 
       WHEN 6 THEN run output-to-port.
  end case. 
   SESSION:SET-WAIT-STATE("").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Customer# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no C-Win
ON LEAVE OF end_job-no IN FRAME FRAME-A /* Ending Job# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no2 C-Win
ON LEAVE OF end_job-no2 IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_prep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_prep C-Win
ON LEAVE OF end_prep IN FRAME FRAME-A /* Ending Prep Code */
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


&Scoped-define SELF-NAME from_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL from_date C-Win
ON LEAVE OF from_date IN FRAME FRAME-A /* Beginning Due Date */
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
  IF SELF:SCREEN-VALUE BEGINS "L" THEN 
    ASSIGN lv-font-no = "12"
           lines-per-page = 55
           lv-font-name = "Courier New Size=8 (15CPI)".

 ELSE
    ASSIGN lv-font-no = "10"
           lines-per-page = 99
           lv-font-name = "Courier NEW SIZE=6 (20 CPI)".

 DISPL lv-font-no lines-per-page lv-font-name WITH FRAME {&FRAME-NAME}.
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


&Scoped-define SELF-NAME tb_total
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_total C-Win
ON VALUE-CHANGED OF tb_total IN FRAME FRAME-A /* Show Totals? */
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


&Scoped-define SELF-NAME to_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL to_date C-Win
ON LEAVE OF to_date IN FRAME FRAME-A /* Ending Due Date */
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

   ASSIGN
   from_date  = DATE (1,1,YEAR(TODAY))
   to_date    = DATE (12,31,year(TODAY)).

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    APPLY "entry" TO from_date.
  END.

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
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
  DISPLAY from_date to_date begin_cust end_cust begin_job-no begin_job-no2 
          end_job-no end_job-no2 begin_prep end_prep rsGroupBy tb_total rd-dest 
          lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 from_date to_date begin_cust end_cust begin_job-no 
         begin_job-no2 end_job-no end_job-no2 begin_prep end_prep rsGroupBy 
         tb_total rd-dest lv-ornt lines-per-page lv-font-no td-show-parm 
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
{sys/form/r-top3w.f}

def buffer b-jh for job-hdr.

def var v-stat  as   char format "!"          init "O".
def var v-fjob  like job.job-no.
def var v-tjob  like v-fjob                   init "zzzzzz".
def var v-fjob2 like job.job-no2.
def var v-tjob2 like v-fjob2                  init 99.
def var v-fcust like job-hdr.cust-no          init "".
def var v-tcust like v-fcust                  init "zzzzzzzz".
def var v-fdate as   date format "99/99/9999" init 01/01/0001.
def var v-tdate like v-fdate                  init 12/31/9999.


DEF VAR liQty AS INT NO-UNDO.
DEF VAR ldStdCost AS DEC NO-UNDO.
DEF VAR ldExtCost AS DEC NO-UNDO.
DEF VAR liQtyCust AS INT NO-UNDO.
DEF VAR ldStdCostCust AS DEC NO-UNDO.
DEF VAR ldExtCostCust AS DEC NO-UNDO.
DEF VAR liQtyTot AS INT NO-UNDO.
DEF VAR ldStdCostTot AS DEC NO-UNDO.
DEF VAR ldExtCostTot AS DEC NO-UNDO.
DEF VAR lcHeader AS cha NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.
DEF VAR cCustomerName AS cha FORM "x(25)" NO-UNDO.
DEF VAR cPrepDscr AS cha FORM "x(25)" NO-UNDO.

IF rsGroupBy BEGINS "Customer" THEN
   lcHeader = "Customer Customer Name             Due Date   Job#       Prep            Code Description                 Qty       Cost       Extended".
ELSE 
   lcHeader = "Prep            Code Description          Customer Customer Name             Due Date   Job#              Qty       Cost       Extended".


form header skip(1)
          lcHeader FORM "x(139)" SKIP
            fill("-",135)           format "x(135)"
         with frame r-top.

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

  v-fjob        = fill(" ",6 - length(trim(begin_job-no))) +
                  trim(begin_job-no) + string(int(begin_job-no2),"99")
  v-tjob        = fill(" ",6 - length(trim(end_job-no)))   +
                  trim(end_job-no)   + string(int(end_job-no2),"99")  

  v-fcust       = begin_cust
  v-tcust       = END_cust
  v-fdate       = from_date
  v-tdate       = to_date. 

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  IF rsGroupBy BEGINS "Customer" THEN
     excelheader = "Customer Code,Customer Name,Due Date,Job#,Prep Code,Code Description,Qty,Cost,Extended".
  ELSE 
     excelheader = "Prep Code,Code Description,Customer Code,Customer Name,Due Date,Job#,Qty,Cost,Extended".

  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

display "" with frame r-top.

IF rsGroupBy BEGINS "Customer" THEN
for EACH ASI.job-prep WHERE job-prep.company eq cocode
                    and job-prep.job-no  ge substr(v-fjob,1,6)
                    and job-prep.job-no  le substr(v-tjob,1,6)
                    and fill(" ",6 - length(trim(job-PREP.job-no))) +
                  trim(job-prep.job-no) + string(job-prep.job-no2,"99")
                              ge v-fjob
              and fill(" ",6 - length(trim(job-prep.job-no))) +
                  trim(job-prep.job-no) + string(job-prep.job-no2,"99")
                              le v-tjob
                    /*job-prep.blank-no = job-hdr.blank-no AND*/
                    AND job-prep.CODE GE begin_prep 
                    AND job-prep.CODE LE END_prep NO-LOCK,
    FIRST job-hdr
        where job-hdr.company eq cocode
          and job-hdr.cust-no ge v-fcust
          and job-hdr.cust-no le v-tcust
          and job-hdr.job-no  EQ job-prep.job-no
          and job-hdr.job-no2  EQ job-prep.job-no2
          AND job-hdr.due-date GE v-fdate
          AND job-hdr.due-date LE v-tdate
        /*use-index cust-idx*/ NO-LOCK
                    BREAK BY job-hdr.cust-no BY job-hdr.job-no BY job-prep.CODE:



    IF FIRST-OF(job-prep.code) THEN
       ASSIGN liQty = 0
              ldExtCost = 0.

    IF FIRST-OF(job-hdr.cust-no) THEN
       ASSIGN liQtyCust = 0
              ldExtCostCust = 0.

    ASSIGN liQty = liQty + job-prep.qty
           ldExtCost = ldExtCost + job-prep.qty * job-prep.std-cost
           liQtyCust = liQtyCust + job-prep.qty
           ldExtCostCust = ldExtCostCust + job-prep.qty * job-prep.std-cost
           liQtyTot = liQtyTot + job-prep.qty
           ldExtCostTot = ldExtCostTot + job-prep.qty * job-prep.std-cost
           .


    IF LAST-OF(job-prep.CODE) THEN DO:
       FIND cust OF job-hdr NO-LOCK NO-ERROR.
       cCustomerName = IF AVAIL cust THEN cust.NAME ELSE job-hdr.cust-no.

       FIND FIRST prep WHERE prep.company EQ cocode
                         AND prep.loc     EQ locode
                         AND prep.code    EQ job-prep.CODE NO-LOCK NO-ERROR.
       cPrepDscr = IF AVAIL prep THEN prep.dscr ELSE job-prep.CODE.

       display job-hdr.cust-no
            cCustomerName
            job-hdr.due-date
            job-hdr.job-no + "-" + string(job-hdr.job-no2,"99") FORM "x(10)"
            job-prep.CODE FORM "x(15)"
            cPrepDscr  
            liQty
            ldExtCost / liQty @ ldStdCost
            ldExtCost FORM "->>,>>>,>>9.99"
            with frame det STREAM-IO width 140 no-box no-labels down.

       IF tb_excel THEN
         EXPORT STREAM excel DELIMITER ","
           job-hdr.cust-no
            cCustomerName
            job-hdr.due-date
            job-hdr.job-no + "-" + string(job-hdr.job-no2,"99") FORM "x(10)"
            job-prep.CODE FORM "x(15)"
            cPrepDscr  
            liQty
            ldExtCost / liQty
            ldExtCost FORM "->>,>>>,>>9.99"
            SKIP.

    END.
    IF LAST-OF(job-hdr.cust-no) AND tb_total  THEN DO:
       DOWN WITH FRAME DET.
       UNDERLINE cPrepDscr
                 liQty ldStdCost ldExtCost
           WITH FRAME det.
       DOWN WITH FRAME det.
       display /*cCustomerName*/
               "Total" @ cPrepDscr
            liQtyCust @ liQty
            ldExtCostCust / liQtyCust @ ldStdCost
            ldExtCostCust @ ldExtCost
            with frame det .
       DOWN WITH FRAME det.
       PUT SKIP(1).
    END.
    IF LAST(job-hdr.cust-no) AND tb_total THEN DO:
       DOWN WITH FRAME DET.
       UNDERLINE cPrepDscr
                 liQty ldStdCost ldExtCost
           WITH FRAME det.
       DOWN WITH FRAME det.
       display "Customer Total" @ cPrepDscr
               liQtyTot @ liQty
               ldExtCostTot / liQtyTot @ ldStdCost
               ldExtCostTot @ ldExtCost
            with frame det.
    END.

END.

ELSE /* prep code */
    for EACH ASI.job-prep WHERE job-prep.company eq cocode
                        and job-prep.job-no  ge substr(v-fjob,1,6)
                        and job-prep.job-no  le substr(v-tjob,1,6)
                        and fill(" ",6 - length(trim(job-PREP.job-no))) +
                      trim(job-prep.job-no) + string(job-prep.job-no2,"99")
                                  ge v-fjob
                  and fill(" ",6 - length(trim(job-prep.job-no))) +
                      trim(job-prep.job-no) + string(job-prep.job-no2,"99")
                                  le v-tjob
                        /*job-prep.blank-no = job-hdr.blank-no AND*/
                        AND job-prep.CODE GE begin_prep 
                        AND job-prep.CODE LE END_prep NO-LOCK,
        FIRST job-hdr
            where job-hdr.company eq cocode
              and job-hdr.cust-no ge v-fcust
              and job-hdr.cust-no le v-tcust
              and job-hdr.job-no  EQ job-prep.job-no
              and job-hdr.job-no2  EQ job-prep.job-no2
              AND job-hdr.due-date GE v-fdate
              AND job-hdr.due-date LE v-tdate
            /*use-index cust-idx*/ NO-LOCK
                        BREAK BY job-prep.code BY job-hdr.cust-no BY job-hdr.job-no:

    IF FIRST-OF(job-hdr.job-no) THEN
       ASSIGN liQty = 0
              ldExtCost = 0.

    IF FIRST-OF(job-prep.CODE) THEN
       ASSIGN liQtyCust = 0
              ldExtCostCust = 0.

    ASSIGN liQty = liQty + job-prep.qty
           ldExtCost = ldExtCost + job-prep.qty * job-prep.std-cost
           liQtyCust = liQtyCust + job-prep.qty
           ldExtCostCust = ldExtCostCust + job-prep.qty * job-prep.std-cost
           liQtyTot = liQtyTot + job-prep.qty
           ldExtCostTot = ldExtCostTot + job-prep.qty * job-prep.std-cost
           .


    IF LAST-OF(job-hdr.job-no) THEN DO:
        FIND cust OF job-hdr NO-LOCK NO-ERROR.
         cCustomerName = IF AVAIL cust THEN cust.NAME ELSE job-hdr.cust-no.

         FIND FIRST prep WHERE prep.company EQ cocode
                           AND prep.loc     EQ locode
                           AND prep.code    EQ job-prep.CODE NO-LOCK NO-ERROR.
         cPrepDscr = IF AVAIL prep THEN prep.dscr ELSE job-prep.CODE.

       display 
            job-prep.CODE FORM "x(15)"
            cPrepDscr
            job-hdr.cust-no
            cCustomerName
            job-hdr.due-date
            job-hdr.job-no + "-" + string(job-hdr.job-no2,"99") @ job-hdr.job-no FORM "x(10)"
            liQty
            ldExtCost / liQty @ ldStdCost
            ldExtCost FORM "->>,>>>,>>9.99"
            with frame detPrep STREAM-IO width 140 no-box no-labels down.

       IF tb_excel THEN
          EXPORT STREAM excel DELIMITER ","
            job-prep.CODE FORM "x(15)"
            cPrepDscr
            job-hdr.cust-no
            cCustomerName
            job-hdr.due-date
            job-hdr.job-no + "-" + string(job-hdr.job-no2,"99") FORM "x(10)"
            liQty
            ldExtCost / liQty 
            ldExtCost FORM "->>,>>>,>>9.99"
            SKIP.
    END.
    IF LAST-OF(job-prep.CODE) AND tb_total THEN DO:
       FIND FIRST prep WHERE prep.company EQ cocode
                           AND prep.loc     EQ locode
                           AND prep.code    EQ job-prep.CODE NO-LOCK NO-ERROR.
         cPrepDscr = IF AVAIL prep THEN prep.dscr ELSE job-prep.CODE.
       DOWN WITH FRAME DETPrep.
       UNDERLINE job-hdr.job-no
                 liQty ldStdCost ldExtCost
           WITH FRAME detPrep.
       DOWN WITH FRAME detPrep.
       display /*cPrepDscr*/
               "Total" @ job-hdr.job-no
            liQtyCust @ liQty
            ldExtCostCust / liQtyCust @ ldStdCost
            ldExtCostCust @ ldExtCost
            with frame detPrep.
       DOWN WITH FRAME detPrep.
       PUT SKIP(1).
    END.
    IF LAST(job-prep.CODE) AND tb_total THEN DO:
       DOWN WITH FRAME detPrep.
       UNDERLINE job-hdr.job-no
                 liQty ldStdCost ldExtCost
           WITH FRAME detPrep.
       DOWN WITH FRAME detPrep.
       display "Prep Total" @ job-hdr.job-no
               liQtyTot @ liQty
               ldExtCostTot / liQtyTot @ ldStdCost
               ldExtCostTot @ ldExtCost
            with frame detPrep .
    END.






END.  /* by prep code */



IF tb_excel THEN DO:
   OUTPUT STREAM excel CLOSE.
   /*
   IF tb_total THEN
      RUN create-excel-pivot (fi_file, OUTPUT fi_file) .*/
   IF tb_runExcel THEN
     OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE("").

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

