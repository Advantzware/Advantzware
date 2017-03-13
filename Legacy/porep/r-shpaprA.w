&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: porep\r-shpapr.w

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

def temp-table tt-report NO-UNDO like report
    FIELD mach-vend LIKE vend.vend-no.

def TEMP-TABLE wk-sh-ord NO-UNDO
    field due-date like po-ordl.due-date
    field rec-id as recid.

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEF STREAM excel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_vend end_vend ~
begin_due-date end_due-date tb_cust tb_fg-itm tb_mach-opr tb_rel-date ~
rd_break rd_sort rd-dest lv-ornt lines-per-page lv-font-no td-show-parm ~
tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_vend end_vend begin_due-date ~
end_due-date tb_cust tb_fg-itm tb_mach-opr tb_rel-date lbl_break rd_break ~
lbl_sort rd_sort rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
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

DEFINE VARIABLE begin_due-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Due Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_vend AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_due-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Due Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-shpapr.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_break AS CHARACTER FORMAT "X(256)":U INITIAL "Page Break?" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Sort?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

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
     SIZE 19 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd_break AS CHARACTER INITIAL "Vendor#" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Vendor#", "Vendor#",
"None", "None"
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Job#" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Job#", "Job#",
"Customer#", "Customer#",
"Machine", "Machine"
     SIZE 43 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 9.05.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 10.48.

DEFINE VARIABLE tb_cust AS LOGICAL INITIAL yes 
     LABEL "Print Customer Name?" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_fg-itm AS LOGICAL INITIAL yes 
     LABEL "Print Finished Goods Item?" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE tb_mach-opr AS LOGICAL INITIAL yes 
     LABEL "Print First Machine Operations?" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE tb_rel-date AS LOGICAL INITIAL yes 
     LABEL "Print Release Date?" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY 1 NO-UNDO.

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
     begin_vend AT ROW 2.43 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Vendor Number"
     end_vend AT ROW 2.43 COL 69 COLON-ALIGNED HELP
          "Enter Ending Vendor Number"
     begin_due-date AT ROW 3.38 COL 26 COLON-ALIGNED
     end_due-date AT ROW 3.38 COL 69 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     tb_cust AT ROW 4.57 COL 36
     tb_fg-itm AT ROW 5.52 COL 68 RIGHT-ALIGNED
     tb_mach-opr AT ROW 6.48 COL 69 RIGHT-ALIGNED
     tb_rel-date AT ROW 7.43 COL 68 RIGHT-ALIGNED
     lbl_break AT ROW 8.62 COL 26 COLON-ALIGNED NO-LABEL
     rd_break AT ROW 8.62 COL 43 NO-LABEL
     lbl_sort AT ROW 9.81 COL 33 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 9.81 COL 43 NO-LABEL
     rd-dest AT ROW 12.67 COL 6 NO-LABEL
     lv-ornt AT ROW 13.14 COL 31 NO-LABEL
     lines-per-page AT ROW 13.14 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 14.81 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 15.76 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 16.91 COL 31
     tb_excel AT ROW 18.29 COL 51 RIGHT-ALIGNED
     tb_runExcel AT ROW 18.29 COL 72 RIGHT-ALIGNED
     fi_file AT ROW 19.1 COL 29 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 20.67 COL 19
     btn-cancel AT ROW 20.67 COL 57
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11.71 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 11.48 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 94.4 BY 21.57.


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
         TITLE              = "Shipment Approval Report"
         HEIGHT             = 21.81
         WIDTH              = 95.4
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
       begin_due-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_due-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_break IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_break:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_break".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_break:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_fg-itm IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_fg-itm:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_mach-opr IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_mach-opr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_rel-date IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_rel-date:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Shipment Approval Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Shipment Approval Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_due-date C-Win
ON LEAVE OF begin_due-date IN FRAME FRAME-A /* Beginning Due Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend C-Win
ON LEAVE OF begin_vend IN FRAME FRAME-A /* Beginning Vendor# */
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
    ASSIGN {&displayed-objects}.
  END.

  RUN run-report. 
  STATUS DEFAULT "Processing Complete".

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_vend
                            &END_cust=END_vend
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = "Vendor"
                             &begin_cust= begin_vend
                             &END_cust=end_vend
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Vendor"
                                  &begin_cust= begin_vend
                                  &END_cust=end_vend
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.

       END. 
       WHEN 6 THEN run output-to-port.
  end case.
  SESSION:SET-WAIT-STATE (""). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_due-date C-Win
ON LEAVE OF end_due-date IN FRAME FRAME-A /* Ending Due Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend C-Win
ON LEAVE OF end_vend IN FRAME FRAME-A /* Ending Vendor# */
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


&Scoped-define SELF-NAME rd_break
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_break C-Win
ON VALUE-CHANGED OF rd_break IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sort C-Win
ON VALUE-CHANGED OF rd_sort IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cust C-Win
ON VALUE-CHANGED OF tb_cust IN FRAME FRAME-A /* Print Customer Name? */
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


&Scoped-define SELF-NAME tb_fg-itm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fg-itm C-Win
ON VALUE-CHANGED OF tb_fg-itm IN FRAME FRAME-A /* Print Finished Goods Item? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_mach-opr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_mach-opr C-Win
ON VALUE-CHANGED OF tb_mach-opr IN FRAME FRAME-A /* Print First Machine Operations? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_rel-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_rel-date C-Win
ON VALUE-CHANGED OF tb_rel-date IN FRAME FRAME-A /* Print Release Date? */
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

  assign
   begin_due-date = date(1,1,year(today))
   end_due-date   = today.

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO begin_vend.
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
  DISPLAY begin_vend end_vend begin_due-date end_due-date tb_cust tb_fg-itm 
          tb_mach-opr tb_rel-date lbl_break rd_break lbl_sort rd_sort rd-dest 
          lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_vend end_vend begin_due-date end_due-date tb_cust 
         tb_fg-itm tb_mach-opr tb_rel-date rd_break rd_sort rd-dest lv-ornt 
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
{custom/out2file.i}.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-grand-total C-Win 
PROCEDURE print-grand-total :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT PARAM v-char-ord-qty AS CHAR NO-UNDO.
  DEF INPUT PARAM v-msf-rem AS DEC NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:

    PUT STREAM excel UNFORMATTED
        '"' "Grand Totals"                      '",'.

    IF rd_break EQ "Vendor#" THEN
       PUT STREAM excel UNFORMATTED
           '"' "" '",'.

    PUT STREAM excel UNFORMATTED
      '"' ""                                    '",'
      '"' v-char-ord-qty                        '",'
      '"' ""                                    '",'
      '"' ""                                    '",'
      '"' ""                                    '",'
      '"' ""                                    '",'
      '"' ""                                    '",'
      '"' ""                                    '",'
      '"' ""                                    '",'
      '"' ""                                    '",'
      '"' ""                                    '",'
      '"' STRING(v-msf-rem,"->,>>9.99")         '",'
      '"' ""                                    '",'
      SKIP.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------ po/rep/sh-apr.p 6/00 djk  */
/* Sheets On Order Report                                                     */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}

def var v-msf-rem as dec format "->,>>9.99" no-undo.
def var v-cst-rem as dec format ">>>,>>9" no-undo.
def var v-s-vend like vend.vend-no init "" no-undo.
def var v-e-vend like vend.vend-no init "zzzzzzzz" no-undo.
def var v-s-date like po-ord.po-date format "99/99/9999" no-undo init 01/01/2000.
def var v-e-date like po-ord.po-date format "99/99/9999" init today no-undo.
def var v-name as log no-undo init yes.
def var v-cust-name like oe-ord.cust-name.
def var v-wid like po-ordl.s-wid.
def var v-len like po-ordl.s-len.
def var v-dep like item.s-dep.
def var v-fgitem as logical init yes.
def var v-pmach  as logical init yes.
def var v-preld as logical init yes.
def var v-sortby as logical format "J/C" init yes.
def var v-bwt like item.basis-w.
def var v-raw like po-ordl.i-no.
def var v-fg like po-ordl.i-no.

def var v-cust-vend as char format "x(26)" init "--------- VENDOR ---------".

def var v-job-no as char format "x(9)".

def var tot-cons-qty like po-ordl.cons-qty no-undo.
def var tot-rec-qty as dec no-undo.

def var tot-msf-rem as dec format "->>>,>>>,>>>,>>>,>>9.99" EXTENT 2 no-undo.
def var tot-qty-ord as dec format "->>>,>>>,>>>,>>>,>>9.99" EXTENT 2 no-undo.

def var tot-msf-rem-str as char format "x(60)" .

def var v-trcv as char  format "x(7)".    /* format "->>,>>9.9" */ 
def var v-char-ord-qty as char format "x(8)".
def var v-rel-date like oe-rel.rel-date.
def var v-mach like mach.m-code.
def var v-vend like po-ord.vend-no.
def var v-stat as char.
DEF VAR lv-label AS CHAR EXTENT 2 NO-UNDO. 
DEF VAR ll-sub AS LOG NO-UNDO.
DEF VAR lv-under AS CHAR FORMAT "x(5)" INIT "_____" EXTENT 2 NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.

form header
     lv-label[1]                            FORMAT "x(20)" 
     skip(1)
     "Job#"                     at 1
     "PO#"                      to 16
     "Qty Ord"                  to 25
     "Receivd"                  to 33
     "Customer Name"            at 35
     "   Due  "                 at 56
     "Units"                    at 65
     "Sht W"                    to 77
     "Sht L"                    to 84
     lv-label[2]                at 86       FORMAT "x(8)"
     "FGItem#"                  at 95
     "RMItem#"                  at 110
     "MSF Rem"                  to 128
     "Ship?"                    at 130
     skip
     fill("-",134) format "x(134)"

    with frame r-top.

form v-job-no                   at 1     
     po-ordl.po-no              to 16
     v-char-ord-qty             to 25
     v-trcv                     to 33
     v-cust-name                at 35  format "x(20)"
     tt-report.key-07           at 56
     lv-under[1]                at 65
     po-ordl.s-wid              to 77  format "->>9.99"
     po-ordl.s-len              to 84  format "->>9.99"
     tt-report.mach-vend        at 86
     v-fg                       at 95  format "x(15)"       
     v-raw                      at 110 format "x(10)"
     v-msf-rem                  to 128
     lv-under[2]                at 130

    with down STREAM-IO WIDTH 200 no-labels no-box no-underline frame sh-ord.


SESSION:SET-WAIT-STATE ("general").

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 v-s-vend     = begin_vend
 v-e-vend     = end_vend
 v-s-date     = begin_due-date
 v-e-date     = end_due-date
 v-name       = tb_cust
 v-fgitem     = tb_fg-itm
 v-pmach      = tb_mach-opr OR rd_sort EQ "Machine"
 v-preld      = tb_rel-date
 v-sortby     = rd_sort EQ "Job#".

assign tot-cons-qty = 0
       tot-rec-qty  = 0
       tot-msf-rem  = 0
       lv-label[2]  = IF v-pmach OR rd_break NE "None" THEN "Machine"
                      ELSE "Vendor#".

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).

  IF rd_break EQ "Vendor#" THEN
     excelheader = "Vendor,".

  excelheader = excelheader    
              + "Job#,PO#,Qty Ord,Receivd,Customer Name,"
              + "Due,Units,Sht W,Sht L," + lv-label[2] + ",FGItem#,RMItem#,"
              + "MSF Rem,Ship?".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

if td-show-parm then run show-param.

VIEW frame r-top.

  EMPTY TEMP-TABLE tt-report.

  for each po-ord
      where po-ord.company eq cocode
        and index("CXF",po-ord.stat) eq 0
        and po-ord.vend-no ge v-s-vend
        and po-ord.vend-no le v-e-vend
      no-lock:

    for each po-ordl WHERE
        po-ordl.company EQ po-ord.company AND
        po-ordl.po-no   EQ po-ord.po-no AND
        po-ordl.item-type
          and index("CXF",po-ordl.stat) eq 0
          and po-ordl.due-date ge v-s-date
          and po-ordl.due-date le v-e-date
        no-lock:

        {custom/statusMsg.i " 'Processing PO#  '  + string(po-ordl.po-no) "}

      release job-hdr.
      release oe-ord.
      release oe-ordl.

      assign
       v-job-no    = trim(po-ordl.job-no) + "-" + string(po-ordl.job-no2,"99")
       v-raw       = ""
       v-fg        = ""
       v-cust-name = "".

      if trim(v-job-no) eq "-00" then v-job-no = "".

      if po-ordl.item-type then do:
        v-raw = po-ordl.i-no.

        find first job-hdr
            where job-hdr.company eq cocode
              and job-hdr.job-no  eq po-ordl.job-no
              and job-hdr.job-no2 eq po-ordl.job-no2
            no-lock no-error.

        if avail job-hdr then do:
          v-fg = job-hdr.i-no.

          find first oe-ordl
              where oe-ordl.company eq cocode
                and oe-ordl.ord-no  eq job-hdr.ord-no
                and oe-ordl.i-no    eq v-fg
                and oe-ordl.job-no  eq po-ordl.job-no
                and oe-ordl.job-no2 eq po-ordl.job-no2
              no-lock no-error.
        end.
      end.

      else do:
        v-fg = po-ordl.i-no.

        if po-ordl.ord-no ne 0 then
        find first oe-ordl
            where oe-ordl.company eq cocode
              and oe-ordl.ord-no  eq po-ordl.ord-no
              and oe-ordl.i-no    eq v-fg
            no-lock no-error.
      end.

      v-rel-date = ?.

      if avail oe-ordl then do:
        find first oe-ord
            where oe-ord.company eq cocode
              and oe-ord.ord-no  eq oe-ordl.ord-no
            no-lock no-error.
        if avail oe-ord then v-cust-name = oe-ord.cust-name.

        FOR EACH oe-rell
            WHERE oe-rell.company  EQ cocode
              AND oe-rell.ord-no   EQ oe-ordl.ord-no
              AND oe-rell.i-no     EQ oe-ordl.i-no
              AND oe-rell.line     EQ oe-ordl.line
            NO-LOCK,
            FIRST oe-relh
            WHERE oe-relh.r-no    EQ oe-rell.r-no
              AND oe-relh.deleted EQ NO
            NO-LOCK
            BY oe-relh.rel-date:

          LEAVE.
        END.

        if avail oe-relh then v-rel-date = oe-relh.rel-date.

        else
        for each oe-rel
            where oe-rel.company eq cocode
              and oe-rel.ord-no  eq oe-ordl.ord-no
              and oe-rel.i-no    eq oe-ordl.i-no
              and oe-rel.line    eq oe-ordl.line
            no-lock:

          {oe/rel-stat.i v-stat}

          if index("ILS",v-stat) ne 0                           and
             (oe-rel.rel-date lt v-rel-date or v-rel-date eq ?) then
            v-rel-date = oe-rel.rel-date.
        end.
      end.

      v-mach = "".

      for first job
          where job.company eq po-ordl.company
            and job.job-no  eq po-ordl.job-no
            and job.job-no2 eq po-ordl.job-no2
          no-lock,

          first job-mch
          where job-mch.company eq job.company
            and job-mch.job     eq job.job
            and job-mch.frm     eq po-ordl.s-num
            and (job-mch.dept   ne "DM" and
                 job-mch.dept   ne "PM")
          use-index line-idx no-lock:

        v-mach = job-mch.m-code.
      end.

      create tt-report.
      assign
       tt-report.key-01    = IF rd_break EQ "Vendor#" THEN po-ord.vend-no
                             ELSE
                             IF rd_sort EQ "Machine" THEN v-mach ELSE ""
       tt-report.key-02    = IF rd_sort EQ "Job#"      THEN v-job-no
                             ELSE
                             IF rd_sort EQ "Customer#" THEN v-cust-name
                             ELSE
                             IF rd_break EQ "Vendor#" THEN v-mach ELSE ""
       tt-report.key-03    = v-job-no
       tt-report.key-04    = v-fg
       tt-report.key-05    = v-raw
       tt-report.key-06    = v-cust-name
       tt-report.key-07    = if v-rel-date eq ? then ""
                                             else string(v-rel-date,"99/99/99")
       tt-report.rec-id    = recid(po-ordl)
       tt-report.mach-vend = IF v-pmach THEN v-mach
                             ELSE
                             IF rd_break EQ "None" THEN po-ord.vend-no ELSE "".
    end.
  end.

  if v-name then v-cust-vend = "-------- CUSTOMER --------".

  view frame r-top.

  for each tt-report,
      each po-ordl where recid(po-ordl) eq tt-report.rec-id no-lock,
      FIRST po-ord WHERE
            po-ord.company EQ po-ordl.company AND
            po-ord.po-no   EQ po-ordl.po-no no-lock
      break by tt-report.key-01
            by tt-report.key-02:

      {custom/statusMsg.i " 'Processing PO#  '  + string(po-ordl.po-no) "}

    if rd_break EQ "Vendor#" AND first-of(tt-report.key-01) then do:
      lv-label[1] = TRIM(rd_break) + ": " + tt-report.key-01.
      page.
    end.

    release item.
    release itemfg.

    if po-ordl.item-type then do:
      find first item
          where item.company eq cocode
            and item.i-no    eq po-ordl.i-no
          no-lock no-error.
    end.

    else do:
      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq po-ordl.i-no
          no-lock no-error.
    end.

    assign
     v-len = po-ordl.s-len
     v-wid = po-ordl.s-wid
     v-dep = if avail item then item.s-dep else 0
     v-bwt = 0.

    if (v-len eq 0 or v-wid eq 0 or v-bwt eq 0) then do:
      find first job
          where job.company eq cocode
            and job.job-no  eq po-ordl.job-no
            and job.job-no2 eq po-ordl.job-no2
          no-lock no-error.

      if avail job then do:
        find first job-mat
            where job-mat.company eq cocode
              and job-mat.job     eq job.job
              and job-mat.i-no    eq po-ordl.i-no
            no-lock no-error.

        if avail job-mat then
          assign
           v-len = if v-len eq 0 then job-mat.len     else v-len
           v-wid = if v-wid eq 0 then job-mat.wid     else v-wid
           v-bwt = if v-bwt eq 0 then job-mat.basis-w else v-bwt.
      end.

      if avail item then do:
        if po-ordl.item-type = yes then 
          if v-len eq 0 then v-len = item.s-len.

        if po-ordl.item-type = yes then 
          if v-wid eq 0 then v-wid = if item.r-wid ne 0 then item.r-wid
                                                        else item.s-wid.

        if po-ordl.item-type = yes then 
          if v-bwt eq 0 then v-bwt = item.basis-w.
      end.
    end.

    if po-ordl.cons-uom eq "MSF" then
      v-msf-rem = po-ordl.cons-qty - po-ordl.t-rec-qty.

    else do:
      run sys/ref/convquom.p(po-ordl.cons-uom, "MSF",
                             v-bwt, v-len, v-wid, v-dep,
                             (po-ordl.cons-qty - po-ordl.t-rec-qty),
                             output v-msf-rem).
    end.

    run sys/ref/convcuom.p(po-ordl.cons-uom, "MSF",
                           v-bwt, v-len, v-wid, v-dep,
                           po-ordl.cons-cost, output v-cst-rem).

    assign
     tot-cons-qty   = tot-cons-qty + po-ordl.cons-qty
     tot-rec-qty    = tot-rec-qty + t-rec-qty
     tot-qty-ord[1] = tot-qty-ord[1] + po-ordl.ord-qty
     tot-msf-rem[1] = tot-msf-rem[1] + v-msf-rem.

    if v-msf-rem ge 0 or v-cst-rem ge 0 then do:
      assign
       v-job-no    = tt-report.key-03
       v-fg        = tt-report.key-04
       v-raw       = tt-report.key-05
       v-cust-name = tt-report.key-06.

      if (v-cust-name eq "") or (v-name eq no) then v-cust-name = fill("_",20).

      if po-ordl.ord-qty - trunc(po-ordl.ord-qty,0) ne 0 and
         po-ordl.ord-qty lt 100000                       then
        v-char-ord-qty = string(po-ordl.ord-qty,">>,>>9.9<<<").
      else
        v-char-ord-qty = string(po-ordl.ord-qty,">>>>,>>9").

      if po-ordl.t-rec-qty ne 0 then
        if po-ordl.t-rec-qty - trunc(po-ordl.t-rec-qty,0) ne 0 and
           po-ordl.t-rec-qty lt 10000                          then
          v-trcv = string(po-ordl.t-rec-qty,">,>>9.9<<<").
        else
          v-trcv = string(po-ordl.t-rec-qty,">>>,>>9").
      else
        v-trcv = fill("_",7).

      display v-job-no                              
              po-ordl.po-no
              v-char-ord-qty                      
              v-trcv
              v-cust-name                         
              tt-report.key-07
                "________" when not v-preld @ tt-report.key-07
              lv-under[1]
              po-ordl.s-wid                       
              po-ordl.s-len
              tt-report.mach-vend
                "______" when not v-pmach AND rd_break NE "None" @ tt-report.mach-vend
              v-fg when v-fgitem 
              v-raw
              v-msf-rem
              lv-under[2]
          with frame sh-ord.
      down with frame sh-ord.

      IF tb_excel THEN DO:

        IF rd_break EQ "Vendor#" THEN 
           PUT STREAM excel UNFORMATTED
               '"' (IF first-of(tt-report.key-01) THEN tt-report.key-01
                    ELSE "") '",'.

        PUT STREAM excel UNFORMATTED
          '"' v-job-no                           '",'
          '"' po-ordl.po-no                      '",'
          '"' v-char-ord-qty                      '",'
          '"' v-trcv                            '",'
          '"' v-cust-name                       '",'
          '"' (IF v-preld THEN tt-report.key-07
               ELSE "________")                 '",'
          '"' lv-under[1]                       '",'
          '"' STRING(po-ordl.s-wid,">>9.99")    '",'
          '"' STRING(po-ordl.s-len,">>9.99")    '",'
          '"' (IF NOT v-pmach AND rd_break NE "None" THEN "______"
               ELSE tt-report.mach-vend)            '",'
          '"' (IF v-fgitem THEN v-fg ELSE "")       '",'
          '"' v-raw                                 '",'
          '"' STRING(v-msf-rem,"->,>>9.99")         '",'
          '"' lv-under[2]                           '",'
          SKIP.
      END.

      ll-sub = YES.
    end.

    IF LAST-OF(tt-report.key-01) THEN DO:
      IF (rd_break EQ "Vendor#" OR rd_sort EQ "Machine") AND ll-sub THEN DO:
        IF tot-qty-ord[1] - TRUNC(tot-qty-ord[1],0) NE 0 AND
           tot-qty-ord[1] LT 100000                       THEN
          v-char-ord-qty = STRING(tot-qty-ord[1],">>,>>9.9<<<").
        ELSE
          v-char-ord-qty = STRING(tot-qty-ord[1],">>>>,>>9").

        v-msf-rem = tot-msf-rem[1].

        UNDERLINE v-char-ord-qty                      
                  v-msf-rem                          
            WITH FRAME sh-ord.

        DISPLAY FILL(" ",9 - LENGTH(TRIM(rd_break))) +
                TRIM(IF rd_break EQ "Vendor#" THEN rd_break ELSE "Machine")
                               @ v-job-no
                "Totals"       @ po-ordl.po-no
                v-char-ord-qty                      
                v-msf-rem                            
            WITH FRAME sh-ord.
        DOWN WITH FRAME sh-ord.

        IF rd_break EQ "None" THEN PUT SKIP(2).

        IF tb_excel THEN
        DO:
           PUT STREAM excel UNFORMATTED
             SKIP(1)
             '"' (IF rd_break EQ "Vendor#" THEN "Vendor# Totals"
                  ELSE "Machine Totals")               '",'.

            IF rd_break EQ "Vendor#" THEN
               PUT STREAM excel UNFORMATTED
                  '"' "" '",'.

            PUT STREAM excel UNFORMATTED
             '"' ""                                    '",'
             '"' v-char-ord-qty                        '",'
             '"' ""                                    '",'
             '"' ""                                    '",'
             '"' ""                                    '",'
             '"' ""                                    '",'
             '"' ""                                    '",'
             '"' ""                                    '",'
             '"' ""                                    '",'
             '"' ""                                    '",'
             '"' ""                                    '",'
             '"' STRING(v-msf-rem,"->,>>9.99")         '",'
             '"' ""                                    '",'
             SKIP(1).
        END.
      END.

      ASSIGN
       tot-msf-rem[2] = tot-msf-rem[2] + tot-msf-rem[1]
       tot-qty-ord[2] = tot-qty-ord[2] + tot-qty-ord[1]

       tot-qty-ord[1] = 0
       tot-msf-rem[1] = 0
       ll-sub         = NO.
    END.

    IF LAST(tt-report.key-01) THEN DO:
      IF tot-qty-ord[2] - TRUNC(tot-qty-ord[2],0) NE 0 AND
         tot-qty-ord[2] LT 100000                       THEN
        v-char-ord-qty = STRING(tot-qty-ord[2],">>,>>9.9<<<").
      ELSE
        v-char-ord-qty = STRING(tot-qty-ord[2],">>>>,>>9").

      v-msf-rem = tot-msf-rem[2].

      UNDERLINE v-char-ord-qty                      
                v-msf-rem                          
          WITH FRAME sh-ord.

      UNDERLINE v-char-ord-qty                      
                v-msf-rem                          
          WITH FRAME sh-ord.

      DISPLAY "    Grand" @ v-job-no
              "Totals"    @ po-ordl.po-no
              v-char-ord-qty                      
              v-msf-rem                            
          WITH FRAME sh-ord.
      DOWN WITH FRAME sh-ord.

      IF tb_excel THEN
         RUN print-grand-total(v-char-ord-qty,
                               v-msf-rem).
    END.

    DELETE tt-report.
  END.

IF tb_excel THEN DO:
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

