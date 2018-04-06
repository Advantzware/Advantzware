&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oerep\r-ontime.w

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
DEF VAR ls-fax-file AS CHAR NO-UNDO.

DEF TEMP-TABLE tt-date-reasons
    FIELD reason-code AS CHAR
    FIELD occurrences AS INT.

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
&Scoped-Define ENABLED-OBJECTS rsCompareDate tbPercentPerReason ~
tbPrintPromise begin_cust end_cust begin_i-no end_i-no begin_ord-date ~
end_ord-date begin_bol-date end_bol-date tb_pw tb_pmsf tb_ptr rd-dest ~
lv-ornt lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file ~
btn-ok btn-cancel RECT-7 RECT-8 
&Scoped-Define DISPLAYED-OBJECTS rsCompareDate tbPercentPerReason ~
tbPrintPromise begin_cust end_cust begin_i-no end_i-no begin_ord-date ~
end_ord-date begin_bol-date end_bol-date tb_pw tb_pmsf tb_ptr rd-dest ~
lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm tb_excel ~
tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendXLLine C-Win 
FUNCTION appendXLLine RETURNS CHARACTER
 ( ipc-append AS CHAR )  FORWARD.

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

DEFINE VARIABLE begin_bol-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning BOL Date" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Order Date" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_bol-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending BOL Date" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Order Date" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-ontime.csv" 
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

DEFINE VARIABLE rsCompareDate AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Promise Date", "Promise",
"Release Date", "Release"
     SIZE 41 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 10.95.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.81.

DEFINE VARIABLE tbPercentPerReason AS LOGICAL INITIAL no 
     LABEL "Print Percent Per Reason?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE tbPrintPromise AS LOGICAL INITIAL no 
     LABEL "Print Promise Date and Reason Code?" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_pmsf AS LOGICAL INITIAL no 
     LABEL "Print MSF ?" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE tb_ptr AS LOGICAL INITIAL no 
     LABEL "Print Trailer ?" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE tb_pw AS LOGICAL INITIAL no 
     LABEL "Print Weight ?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

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
     rsCompareDate AT ROW 10.24 COL 24 NO-LABEL WIDGET-ID 2
     tbPercentPerReason AT ROW 9.05 COL 54.2 WIDGET-ID 8
     tbPrintPromise AT ROW 9.05 COL 12 WIDGET-ID 6
     begin_cust AT ROW 2.81 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 2.81 COL 67 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     begin_i-no AT ROW 4 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_i-no AT ROW 4 COL 67 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_ord-date AT ROW 5.19 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Order Date"
     end_ord-date AT ROW 5.19 COL 67 COLON-ALIGNED HELP
          "Enter Ending Order Date"
     begin_bol-date AT ROW 6.38 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Order Date"
     end_bol-date AT ROW 6.38 COL 67 COLON-ALIGNED HELP
          "Enter Ending Order Date"
     tb_pw AT ROW 8 COL 12
     tb_pmsf AT ROW 8.1 COL 41
     tb_ptr AT ROW 8.1 COL 67
     rd-dest AT ROW 13.76 COL 6 NO-LABEL
     lv-ornt AT ROW 14 COL 30 NO-LABEL
     lines-per-page AT ROW 13.91 COL 83 COLON-ALIGNED
     lv-font-no AT ROW 15.95 COL 33 COLON-ALIGNED
     lv-font-name AT ROW 16.91 COL 27 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 18.1 COL 29
     tb_excel AT ROW 18.29 COL 71 RIGHT-ALIGNED
     tb_runExcel AT ROW 18.29 COL 93 RIGHT-ALIGNED
     fi_file AT ROW 19.67 COL 49 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 21.05 COL 24
     btn-cancel AT ROW 21.05 COL 59
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 12.19 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Use Date:" VIEW-AS TEXT
          SIZE 10 BY .95 AT ROW 10.24 COL 12 WIDGET-ID 10
     RECT-7 AT ROW 1 COL 1
     RECT-8 AT ROW 12.1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.24
         SIZE 94.4 BY 21.86.


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
         TITLE              = "On-Time Deliveries"
         HEIGHT             = 22.1
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
   FRAME-NAME Custom                                                    */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_bol-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ord-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_bol-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ord-date:PRIVATE-DATA IN FRAME FRAME-A     = 
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

ASSIGN 
       tb_pmsf:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_ptr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_pw:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* On-Time Deliveries */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* On-Time Deliveries */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol-date C-Win
ON LEAVE OF begin_bol-date IN FRAME FRAME-A /* Beginning BOL Date */
DO:
  assign {&self-name}.
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
ON LEAVE OF begin_ord-date IN FRAME FRAME-A /* Beginning Order Date */
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
                            &begin_cust=begin_cust
                            &END_cust= begin_cust
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = "Customer"
                             &begin_cust= begin_cust
                             &END_cust=begin_cust
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Customer"
                                  &begin_cust= begin_cust
                                  &END_cust=begin_cust
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.
       END. 
       WHEN 6  THEN RUN output-to-port.
  end case. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_bol-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_bol-date C-Win
ON LEAVE OF end_bol-date IN FRAME FRAME-A /* Ending BOL Date */
DO:
  assign {&self-name}.
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
ON LEAVE OF end_ord-date IN FRAME FRAME-A /* Ending Order Date */
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

/* security check need {methods/prgsecur.i} in definition section */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

  assign
   begin_ord-date = today
   begin_bol-date = today
   lv-ornt = "L".

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO begin_cust.
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
  DISPLAY rsCompareDate tbPercentPerReason tbPrintPromise begin_cust end_cust 
          begin_i-no end_i-no begin_ord-date end_ord-date begin_bol-date 
          end_bol-date tb_pw tb_pmsf tb_ptr rd-dest lv-ornt lines-per-page 
          lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE rsCompareDate tbPercentPerReason tbPrintPromise begin_cust end_cust 
         begin_i-no end_i-no begin_ord-date end_ord-date begin_bol-date 
         end_bol-date tb_pw tb_pmsf tb_ptr rd-dest lv-ornt lines-per-page 
         lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
         RECT-7 RECT-8 
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
/* ------------------------------------------------ oe/rep/ontime.p 08/01 JLF */
/* On-Time Deliveries                                                         */
/* -------------------------------------------------------------------------- */

{sys/form/r-top3.f}

def var v-cust  like oe-ord.cust-no  extent 2 init ["","zzzzzzzz"] NO-UNDO.
def var v-date  like oe-ord.ord-date format "99/99/9999"
                                     extent 2 init [today, 12/31/9999] NO-UNDO.

def var v-cust-no like cust.cust-no NO-UNDO.
def var v-name    like cust.NAME NO-UNDO.
def var v-del     as   int extent 2 NO-UNDO.
def var v-ont     like v-del NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.
def var v-msf as dec format  "->,>>>,>>9.9999" NO-UNDO.
DEF VAR v-sqft LIKE itemfg.t-sqft NO-UNDO.
DEF VAR v-compare-dt AS DATE NO-UNDO.
DEF VAR lcXLLine AS CHAR NO-UNDO.

format header
       skip(1)
       "Cust:"
       v-cust-no
       v-name
       skip(1)
    with frame r-top.
EMPTY TEMP-TABLE tt-date-reasons.
assign
 str-tit2 = c-win:title
 str-tit3 = "By Customer"
 {sys/inc/ctrtext.i str-tit2 56}
 {sys/inc/ctrtext.i str-tit3 80}

 v-cust[1] = begin_cust
 v-cust[2] = end_cust
 v-date[1] = begin_ord-date
 v-date[2] = end_ord-date.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).
   excelheader = "Cust#,Customer Name,Customer Part#,FG Item#,Order#,Order Date,Due Date,BOL Date,On-Time,Prom Date,Date Change Reason,MSF,Weight,Trailer#".
   PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

SESSION:SET-WAIT-STATE ("general").

    for each oe-ord
        where oe-ord.company  eq cocode
          and oe-ord.cust-no  ge v-cust[1]
          and oe-ord.cust-no  le v-cust[2]
          and oe-ord.ord-date ge v-date[1]
          and oe-ord.ord-date le v-date[2]
        use-index cust no-lock,

        each oe-ordl of oe-ord
        where oe-ordl.i-no ge begin_i-no
          and oe-ordl.i-no le end_i-no 
        no-lock,

        EACH oe-rel
        WHERE oe-rel.company EQ oe-ordl.company
          AND oe-rel.ord-no  EQ oe-ordl.ord-no
          AND oe-rel.i-no    EQ oe-ordl.i-no
          AND oe-rel.line    EQ oe-ordl.line
          AND oe-rel.link-no ne 0
        NO-LOCK,

        first oe-rell
        where oe-rell.company eq cocode
          and oe-rell.r-no    eq oe-rel.link-no
          and oe-rell.i-no    eq oe-rel.i-no
          and oe-rell.line    eq oe-rel.line
          and can-find(first oe-relh where oe-relh.r-no eq oe-rell.r-no)
        USE-INDEX r-no no-lock,

        each oe-boll
        where oe-boll.company  eq cocode
          and oe-boll.r-no     eq oe-rell.r-no
          and oe-boll.ord-no   eq oe-rell.ord-no
          and oe-boll.rel-no   eq oe-rell.rel-no
          and oe-boll.b-ord-no eq oe-rell.b-ord-no
          and oe-boll.i-no     eq oe-rell.i-no
          and oe-boll.line     eq oe-rell.line
        no-lock,

        first oe-bolh
        where oe-bolh.b-no     eq oe-boll.b-no
          AND oe-bolh.bol-date GE begin_bol-date
          AND oe-bolh.bol-date LE end_bol-date
        no-lock

        break by oe-ord.cust-no
              by oe-bolh.bol-date
              by oe-ord.ord-no
              by oe-ordl.i-no:

         {custom/statusMsg.i "'Processing Order # ' + string(oe-ord.ord-no)"} 

      FIND FIRST itemfg WHERE itemfg.company = oe-boll.company AND 
                              itemfg.i-no    = oe-boll.i-no NO-LOCK NO-ERROR.
      ASSIGN
         v-sqft = IF AVAIL itemfg THEN itemfg.t-sqft ELSE 0.
         v-msf = (oe-boll.qty * v-sqft )/ 1000. 

      if first-of(oe-ord.cust-no) then do:
        find first cust
            where cust.company eq cocode
              and cust.cust-no eq oe-ord.cust-no
            no-lock no-error.
        assign
         v-cust-no = oe-ord.cust-no
         v-name    = if avail cust then cust.name else "Not on File".

        if first(oe-ord.cust-no) THEN display "" with frame r-top.
        ELSE page.
      end.

      v-del[1] = v-del[1] + 1.
      v-compare-dt = (IF rsCompareDate EQ "Release" THEN oe-rel.rel-date
                                                    ELSE oe-ordl.prom-date).
      if oe-bolh.bol-date le v-compare-dt then do:
          v-ont[1] = v-ont[1] + 1.

      END.
      ELSE DO:
          FIND FIRST tt-date-reasons 
              WHERE tt-date-reasons.reason-code = oe-rel.spare-char-2
              NO-LOCK NO-ERROR.
          IF NOT AVAIL tt-date-reasons THEN DO:

              CREATE tt-date-reasons.
              tt-date-reasons.reason-code = oe-rel.spare-char-2.
          END.
          tt-date-reasons.occurrences = tt-date-reasons.occurrences + 1.
      END.
      IF NOT tbPrintPromise THEN
      display oe-ordl.part-no       column-label "Customer Part#"
              space(2)
              oe-ordl.i-no          column-label "FG Item#"
              space(2)
              oe-ord.ord-no         column-label "Order#"
              space(2)
              oe-ord.ord-date       column-label "Ord Date"
                                    FORMAT "99/99/99"
              space(2)
              oe-rel.rel-date       column-label "Due Date"
                                    FORMAT "99/99/99"
              space(2)
              oe-bolh.bol-date      column-label "BOL Date"
                                    FORMAT "99/99/99"
              space(2)
              oe-bolh.bol-date le  v-compare-dt format "Y/N"
                                    column-label "On-Time"  SPACE(2)
              oe-ordl.prom-date     COLUMN-LABEL "Prom Dt" WHEN tbPrintPromise
              oe-rel.spare-char-2   COLUMN-LABEL "Reason" WHEN tbPrintPromise
             v-msf                  COLUMN-LABEL  "MSF" WHEN tb_pmsf SPACE(2)
             oe-boll.weight         COLUMN-LABEL  "WT"  WHEN tb_pw   SPACE(2)
             oe-bolh.trailer        COLUMN-LABEL  "Trailer#"  WHEN tb_ptr

            with down no-box stream-io width 200 no-attr-space.
      ELSE DO:
        display oe-ordl.part-no       column-label "Customer Part#"
              space(2)
              oe-ordl.i-no          column-label "FG Item#"
              space(2)
              oe-ord.ord-no         column-label "Order#"
              space(2)
              oe-ord.ord-date       column-label "Ord Date"
                                    FORMAT "99/99/99"
              space(2)
              oe-rel.rel-date       column-label "Due Date"
                                    FORMAT "99/99/99"
              space(2)
              oe-bolh.bol-date      column-label "BOL Date"
                                    FORMAT "99/99/99"
              space(2)
              oe-bolh.bol-date le v-compare-dt format "Y/N"
                                    column-label "On-Time"  SPACE(2)
              oe-ordl.prom-date     COLUMN-LABEL "Prom Dt" WHEN tbPrintPromise
              oe-rel.spare-char-2   COLUMN-LABEL "Reason" WHEN tbPrintPromise
             v-msf                  COLUMN-LABEL  "MSF" WHEN tb_pmsf SPACE(2)
             oe-boll.weight         COLUMN-LABEL  "WT"  WHEN tb_pw   SPACE(2)
             oe-bolh.trailer        COLUMN-LABEL  "Trailer#"  WHEN tb_ptr

            with FRAME f-promise down no-box stream-io width 200 no-attr-space.
         DOWN WITH FRAME f-promise.
      END.
      IF tb_excel THEN 
        PUT STREAM excel UNFORMATTED
            appendXLLine(v-cust-no)
            appendXLLine(v-name)
            appendXLLine(oe-ordl.part-no)
            appendXLLine(oe-ordl.i-no)
/*             '"' v-cust-no                                              '",' */
/*             '"' v-name                                                 '",' */
/*             '"' oe-ordl.part-no                                        '",' */
/*             '"' oe-ordl.i-no                                           '",' */
            '"' oe-ord.ord-no                                          '",'
            '"' (IF oe-ord.ord-date <> ? THEN STRING(oe-ord.ord-date)
                 ELSE "")                                              '",'
            '"' (IF oe-rel.rel-date <> ? THEN STRING(oe-rel.rel-date)
                 ELSE "")                                              '",'
            '"' (IF oe-bolh.bol-date <> ? THEN STRING(oe-bolh.bol-date)
                 ELSE "")                                              '",'
            '"'  STRING(oe-bolh.bol-date le v-compare-dt,"Y/N")     '",'
            '"'  ( IF tbPrintPromise THEN oe-ordl.prom-date ELSE ? )   '",'
            '"'  ( IF tbPrintPromise THEN oe-rel.spare-char-2 ELSE "" ) '",'
            '"' ( IF tb_pmsf THEN v-msf ELSE 0 )                        '",'
            '"' (IF tb_pw THEN  oe-boll.weight ELSE 0 )                 '",'
            '"' (IF tb_ptr THEN oe-bolh.trailer ELSE  "" )              '",'
            SKIP.

      if last-of(oe-ord.cust-no) then do:
        put skip(1)
            "Customer Totals:"          at 5
            space(5)
            "Deliveries: " + trim(string(v-del[1],">,>>>,>>9"))
                                        format "x(21)"
            space(3)
            "On-Time: "    + trim(string(v-ont[1],">,>>>,>>9"))
                                        format "x(18)"
            v-ont[1] / v-del[1] * 100   format ">>9.99%"
            skip(1).

        assign
         v-del[2] = v-del[2] + v-del[1]
         v-ont[2] = v-ont[2] + v-ont[1]
         v-del[1] = 0
         v-ont[1] = 0.
      end.

      if last(oe-ord.cust-no) then do:
        assign
         v-cust-no = ""
         v-name    = "".

/*         page. */

        put skip(3)
            "   Grand Totals:"          at 5
            space(5)
            "Deliveries: " + trim(string(v-del[2],">,>>>,>>9"))
                                        format "x(21)"
            space(3)
            "On-Time: "    + trim(string(v-ont[2],">,>>>,>>9"))
                                        format "x(18)"
            v-ont[2] / v-del[2] * 100   format ">>9.99%"
            skip(1).
        FIND FIRST tt-date-reasons NO-ERROR.
        IF tbPercentPerReason AND avail(tt-date-reasons) AND (v-del[2] - v-ont[2]) GT 0 THEN DO:
            PUT UNFORMATTED "Date Change Reason Summary: " SKIP(1).
            FOR EACH tt-date-reasons:
                FIND FIRST rejct-cd 
                    WHERE rejct-cd.CODE = tt-date-reasons.reason-code
                    NO-LOCK NO-ERROR.

                IF v-del[2] - v-ont[2] GT 0 THEN
                DISP tt-date-reasons.reason-code FORMAT "x(30)" COLUMN-LABEL "Reason"
                     rejct-cd.dscr WHEN AVAIL rejct-cd
                     tt-date-reasons.occurrences / (v-del[2] - v-ont[2]) * 100 WHEN v-del[2] - v-ont[2] GT 0
                        FORMAT ">>9.99%" COLUMN-LABEL "%"
                    WITH FRAME f-occurrences WIDTH 132 STREAM-IO.
            END.
        END.
      end.
    end. /* each oe-ord */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendXLLine C-Win 
FUNCTION appendXLLine RETURNS CHARACTER
 ( ipc-append AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Adds a value to a csv line
    Notes:  Protects agains commans and quotes.
------------------------------------------------------------------------------*/
    DEF VAR lc-line AS CHAR NO-UNDO.

    ipc-append = REPLACE(ipc-append, '"', '').
    ipc-append = REPLACE(ipc-append, ',', ' ').
    lc-line = lc-line + '"' + ipc-append + '",'.
    RETURN lc-line.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

