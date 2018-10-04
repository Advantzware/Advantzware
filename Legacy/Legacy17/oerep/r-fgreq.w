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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_ord-no end_ord-no begin_cust-no ~
end_cust-no begin_i-no end_i-no begin_ord-date end_ord-date begin_prm-date ~
end_prm-date tb_re-prnt rd-dest lv-ornt lines-per-page lv-font-no ~
td-show-parm btn-ok btn-cancel RECT-6 RECT-7 
&Scoped-Define DISPLAYED-OBJECTS begin_ord-no end_ord-no begin_cust-no ~
end_cust-no begin_i-no end_i-no begin_ord-date end_ord-date begin_prm-date ~
end_prm-date tb_re-prnt rd-dest lv-ornt lines-per-page lv-font-no ~
lv-font-name td-show-parm 

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

DEFINE VARIABLE begin_prm-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/01 
     LABEL "From Promise Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

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

DEFINE VARIABLE end_prm-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "To Promise Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

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

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 92 BY 9.05.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 9.05.

DEFINE VARIABLE tb_re-prnt AS LOGICAL INITIAL no 
     LABEL "Reprint?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY 1.05 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
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
     begin_ord-date AT ROW 5.76 COL 28 COLON-ALIGNED
     end_ord-date AT ROW 5.76 COL 69 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     begin_prm-date AT ROW 6.71 COL 28 COLON-ALIGNED
     end_prm-date AT ROW 6.71 COL 69 COLON-ALIGNED HELP
          "Enter Ending Promise Date"
     tb_re-prnt AT ROW 8.62 COL 48
     rd-dest AT ROW 12.19 COL 7 NO-LABEL
     lv-ornt AT ROW 12.43 COL 31 NO-LABEL
     lines-per-page AT ROW 12.43 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 14.33 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 15.29 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 17.67 COL 31
     btn-ok AT ROW 19.81 COL 19
     btn-cancel AT ROW 19.81 COL 57
     RECT-6 AT ROW 10.52 COL 2
     RECT-7 AT ROW 1 COL 1
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11.48 COL 4
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
         TITLE              = "FG Purchase Requirements"
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
       begin_prm-date:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       end_prm-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_re-prnt:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* FG Purchase Requirements */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* FG Purchase Requirements */
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


&Scoped-define SELF-NAME begin_prm-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_prm-date C-Win
ON LEAVE OF begin_prm-date IN FRAME FRAME-A /* From Promise Date */
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
SESSION:SET-WAIT-STATE ("general").

  assign rd-dest.

  run run-report. 

  SESSION:SET-WAIT-STATE ("").

  case rd-dest:

when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type= "Customer"
                            &begin_cust=begin_cust-no
                            &END_cust= begin_cust-no
                            &fax-subject="FG Purchase Requirements"
                            &fax-body="FG Purchase Requirements"
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = "Customer"
                             &begin_cust= begin_cust-no
                             &END_cust=begin_cust-no
                             &mail-subject="FG Purchase Requirements"
                             &mail-body="FG Purchase Requirements"
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Customer"
                                  &begin_cust= begin_cust-no
                                  &END_cust=begin_cust-no
                                  &mail-subject="FG Purchase Requirements"
                                  &mail-body="FG Purchase Requirements"
                                  &mail-file=list-name }
       END.
       END.
       WHEN 6 THEN RUN OUTPUT-to-port.

  end case. 
  SESSION:SET-WAIT-STATE("").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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


&Scoped-define SELF-NAME end_prm-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_prm-date C-Win
ON LEAVE OF end_prm-date IN FRAME FRAME-A /* To Promise Date */
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


&Scoped-define SELF-NAME tb_re-prnt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_re-prnt C-Win
ON VALUE-CHANGED OF tb_re-prnt IN FRAME FRAME-A /* Reprint? */
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

  assign
   begin_ord-date = date(1,1,year(today))
   end_ord-date   = date(12,31,9999)
   begin_prm-date = date(1,1,year(today))
   end_prm-date   = date(12,31,9999).

  RUN enable_UI.

  {methods/nowait.i} 

      APPLY "entry" TO begin_ord-no IN FRAME {&FRAME-NAME}.

    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images1.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images1.p */
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
  DISPLAY begin_ord-no end_ord-no begin_cust-no end_cust-no begin_i-no end_i-no 
          begin_ord-date end_ord-date begin_prm-date end_prm-date tb_re-prnt 
          rd-dest lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_ord-no end_ord-no begin_cust-no end_cust-no begin_i-no end_i-no 
         begin_ord-date end_ord-date begin_prm-date end_prm-date tb_re-prnt 
         rd-dest lv-ornt lines-per-page lv-font-no td-show-parm btn-ok 
         btn-cancel RECT-6 RECT-7 
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
  run scr-rpt.w (list-name,c-win:title,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* -------------------------------------------------------------------------  */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}

def buffer b-oe-ordl for oe-ordl.

def var v-ord   like oe-ord.ord-no      format ">>>>>>"
                                        extent 2 init [0,999999].
def var v-cust  like oe-ord.cust-no     extent 2 init ["","zzzzzzzz"].
def var v-item  like oe-ordl.i-no       extent 2 init ["","zzzzzzzzzzzzzzz"].
def var v-odate like oe-ord.ord-date    format "99/99/9999"
                                        extent 2 init [01/01/0001,12/31/9999].
def var v-pdate like oe-ordl.prom-date  format "99/99/9999"
                                        extent 2 init [01/01/0001,12/31/9999].
def var v-rprnt as   log                init no.

def var v-cost      like oe-ordl.cost.
def var v-area-code like vend.area-code     extent 2.
def var v-phone     like vend.phone         extent 2.
def var v-fax-area  like vend.fax-area      extent 2.
def var v-fax       like vend.fax           extent 2.
def var v-contact   like vend.contact       extent 2.

SESSION:SET-WAIT-STATE("general").

assign
 str-tit2 = c-win:title + " by Job#"
 {sys/inc/ctrtext.i str-tit2 112}

 v-ord[1]    = begin_ord-no
 v-ord[2]    = end_ord-no
 v-cust[1]   = begin_cust-no
 v-cust[2]   = end_cust-no 
 v-item[1]   = begin_i-no
 v-item[2]   = end_i-no
 v-odate[1]  = begin_ord-date
 v-odate[2]  = end_ord-date
 v-pdate[1]  = begin_prm-date
 v-pdate[2]  = end_prm-date
 v-rprnt     = tb_re-prnt.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

display "" with frame r-top.

for each itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    ge v-item[1]
          and itemfg.i-no    le v-item[2]
          and itemfg.pur-man eq yes
        use-index i-no no-lock,

        each oe-ordl
        where oe-ordl.company    eq cocode
          and oe-ordl.i-no       eq itemfg.i-no
          and oe-ordl.ftick-prnt eq v-rprnt
          and oe-ordl.ord-no     ge v-ord[1]
          and oe-ordl.ord-no     le v-ord[2]
          and oe-ordl.prom-date  ge v-pdate[1]
          and oe-ordl.prom-date  le v-pdate[2]
        use-index item,

        first oe-ord
        where oe-ord.company           eq cocode
          and oe-ord.ord-no            eq oe-ordl.ord-no
          and index("CDZ",oe-ord.stat) eq 0
          and oe-ord.cust-no           ge v-cust[1]
          and oe-ord.cust-no           le v-cust[2]
          and oe-ord.ord-date          ge v-odate[1]
          and oe-ord.ord-date          le v-odate[2]
        use-index ord-no no-lock

        by oe-ord.cust-no
        by itemfg.i-no:

      page.

      find first cust
          where cust.company eq cocode
            and cust.cust-no eq oe-ord.cust-no
          no-lock no-error.

      assign
       v-cost      = 0
       v-area-code = ""
       v-phone     = ""
       v-fax-area  = ""
       v-fax       = ""
       v-contact   = "".

      for each b-oe-ordl
          where b-oe-ordl.company  eq cocode
            and b-oe-ordl.i-no     eq oe-ordl.i-no
            and b-oe-ordl.po-no-po ne 0
            and recid(b-oe-ordl)   ne recid(oe-ordl)
          no-lock
          by b-oe-ordl.po-no-po desc:

        v-cost = b-oe-ordl.cost / 1000.
        leave.
      end.

      do i = 1 to 2:
        find first vend
            where vend.company eq cocode
              and vend.vend-no eq (if i eq 1 then itemfg.vend-no
                                             else itemfg.vend2-no)
            no-lock no-error.
        if avail vend then
          assign
           v-area-code[i] = vend.area-code
           v-phone[i]     = vend.phone
           v-fax-area[i]  = vend.fax-area
           v-fax[i]       = vend.fax
           v-contact[i]   = vend.contact.
      end.

      DISPLAY oe-ord.cust-no    colon 40 label "Customer Number"
              cust.name         colon 40 label "Name"           when avail cust
              skip(1)
              oe-ord.ord-no     colon 40 label "Order"
              skip(1)
              itemfg.i-no       colon 40 label "Item Number"
              itemfg.i-name     colon 40 label "Name"
              itemfg.part-dscr1 colon 40 label "Desc"
              itemfg.part-dscr2    at 42 no-label
              itemfg.part-dscr3    at 42 no-label
              oe-ordl.part-no   colon 40 label "Customer Part Number"
              skip(1)
              itemfg.q-onh      colon 40
              oe-ordl.qty       colon 40 label "Customer Order Qty"
              v-cost            colon 40 label "Last Cost"
              skip(1)
              itemfg.vend-no    colon 40 label "Primary Vendor"
              v-area-code[1]    colon 40 label "Phone"
              v-phone[1]                 no-label
              v-fax-area[1]     colon 40 label "Fax"
              v-fax[1]                   no-label
              v-contact[1]      colon 40 label "Contact"
              skip(1)
              itemfg.vend2-no   colon 40 label "Secondary Vendor"
              v-area-code[2]    colon 40 label "Phone"
              v-phone[2]                 no-label
              v-fax-area[2]     colon 40 label "Fax"
              v-fax[2]                   no-label
              v-contact[2]      colon 40 label "Contact"

        with no-box STREAM-IO width 132 no-attr-space no-underline side-labels.

      oe-ordl.ftick-prnt = yes.
    end. /* each itemfg */
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

