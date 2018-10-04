&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: windows/r-sysctl.w

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
/*  &SCOPED-DEFINE nameField {&tableName}.name:SCREEN-VALUE */
/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER ip-name AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ip-module AS CHAR NO-UNDO.

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

{sys/ref/sys-ctrl.i}

assign
 cocode = gcompany
 locode = gloc.


DEF VAR v-print-fmt    AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL   .
DEF VAR ls-fax-file    AS CHAR      NO-UNDO.
DEF VAR security-flag  AS LOG       NO-UNDO.

DEF VAR v-help-line AS CHAR FORMAT "x(100)" EXTENT 100 NO-UNDO.
DEF VAR v-help-text AS CHAR NO-UNDO.

DEF STREAM s-excel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-8 begin_name end_name ~
begin_module end_module TG_xls-help-doc rd-dest lv-ornt lines-per-page ~
lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-cancel btn-ok 
&Scoped-Define DISPLAYED-OBJECTS begin_name end_name begin_module ~
end_module TG_xls-help-doc rd-dest lv-ornt lines-per-page lv-font-no ~
lv-font-name td-show-parm tb_excel tb_runExcel fi_file 

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

DEFINE VARIABLE begin_module AS CHARACTER FORMAT "x(15)" 
     LABEL "Beginning Module" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.

DEFINE VARIABLE begin_name AS CHARACTER FORMAT "x(15)" 
     LABEL "Beginning Name" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.

DEFINE VARIABLE end_module AS CHARACTER FORMAT "x(15)" INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Module" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.

DEFINE VARIABLE end_name AS CHARACTER FORMAT "x(15)" INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Name" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-sysctrl.csv" 
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
     SIZE 20 BY 7.33 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.52.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 10.71.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL no 
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

DEFINE VARIABLE TG_xls-help-doc AS LOGICAL INITIAL no 
     LABEL "Print Help Documentation" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_name AT ROW 3.71 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Name"
     end_name AT ROW 3.71 COL 62.2 COLON-ALIGNED HELP
          "Enter Ending Name"
     begin_module AT ROW 4.95 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Module"
     end_module AT ROW 4.95 COL 62.2 COLON-ALIGNED HELP
          "Enter Ending Module"
     TG_xls-help-doc AT ROW 6.67 COL 29.6
     rd-dest AT ROW 11.52 COL 5 NO-LABEL
     lv-ornt AT ROW 12.24 COL 31 NO-LABEL
     lines-per-page AT ROW 12.24 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 14.14 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 15.1 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 17 COL 30
     tb_excel AT ROW 18.33 COL 50.4 RIGHT-ALIGNED
     tb_runExcel AT ROW 18.33 COL 72.4 RIGHT-ALIGNED
     fi_file AT ROW 19.57 COL 28.4 COLON-ALIGNED HELP
          "Enter File Name"
     btn-cancel AT ROW 21.48 COL 57
     btn-ok AT ROW 21.71 COL 19
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 10.81 COL 2
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.48 COL 2.8
          BGCOLOR 2 
     RECT-7 AT ROW 1 COL 1
     RECT-8 AT ROW 10.52 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 22.86.


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
         TITLE              = "N-K Parameters"
         HEIGHT             = 23.57
         WIDTH              = 96
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
       begin_module:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_name:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_module:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_name:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* N-K Parameters */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* N-K Parameters */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_module
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_module C-Win
ON LEAVE OF begin_module IN FRAME FRAME-A /* Beginning Module */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_name C-Win
ON LEAVE OF begin_name IN FRAME FRAME-A /* Beginning Name */
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

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type= " "
                            &begin_cust= "begin_name"
                            &END_cust= "begin_name" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = " "
                             &begin_cust= "begin_name"
                             &END_cust= "begin_name"
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = " "
                                  &begin_cust="begin_name"
                                  &END_cust="begin_name"
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.
       END. 
       WHEN 6 THEN RUN OUTPUT-to-port.
  end case.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_module
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_module C-Win
ON LEAVE OF end_module IN FRAME FRAME-A /* Ending Module */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_name C-Win
ON LEAVE OF end_name IN FRAME FRAME-A /* Ending Name */
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
/*   IF access-close THEN DO:            */
/*      APPLY "close" TO THIS-PROCEDURE. */
/*      RETURN .                         */
/*   END.                                */

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}

    ASSIGN
       begin_name:SCREEN-VALUE = ip-name
       end_name:SCREEN-VALUE = ip-name
       begin_module:SCREEN-VALUE = ip-module
       end_module:SCREEN-VALUE = ip-module.

    APPLY "entry" TO begin_name.
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
  DISPLAY begin_name end_name begin_module end_module TG_xls-help-doc rd-dest 
          lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-7 RECT-8 begin_name end_name begin_module end_module 
         TG_xls-help-doc rd-dest lv-ornt lines-per-page lv-font-no td-show-parm 
         tb_excel tb_runExcel fi_file btn-cancel btn-ok 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-help-doc C-Win 
PROCEDURE get-help-doc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR v-return AS CHAR NO-UNDO.
DEF VAR v-line-feed AS CHAR NO-UNDO.

DEF VAR v-spcnt AS INT NO-UNDO.
DEF VAR v-lntxt AS CHAR NO-UNDO.
DEF VAR v-cnt1  AS INT NO-UNDO INIT 0.
DEF VAR v-cnt2  AS INT NO-UNDO INIT 0.
DEF VAR v-excnt AS INT NO-UNDO INIT 1.

ASSIGN v-return    = CHR(13)
       v-line-feed = CHR(10) 
       v-help-line = ""
       v-spcnt    = 0
       v-lntxt    = ""
       v-cnt1     = 0
       v-cnt2     = 0
       v-excnt    = 0.

FOR EACH hlp-head NO-LOCK WHERE hlp-head.fld-name = sys-ctrl.NAME:

   ASSIGN v-help-text = hlp-head.help-txt
          v-help-text = REPLACE(v-help-text, v-return , ' ')
          v-help-text = REPLACE(v-help-text, v-line-feed , ' ').

   DO v-cnt1 = 1 TO 100:

    ASSIGN v-excnt = v-excnt + 1
           v-spcnt = 0
           v-help-text = TRIM(v-help-text).

    IF LENGTH(v-help-text) GT 100 THEN DO:
      IF SUBSTR(v-help-text,99,1) NE "" AND 
         SUBSTR(v-help-text,101,1) NE ""
        THEN ASSIGN v-spcnt = R-INDEX(SUBSTR(v-help-text,1,100)," ")
                    v-lntxt = SUBSTR(v-help-text,1,v-spcnt)
                    v-help-text = SUBSTR(v-help-text,v-spcnt + 1).
        ELSE
         IF SUBSTR(v-help-text,99,1) EQ "" AND 
            SUBSTR(v-help-text,101,1) NE ""
           THEN ASSIGN v-spcnt = R-INDEX(SUBSTR(v-help-text,1,100)," ")
                       v-lntxt = SUBSTR(v-help-text,1,v-spcnt)
                       v-help-text = SUBSTR(v-help-text,v-spcnt + 1).
           ELSE ASSIGN v-lntxt = SUBSTR(v-help-text,1,100)
                       v-help-text = SUBSTR(v-help-text,101).

       ASSIGN v-help-line[v-excnt] = v-help-line[v-excnt] + v-lntxt.

    END.
    ELSE  ASSIGN v-help-line[v-excnt] = v-help-line[v-excnt] + v-help-text
                 v-help-text        = "".           
    IF v-help-text EQ ""  THEN LEAVE.

  END.
END. 


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
  run scr-rpt.w (list-name,c-win:title,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/***************************************************************************\
*****************************************************************************
**  Program: /u2/fold/all/dev/asi/oe/rep
**       by: Christopher A. Heins, 07.14.95
** Descript: SalesRep Performance daily, period and year to date.
**
*****************************************************************************
\***************************************************************************/

{sys/form/r-topw.f}
{custom/notesdef.i}

DEF VAR ii LIKE i NO-UNDO.
DEF VAR ls-name-value AS CHAR FORMAT "x(100)" NO-UNDO. 
DEF VAR v-inst AS CHAR FORMAT "X(120)" EXTENT 20 NO-UNDO.
DEF VAR v-char AS CHAR FORMAT "X(1)" NO-UNDO.
DEF VAR v-cut-at AS INT NO-UNDO.
DEF VAR v-x AS INT NO-UNDO.
DEF VAR v-exp-name AS CHAR FORMAT "x(40)" INIT "c:\tmp\r-sysctl.csv".

DEF VAR v-lincnt AS INT NO-UNDO.
DEF VAR v-hlpdoc AS CHAR NO-UNDO.

FORMAT HEADER
   "NAME"
   "DESCRIPTION"  AT 19
   "SYSTEM"       AT 62
   "INTEGER"      AT 76
   "DECIMAL"      AT 93
   "CHARACTER"    AT 107
   "DATE"         AT 123
   "LOGICAL"      AT 140
   SKIP
   "   "
   "   "
   "MODULE"       AT 62
   "VALUE"        AT 77
   "VALUE"        AT 94
   "VALUE"        AT 109
   "VALUE"        AT 123
   "VALUE"        AT 141
   SKIP
   FILL('-',17) FORMAT "x(17)"  
   FILL('-',42) FORMAT "x(42)"  AT 19
   FILL('-',13) FORMAT "x(13)"  AT 62
   FILL('-',16) FORMAT "x(16)"  AT 76
   FILL('-',13) FORMAT "x(13)"  AT 93
   FILL('-',15) FORMAT "x(15)"  AT 107
   FILL('-',16) FORMAT "x(16)"  AT 123
   FILL('-',10) FORMAT "x(10)"  AT 140
/*    FILL("_",146) FORMAT "x(146)" */
  WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top PAGE-TOP WIDTH 180 STREAM-IO.

ASSIGN
   str-tit2 = c-win:title
   {sys/inc/ctrtext.i str-tit2 112}.


{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF td-show-parm THEN RUN show-param.

DISPLAY "" WITH FRAME r-top. 
DISPLAY "" WITH FRAME f-top.

SESSION:SET-WAIT-STATE ("general").

IF tb_excel THEN DO:
  OUTPUT STREAM s-excel TO VALUE(fi_file).
  PUT STREAM s-excel UNFORMATTED 
  "NAME,DESCRIPTION,SYSTEM MODULE,INTEGER VALUE,DECIMAL VALUE,"
  "CHARACTER VALUE,DATE VALUE,LOGICAL VALUE".

  IF TG_xls-help-doc 
    THEN PUT STREAM s-excel UNFORMATTED ",TITLE,HELP DOCUMENT" SKIP.
    ELSE PUT STREAM s-excel UNFORMATTED SKIP.

END.  

FOR EACH sys-ctrl NO-LOCK WHERE sys-ctrl.company = g_company
                            AND sys-ctrl.NAME >= begin_name
                            AND sys-ctrl.NAME <= END_name 
                            AND sys-ctrl.module >= begin_module
                            AND sys-ctrl.module <= END_module
                             BY sys-ctrl.NAME:
   DISPLAY
      TRIM(sys-ctrl.NAME)     FORMAT "X(15)"
      TRIM(sys-ctrl.descrip)  FORMAT "X(40)"      AT 19  
      TRIM(sys-ctrl.module)   FORMAT "X(5)"       AT 62  
      sys-ctrl.int-fld        FORMAT "-9,999,999" AT 76  
      sys-ctrl.dec-fld        FORMAT "-99,999.99" AT 93  
      TRIM(sys-ctrl.char-fld) FORMAT "X(9)"       AT 109
      sys-ctrl.date-fld       FORMAT "99/99/9999" AT 125
      sys-ctrl.log-fld        FORMAT "YES/NO"     AT 145
      WITH FRAME f-sys-ctrl NO-LABELS NO-BOX NO-UNDERLINE DOWN WIDTH 180.

   IF tb_excel THEN DO:
     PUT STREAM s-excel UNFORMATTED
        '"' TRIM(sys-ctrl.NAME)    '",'
        '"'  TRIM(sys-ctrl.descrip)     '",'
        '"' TRIM(sys-ctrl.module)      '",'
        '"' sys-ctrl.int-fld           '",'
        '"' sys-ctrl.dec-fld           '",'
        '"' TRIM(sys-ctrl.char-fld)    '",'
        '"' sys-ctrl.date-fld          '",'
        '"' sys-ctrl.log-fld           '",'
         SKIP
       .
   END.


   IF TG_xls-help-doc THEN DO:

     FIND FIRST hlp-head NO-LOCK 
       WHERE hlp-head.fld-name = sys-ctrl.NAME NO-ERROR.
     IF AVAIL hlp-head THEN DO:       

       IF tb_excel THEN DO:
         PUT STREAM s-excel UNFORMATTED           
           '"' REPLACE(TRIM(hlp-head.FRM-TITLE),',',' ') '",'.
       END.

       PUT
        SKIP(2)
        "Help Documentation for: "      AT 10
         TRIM(hlp-head.FRM-TITLE) FORMAT "x(20)" AT 38
        SKIP
         FILL("-",75) FORMAT "x(75)" AT 1 SKIP
        SKIP .

       ASSIGN v-help-text = "".

       FIND FIRST hlp-head NO-LOCK 
           WHERE hlp-head.fld-name EQ ip-NAME NO-ERROR.
       IF AVAIL hlp-head THEN ASSIGN v-help-text = hlp-head.help-txt.

       IF TRIM(v-help-text) NE "" THEN
          DISP 
            v-help-text  VIEW-AS EDITOR SIZE 75 BY 5 
           WITH FRAME abc NO-BOX NO-LABEL.

       IF tb_excel THEN DO:
         PUT STREAM s-excel UNFORMATTED
             '"' v-help-text '",'
             SKIP. 
       END.

     END. /* Hlp-head */
   END. /* TG_xls */       

END. /* FOR EACH sys-ctrl */

IF tb_excel THEN DO:
   OUTPUT STREAM s-excel CLOSE.

   IF tb_runExcel 
     THEN OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).

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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

