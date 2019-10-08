&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-prwcst.w

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
 def var fdate       as   date format "99/99/9999" NO-UNDO.
def var tdate       as   date format "99/99/9999" NO-UNDO.
def var v-inc-fc    as   log init NO NO-UNDO.
def var v-misc      as   log init NO NO-UNDO.

def var v-msf       as   dec format "->,>>>.999" NO-UNDO.
def var v-$msf      as   dec format "->>,>>9.99" NO-UNDO.
def var v-amt       as   dec format "->,>>>,>>9.99" NO-UNDO.
def var v-cost      as   dec format "->,>>>,>>9.99" NO-UNDO.
def var v-prof      as   dec format "->,>>9.99" NO-UNDO.

def var v-mtot-msf       like v-msf NO-UNDO.
def var v-mtot-$msf      like v-$msf NO-UNDO.
def var v-mtot-amt       like v-amt NO-UNDO.
def var v-mtot-cost      like v-cost NO-UNDO.
def var v-mtot-prof      like v-prof NO-UNDO.
def var v-mtot-ptd-msf   like v-mtot-msf NO-UNDO.
def var v-mtot-ptd-$msf  like v-mtot-$msf NO-UNDO.
def var v-mtot-ptd-amt   like v-mtot-amt NO-UNDO.
def var v-mtot-ptd-cost  like v-mtot-cost NO-UNDO.
def var v-mtot-ptd-prof  like v-mtot-prof NO-UNDO.

def var v-gtot-msf       like v-msf NO-UNDO.
def var v-gtot-$msf      like v-$msf NO-UNDO.
def var v-gtot-amt       like v-amt NO-UNDO.
def var v-gtot-cost      like v-cost NO-UNDO.
def var v-gtot-prof      like v-prof NO-UNDO.
def var v-gtot-ptd-msf   like v-gtot-msf NO-UNDO.
def var v-gtot-ptd-$msf  like v-gtot-$msf NO-UNDO.
def var v-gtot-ptd-amt   like v-gtot-amt NO-UNDO.
def var v-gtot-ptd-cost  like v-gtot-cost NO-UNDO.
def var v-gtot-ptd-prof  like v-gtot-prof NO-UNDO.

def var v-procat like fgcat.procat NO-UNDO.
def var v-period as INT NO-UNDO.
def var v-year as int format "9999" NO-UNDO.
def var v-qty like ar-invl.ship-qty format "->>>,>>9.99" NO-UNDO.
def var v-fac as INT NO-UNDO.
def var eps as dec no-undo init .001.
def var abs-msf as dec no-undo format "->>>>>>>>>.99999999".

def var v-hdr1   as char          format "x(10)" init "Category  " NO-UNDO.
def var v-hdr1-1 as char extent 2 format "x(10)" init "       MSF" NO-UNDO.
def var v-hdr1-2 as char extent 2 format "x(10)" init "     $/MSF" NO-UNDO.
def var v-hdr1-3 as char extent 2 format "x(13)" init "       Amount" NO-UNDO.
def var v-hdr1-4 as char extent 2 format "x(13)" init "         Cost" NO-UNDO.
def var v-hdr1-5 as char extent 2 format "x(09)" init "   Profit" NO-UNDO.

def var v-hdr2   as char          format "x(10)" init "----------" NO-UNDO.
def var v-hdr2-1 as char extent 2 format "x(10)" init "----------" NO-UNDO.
def var v-hdr2-2 as char extent 2 format "x(10)" init "----------" NO-UNDO.
def var v-hdr2-3 as char extent 2 format "x(13)" init "-------------" NO-UNDO.
def var v-hdr2-4 as char extent 2 format "x(13)" init "-------------" NO-UNDO.
def var v-hdr2-5 as char extent 2 format "x(09)" init "---------" NO-UNDO.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report.

def TEMP-TABLE w-data NO-UNDO
  field w-procat   like fgcat.procat
  field w-msf      like v-msf
  field w-$msf     like v-$msf
  field w-amt      like v-amt
  field w-prof     like v-prof
  field w-cost     like v-cost
  field w-ptd-msf  like v-msf
  field w-ptd-$msf like v-$msf
  field w-ptd-amt  like v-amt
  field w-ptd-cost like v-cost
  field w-ptd-prof like v-prof.

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS inv-date rd_show1 tb_misc tb_fin-chg rd-dest ~
lv-ornt lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file ~
btn-ok btn-cancel RECT-6 RECT-7 
&Scoped-Define DISPLAYED-OBJECTS inv-date lbl_show1 rd_show1 tb_misc ~
tb_fin-chg rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
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

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-prwcst.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE inv-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_show1 AS CHARACTER FORMAT "X(256)":U INITIAL "Which Cost?" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

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
     SIZE 19 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd_show1 AS CHARACTER INITIAL "Board" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Board", "Board",
"Order", "Order",
"Invoice", "Invoice"
     SIZE 40 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 93 BY 9.29.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 8.57.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_fin-chg AS LOGICAL INITIAL no 
     LABEL "Include Finance Charges?" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE tb_misc AS LOGICAL INITIAL no 
     LABEL "Breakout Miscellaneous Sales?" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1 NO-UNDO.

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
     inv-date AT ROW 3.86 COL 34 COLON-ALIGNED
     lbl_show1 AT ROW 5.05 COL 20 COLON-ALIGNED NO-LABEL
     rd_show1 AT ROW 5.05 COL 36 NO-LABEL
     tb_misc AT ROW 6.24 COL 36
     tb_fin-chg AT ROW 7.43 COL 36
     rd-dest AT ROW 11.24 COL 6 NO-LABEL
     lv-ornt AT ROW 11.48 COL 30 NO-LABEL
     lines-per-page AT ROW 11.48 COL 83 COLON-ALIGNED
     lv-font-no AT ROW 13.38 COL 33 COLON-ALIGNED
     lv-font-name AT ROW 14.33 COL 27 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 15.52 COL 28.8
     tb_excel AT ROW 16.95 COL 49 RIGHT-ALIGNED
     tb_runExcel AT ROW 16.95 COL 70 RIGHT-ALIGNED
     fi_file AT ROW 17.76 COL 27 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 19.81 COL 19
     btn-cancel AT ROW 19.81 COL 57
     RECT-6 AT ROW 9.81 COL 2
     RECT-7 AT ROW 1 COL 1
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 10.29 COL 4
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 21.57.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Sales Analysis - By Customer/Item"
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
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       inv-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_show1 IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_show1:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_show1".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_show1:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_fin-chg:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_misc:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Sales Analysis - By Customer/Item */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Sales Analysis - By Customer/Item */
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
    ASSIGN {&displayed-objects}.
  END.

  run run-report. 
  STATUS DEFAULT "Processing Complete".
  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=tb_fin-chg
                            &END_cust=tb_misc 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
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
       WHEN 6 THEN run output-to-port.
  end case.
  SESSION:SET-WAIT-STATE (""). 
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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


&Scoped-define SELF-NAME inv-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-date C-Win
ON LEAVE OF inv-date IN FRAME FRAME-A /* Invoice Date */
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


&Scoped-define SELF-NAME rd_show1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_show1 C-Win
ON VALUE-CHANGED OF rd_show1 IN FRAME FRAME-A
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


&Scoped-define SELF-NAME tb_fin-chg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fin-chg C-Win
ON VALUE-CHANGED OF tb_fin-chg IN FRAME FRAME-A /* Include Finance Charges? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_misc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_misc C-Win
ON VALUE-CHANGED OF tb_misc IN FRAME FRAME-A /* Breakout Miscellaneous Sales? */
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

  assign
   inv-date = TODAY.

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    APPLY "entry" TO inv-date IN FRAME {&FRAME-NAME}.
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
  DISPLAY inv-date lbl_show1 rd_show1 tb_misc tb_fin-chg rd-dest lv-ornt 
          lines-per-page lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE inv-date rd_show1 tb_misc tb_fin-chg rd-dest lv-ornt lines-per-page 
         lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
         RECT-6 RECT-7 
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
/* -------------------------------------------------------------------------- */
/*                                                                            */
/* -------------------------------------------------------------------------- */
SESSION:SET-WAIT-STATE ("general").

{sys/form/r-topw.f}

DEF VAR excelheader AS CHAR NO-UNDO.

form w-procat      format "x(10)"
     space(2)
     w-msf
     w-$msf
     w-amt
     w-cost
     w-prof
     space(2)
     w-ptd-msf
     w-ptd-$msf
     w-ptd-amt
     w-ptd-cost
     w-ptd-prof

    header
     skip(1)
     space(12)
     "---------------------------Daily---------------------------"
     space(2)
     "-----------------------Period to Date----------------------" skip
     v-hdr1
     space(2)
     v-hdr1-1[1]
     v-hdr1-2[1]
     v-hdr1-3[1]
     v-hdr1-4[1]
     v-hdr1-5[1]
     space(2)
     v-hdr1-1[2]
     v-hdr1-2[2]
     v-hdr1-3[2]
     v-hdr1-4[2]
     v-hdr1-5[2]    skip
     v-hdr2
     space(2)
     v-hdr2-1[1]
     v-hdr2-2[1]
     v-hdr2-3[1]
     v-hdr2-4[1]
     v-hdr2-5[1]
     space(2)
     v-hdr2-1[2]
     v-hdr2-2[2]
     v-hdr2-3[2]
     v-hdr2-4[2]
     v-hdr2-5[2]    skip

    with frame itemx no-box no-underline no-labels down STREAM-IO width 144.
assign

 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 tdate      = inv-date
 v-misc     = tb_misc
 v-inc-fc   = tb_fin-chg.

ASSIGN
 v-gtot-msf = 0
 v-gtot-$msf = 0
 v-gtot-amt = 0
 v-gtot-cost = 0
 v-gtot-prof = 0
 v-gtot-ptd-msf = 0
 v-gtot-ptd-$msf = 0
 v-gtot-ptd-amt = 0
 v-gtot-ptd-cost = 0
 v-gtot-ptd-prof = 0

 v-mtot-msf = 0
 v-mtot-$msf = 0
 v-mtot-amt = 0
 v-mtot-cost = 0
 v-mtot-prof = 0
 v-mtot-ptd-msf = 0
 v-mtot-ptd-$msf = 0
 v-mtot-ptd-amt = 0
 v-mtot-ptd-cost = 0
 v-mtot-ptd-prof = 0
           .


{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "Category,Daily MSF,Daily $/MSF,Daily Amount,Daily Cost,Daily Profit,"
              + "PTD MSF,PTD $/MSF,PTD Amount,PTD Cost,PTD Profit".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

if td-show-parm then run show-param.

display "" with frame r-top.

find first period
    where period.company eq cocode
      and period.pst     le tdate
      and period.pend    ge tdate
    no-lock.
assign
 v-period = period.pnum
 v-year   = period.yr
 fdate    = period.pst.

    for each cust where cust.company eq cocode no-lock:
        {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
      for each ar-inv
          where ar-inv.company  eq cocode
            and ar-inv.posted   eq yes
            and ar-inv.cust-no  eq cust.cust-no
            and ar-inv.inv-date ge fdate
            and ar-inv.inv-date le tdate
            and (ar-inv.type    ne "FC" or v-inc-fc)
          no-lock,

          each ar-invl
          where ar-invl.x-no eq ar-inv.x-no
            and (ar-invl.billable or not ar-invl.misc)
          no-lock:

        create tt-report.

        assign
         tt-report.term-id = ""
         tt-report.rec-id  = recid(ar-invl)
         tt-report.key-01  = "MISC"
         tt-report.key-10  = "ar-invl".

        if not ar-invl.misc then do:
          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq ar-invl.i-no
              no-lock no-error.

          if avail itemfg then tt-report.key-01 = itemfg.procat.

          else do:
            find first fgcat
                where fgcat.company eq cocode
                  and fgcat.glacc   eq ar-invl.actnum
                no-lock no-error.
            if avail fgcat then tt-report.key-01 = fgcat.procat.
          end.
        end.

        tt-report.key-02 = if v-misc and tt-report.key-01 eq "MISC"
                           then ar-invl.actnum else tt-report.key-01.
      end.

      for each ar-cash
          where ar-cash.company    eq cocode
            and ar-cash.cust-no    eq cust.cust-no
            and ar-cash.check-date ge fdate
            and ar-cash.check-date le tdate
            and ar-cash.posted     eq yes
          no-lock,

          EACH ar-cashl
          WHERE ar-cashl.c-no    EQ ar-cash.c-no
            AND ar-cashl.posted  EQ YES
            AND ar-cashl.memo    EQ YES
            AND CAN-FIND(FIRST account
                         WHERE account.company EQ ar-cashl.company
                           AND account.actnum  EQ ar-cashl.actnum
                           AND account.type    EQ "R")
          NO-LOCK:

        create tt-report.

        assign
         tt-report.term-id = ""
         tt-report.key-01  = "MEMO"
         tt-report.key-02  = ar-cashl.actnum
         tt-report.key-10  = "ar-cashl"
         tt-report.rec-id  = recid(ar-cashl).

        RELEASE itemfg.

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

        IF AVAIL reftable                      OR
           ar-cashl.dscr MATCHES "*OE RETURN*" THEN DO:

          if avail oe-retl then
          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq oe-retl.i-no
              no-lock no-error.

          tt-report.key-01 = if avail itemfg then itemfg.procat else "MISC".
        END.

        tt-report.key-02 = if v-misc and tt-report.key-01 eq "MISC"
                           then ar-cashl.actnum else tt-report.key-01.
      end.
    end.

    for each tt-report
        where tt-report.term-id eq ""
          and tt-report.key-01  ne "MISC"
          and tt-report.key-01  ne "MEMO"

    {sa/sa-dsr2.i g}

    /* print totals the first time */
    underline w-msf
              w-$msf
              w-amt
              w-cost
              w-prof
              w-ptd-msf
              w-ptd-$msf
              w-ptd-amt
              w-ptd-cost
              w-ptd-prof

        with frame itemx.

    assign
     v-gtot-$msf     = if v-gtot-msf ne 0 then
                         (v-gtot-amt / v-gtot-msf)
                       else 0
     v-gtot-ptd-$msf = if v-gtot-ptd-msf ne 0 then
                         (v-gtot-ptd-amt / v-gtot-ptd-msf)
                       else 0
     v-gtot-prof     = if v-gtot-amt ne 0 then
                         ((v-gtot-amt - v-gtot-cost) / v-gtot-amt)
                       else 0
     v-gtot-ptd-prof = if v-gtot-ptd-amt ne 0 then
                         ((v-gtot-ptd-amt - v-gtot-ptd-cost) / v-gtot-ptd-amt)
                       else 0
     v-gtot-prof     = v-gtot-prof     * 100
     v-gtot-ptd-prof = v-gtot-ptd-prof * 100.

    put skip(1).

    display "  SALES"                   @ w-procat
            v-gtot-msf                  @ w-msf
            v-gtot-$msf                 @ w-$msf
            v-gtot-amt                  @ w-amt
            v-gtot-cost                 @ w-cost
            v-gtot-prof                 @ w-prof
            v-gtot-ptd-msf              @ w-ptd-msf
            v-gtot-ptd-$msf             @ w-ptd-$msf
            v-gtot-ptd-amt              @ w-ptd-amt
            v-gtot-ptd-cost             @ w-ptd-cost
            v-gtot-ptd-prof             @ w-ptd-prof

        with frame itemx.

    IF tb_excel THEN
       PUT STREAM excel UNFORMATTED
           SKIP(1)
           '"' "SALES"                                    '",'
           '"' STRING(v-gtot-msf,"->,>>>.999")            '",'
           '"' STRING(v-gtot-$msf,"->>,>>9.99")           '",'
           '"' STRING(v-gtot-amt,"->,>>>,>>9.99")         '",'
           '"' STRING(v-gtot-cost,"->,>>>,>>9.99")        '",'
           '"' STRING(v-gtot-prof,"->,>>9.99")            '",'
           '"' STRING(v-gtot-ptd-msf,"->,>>>.999")        '",'
           '"' STRING(v-gtot-ptd-$msf,"->>,>>9.99")       '",'
           '"' STRING(v-gtot-ptd-amt,"->,>>>,>>9.99")     '",'
           '"' STRING(v-gtot-ptd-cost,"->,>>>,>>9.99")    '",'
           '"' STRING(v-gtot-ptd-prof,"->,>>9.99")        '",'
           SKIP.

    find first tt-report
        where tt-report.term-id eq ""
          and tt-report.key-01  eq "MISC"
        no-lock no-error.

    if avail tt-report then do:
      down 2 with frame itemx.
      underline w-msf
                w-$msf
                w-amt
                w-cost
                w-prof
                w-ptd-msf
                w-ptd-$msf
                w-ptd-amt
                w-ptd-cost
                w-ptd-prof

          with frame itemx.

      clear frame itemx no-pause.

      for each tt-report
          where tt-report.term-id eq ""
            and tt-report.key-01  eq "MISC"

      {sa/sa-dsr2.i m}

      if v-misc then do:
        underline w-msf
                  w-$msf
                  w-amt
                  w-cost
                  w-prof
                  w-ptd-msf
                  w-ptd-$msf
                  w-ptd-amt
                  w-ptd-cost
                  w-ptd-prof

            with frame itemx.

        assign
         v-mtot-$msf     = if v-mtot-msf ne 0 then
                             (v-mtot-amt / v-mtot-msf)
                           else 0
         v-mtot-ptd-$msf = if v-mtot-ptd-msf ne 0 then
                             (v-mtot-ptd-amt / v-mtot-ptd-msf)
                           else 0
         v-mtot-prof     = if v-mtot-amt ne 0 then
                             ((v-mtot-amt - v-mtot-cost) / v-mtot-amt)
                           else 0
         v-mtot-ptd-prof = if v-mtot-ptd-amt ne 0 then
                             ((v-mtot-ptd-amt - v-mtot-ptd-cost) /
                                                                v-mtot-ptd-amt)
                           else 0
         v-mtot-prof     = v-mtot-prof     * 100
         v-mtot-ptd-prof = v-mtot-ptd-prof * 100.

        put skip(1).

        display "   MISC"                   @ w-procat
                v-mtot-msf                  @ w-msf
                v-mtot-$msf                 @ w-$msf
                v-mtot-amt                  @ w-amt
                v-mtot-cost                 @ w-cost
                v-mtot-prof                 @ w-prof
                v-mtot-ptd-msf              @ w-ptd-msf
                v-mtot-ptd-$msf             @ w-ptd-$msf
                v-mtot-ptd-amt              @ w-ptd-amt
                v-mtot-ptd-cost             @ w-ptd-cost
                v-mtot-ptd-prof             @ w-ptd-prof

            with frame itemx.

        IF tb_excel THEN
           PUT STREAM excel UNFORMATTED
               SKIP(1)
               '"' "MISC"                                     '",'
               '"' STRING(v-mtot-msf,"->,>>>.999")            '",'
               '"' STRING(v-mtot-$msf,"->>,>>9.99")           '",'
               '"' STRING(v-mtot-amt,"->,>>>,>>9.99")         '",'
               '"' STRING(v-mtot-cost,"->,>>>,>>9.99")        '",'
               '"' STRING(v-mtot-prof,"->,>>9.99")            '",'
               '"' STRING(v-mtot-ptd-msf,"->,>>>.999")        '",'
               '"' STRING(v-mtot-ptd-$msf,"->>,>>9.99")       '",'
               '"' STRING(v-mtot-ptd-amt,"->,>>>,>>9.99")     '",'
               '"' STRING(v-mtot-ptd-cost,"->,>>>,>>9.99")    '",'
               '"' STRING(v-mtot-ptd-prof,"->,>>9.99")        '",'
               SKIP.

        put skip.
      end.

      assign
       v-gtot-msf     = v-gtot-msf     + v-mtot-msf
       v-gtot-amt     = v-gtot-amt     + v-mtot-amt
       v-gtot-ptd-msf = v-gtot-ptd-msf + v-mtot-ptd-msf
       v-gtot-ptd-amt = v-gtot-ptd-amt + v-mtot-ptd-amt.
    end.

    find first tt-report
        where tt-report.term-id eq ""
          and tt-report.key-01  eq "MEMO"
        no-lock no-error.

    if avail tt-report then do:
      down 1 with frame itemx.
      underline w-msf
                w-$msf
                w-amt
                w-cost
                w-prof
                w-ptd-msf
                w-ptd-$msf
                w-ptd-amt
                w-ptd-cost
                w-ptd-prof

          with frame itemx.

      clear frame itemx no-pause.
    end.

    for each tt-report
        where tt-report.term-id eq ""
          and tt-report.key-01  eq "MEMO"

    {sa/sa-dsr2.i g}

    /* Print totals the second time */
    put skip(1).
    underline w-msf
              w-$msf
              w-amt
              w-cost
              w-prof
              w-ptd-msf
              w-ptd-$msf
              w-ptd-amt
              w-ptd-cost
              w-ptd-prof

        with frame itemx.

    assign
     v-gtot-$msf     = if v-gtot-msf ne 0 then
                         (v-gtot-amt / v-gtot-msf)
                       else 0
     v-gtot-ptd-$msf = if v-gtot-ptd-msf ne 0 then
                         (v-gtot-ptd-amt / v-gtot-ptd-msf)
                       else 0
     v-gtot-prof     = if v-gtot-amt ne 0 then
                         ((v-gtot-amt - v-gtot-cost) / v-gtot-amt)
                       else 0
     v-gtot-ptd-prof = if v-gtot-ptd-amt ne 0 then
                         ((v-gtot-ptd-amt - v-gtot-ptd-cost) / v-gtot-ptd-amt)
                       else 0
     v-gtot-prof     = v-gtot-prof     * 100
     v-gtot-ptd-prof = v-gtot-ptd-prof * 100.

    display "  TOTAL"                   @ w-procat
            v-gtot-msf                  @ w-msf
            v-gtot-$msf                 @ w-$msf
            v-gtot-amt                  @ w-amt
            v-gtot-cost                 @ w-cost
            v-gtot-prof                 @ w-prof
            v-gtot-ptd-msf              @ w-ptd-msf
            v-gtot-ptd-$msf             @ w-ptd-$msf
            v-gtot-ptd-amt              @ w-ptd-amt
            v-gtot-ptd-cost             @ w-ptd-cost
            v-gtot-ptd-prof             @ w-ptd-prof

        with frame itemx.

    IF tb_excel THEN
       PUT STREAM excel UNFORMATTED
           SKIP(1)
           '"' "TOTAL"                                    '",'
           '"' STRING(v-gtot-msf,"->,>>>.999")            '",'
           '"' STRING(v-gtot-$msf,"->>,>>9.99")           '",'
           '"' STRING(v-gtot-amt,"->,>>>,>>9.99")         '",'
           '"' STRING(v-gtot-cost,"->,>>>,>>9.99")        '",'
           '"' STRING(v-gtot-prof,"->,>>9.99")            '",'
           '"' STRING(v-gtot-ptd-msf,"->,>>>.999")        '",'
           '"' STRING(v-gtot-ptd-$msf,"->>,>>9.99")       '",'
           '"' STRING(v-gtot-ptd-amt,"->,>>>,>>9.99")     '",'
           '"' STRING(v-gtot-ptd-cost,"->,>>>,>>9.99")    '",'
           '"' STRING(v-gtot-ptd-prof,"->,>>9.99")        '",'
           SKIP.

    for each tt-report where tt-report.term-id eq "":
      delete tt-report.
    end.

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

    IF tb_excel THEN DO:
       OUTPUT STREAM excel CLOSE.
       IF tb_runExcel THEN
          OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

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
                  if lv-field2-hdl:private-data = lv-field-hdl:name THEN
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:screen-value + ",".

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

