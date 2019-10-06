&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-detjnl.w

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
DEFINE VARIABLE ou-log      LIKE sys-ctrl.log-fld NO-UNDO INITIAL NO.
DEFINE VARIABLE ou-cust-int LIKE sys-ctrl.int-fld NO-UNDO.

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

{sys/ref/CustList.i NEW}
DEFINE VARIABLE glCustListActive AS LOGICAL     NO-UNDO.

{salrep/ttreport.i NEW}

def TEMP-TABLE w-data NO-UNDO
  field w-cust-no   like ar-inv.cust-no column-label "Customer"
  field w-inv-date  like ar-inv.inv-date column-label "Invoice!Date"
  field w-ord-no    like ar-invl.ord-no column-label "Order!Number"
  field w-inv-no    like ar-invl.inv-no column-label "Invoice!Number"
  field w-bol-no    like ar-invl.inv-no column-label " BOL!Number"
  field w-recid     as recid
  field w-type      as int.

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tb_cust-list btnCustList ~
begin_cust-no end_cust-no begin_inv-date end_inv-date tb_freight tb_summary ~
tb_fin-chg tb_cred tb_prt-bol lv-ornt lines-per-page rd-dest lv-font-no ~
td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_cust-list begin_cust-no end_cust-no ~
begin_inv-date end_inv-date tb_freight tb_summary tb_fin-chg tb_cred ~
tb_prt-bol lv-ornt lines-per-page rd-dest lv-font-no lv-font-name ~
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

DEFINE BUTTON btnCustList 
     LABEL "Preview" 
     SIZE 9.8 BY .81.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_inv-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_inv-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-cstshp.csv" 
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

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.57.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 10.

DEFINE VARIABLE tb_cred AS LOGICAL INITIAL no 
     LABEL "Sort By Credit Rating?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.2 BY .95 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_fin-chg AS LOGICAL INITIAL no 
     LABEL "Include Finance Charges?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE tb_freight AS LOGICAL INITIAL no 
     LABEL "Show Freight Charges?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prt-bol AS LOGICAL INITIAL no 
     LABEL "Print BOL#?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_summary AS LOGICAL INITIAL no 
     LABEL "Summary Only?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tb_cust-list AT ROW 2.05 COL 30.8 WIDGET-ID 6
     btnCustList AT ROW 2.1 COL 62.8 WIDGET-ID 8
     begin_cust-no AT ROW 3.19 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 3.19 COL 68 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_inv-date AT ROW 4.14 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Invoice Date"
     end_inv-date AT ROW 4.14 COL 68 COLON-ALIGNED HELP
          "Enter Ending Invoice Date"
     tb_freight AT ROW 5.62 COL 35
     tb_summary AT ROW 6.57 COL 35
     tb_fin-chg AT ROW 7.52 COL 35
     tb_cred AT ROW 8.48 COL 35
     tb_prt-bol AT ROW 9.43 COL 35
     lv-ornt AT ROW 11.95 COL 30 NO-LABEL
     lines-per-page AT ROW 11.95 COL 83 COLON-ALIGNED
     rd-dest AT ROW 12.19 COL 5 NO-LABEL
     lv-font-no AT ROW 13.62 COL 33 COLON-ALIGNED
     lv-font-name AT ROW 14.57 COL 27 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 16.24 COL 29
     tb_excel AT ROW 17.19 COL 67 RIGHT-ALIGNED
     tb_runExcel AT ROW 17.19 COL 89 RIGHT-ALIGNED
     fi_file AT ROW 18.14 COL 45 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 20.29 COL 26
     btn-cancel AT ROW 20.29 COL 57
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11.71 COL 5
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 11.24 COL 1
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
         TITLE              = "Sales Analysis - Sales Journal"
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
   FRAME-NAME                                                           */
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
       begin_inv-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_inv-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_cred:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       tb_freight:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_prt-bol:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_summary:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Sales Analysis - Sales Journal */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Sales Analysis - Sales Journal */
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


&Scoped-define SELF-NAME begin_inv-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_inv-date C-Win
ON LEAVE OF begin_inv-date IN FRAME FRAME-A /* Beginning Invoice Date */
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

  FIND FIRST  ttCustList NO-LOCK NO-ERROR.
  IF NOT tb_cust-list OR  NOT AVAIL ttCustList THEN do:
  EMPTY TEMP-TABLE ttCustList.
  RUN BuildCustList(INPUT cocode,
                    INPUT tb_cust-list AND glCustListActive ,
                    INPUT begin_cust-no,
                    INPUT end_cust-no).
  END.

  run run-report. 
  STATUS DEFAULT "Processing Complete".
  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type="Customer"
                            &begin_cust=begin_cust-no
                            &END_cust= begin_cust-no
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = "Customer"
                             &begin_cust= begin_cust-no
                             &END_cust=begin_cust-no
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Customer"
                                  &begin_cust= begin_cust-no
                                  &END_cust=begin_cust-no
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
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


&Scoped-define SELF-NAME btnCustList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCustList C-Win
ON CHOOSE OF btnCustList IN FRAME FRAME-A /* Preview */
DO:
  RUN CustList.

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


&Scoped-define SELF-NAME end_inv-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_inv-date C-Win
ON LEAVE OF end_inv-date IN FRAME FRAME-A /* Ending Invoice Date */
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


&Scoped-define SELF-NAME tb_cred
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cred C-Win
ON VALUE-CHANGED OF tb_cred IN FRAME FRAME-A /* Sort By Credit Rating? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cust-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cust-list C-Win
ON VALUE-CHANGED OF tb_cust-list IN FRAME FRAME-A /* Use Defined Customer List */
DO:
  assign {&self-name}.
  EMPTY TEMP-TABLE ttCustList.
  RUN SetCustRange(INPUT tb_cust-list).
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


&Scoped-define SELF-NAME tb_prt-bol
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-bol C-Win
ON VALUE-CHANGED OF tb_prt-bol IN FRAME FRAME-A /* Print BOL#? */
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


&Scoped-define SELF-NAME tb_summary
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_summary C-Win
ON VALUE-CHANGED OF tb_summary IN FRAME FRAME-A /* Summary Only? */
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
   begin_inv-date = date(1,1,year(today))
   end_inv-date   = today.

  RUN enable_UI.

  {methods/nowait.i}

  RUN sys/inc/CustListForm.p ( "HB",cocode, 
                               OUTPUT ou-log,
                               OUTPUT ou-cust-int) .

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    APPLY "entry" TO begin_cust-no.
  END.

  RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'HB',
                          INPUT NO,
                          OUTPUT glCustListActive).
  {sys/inc/chblankcust.i ""HB""}

  IF ou-log THEN DO:
      ASSIGN 
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = YES
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "yes"
        tb_cust-list = YES 
        .
      RUN SetCustRange(INPUT tb_cust-list).
  END.
  ELSE
      ASSIGN
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        .

   IF ou-log AND ou-cust-int = 0 THEN do:
       ASSIGN 
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = YES
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "No"
        tb_cust-list = NO
        .
      RUN SetCustRange(tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "YES").
   END.

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildCustList C-Win 
PROCEDURE BuildCustList :
/*------------------------------------------------------------------------------
  Purpose:     Builds the temp table of customers   
  Parameters:  Company Code, Customer list logical and/or customer range
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iplList AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipcBeginCust AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcEndCust AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-cust FOR cust.

DEFINE VARIABLE lActive AS LOGICAL     NO-UNDO.

IF iplList THEN DO:
    RUN sys/ref/CustList.p (INPUT ipcCompany,
                            INPUT 'HB',
                            INPUT YES,
                            OUTPUT lActive).
END.
ELSE DO:
    FOR EACH bf-cust
        WHERE bf-cust.company EQ ipcCompany
          AND bf-cust.cust-no GE ipcBeginCust
          AND bf-cust.cust-no LE ipcEndCust
        NO-LOCK:
        CREATE ttCustList.
        ASSIGN 
            ttCustList.cust-no = bf-cust.cust-no
            ttCustList.log-fld = YES
        .
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CustList C-Win 
PROCEDURE CustList :
/*------------------------------------------------------------------------------
  Purpose:  Display a UI of selected customers   
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

    RUN sys/ref/CustListManager.w(INPUT cocode,
                                  INPUT 'HB').


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
  DISPLAY tb_cust-list begin_cust-no end_cust-no begin_inv-date end_inv-date 
          tb_freight tb_summary tb_fin-chg tb_cred tb_prt-bol lv-ornt 
          lines-per-page rd-dest lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tb_cust-list btnCustList begin_cust-no end_cust-no 
         begin_inv-date end_inv-date tb_freight tb_summary tb_fin-chg tb_cred 
         tb_prt-bol lv-ornt lines-per-page rd-dest lv-font-no td-show-parm 
         tb_excel tb_runExcel fi_file btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ouput-to-port C-Win 
PROCEDURE ouput-to-port :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN custom/d-print.w (list-name).
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
/***************************************************************************\
*****************************************************************************
**  Program: /u2/fold/all/dev/asi/sa/sa-slsj.p
**       by: Dave Reimer, Update by Chris Heins 9507
** Descript: Sales analysis daily sales by category
**
*****************************************************************************
\***************************************************************************/

{sys/form/r-topw.f}

def var v-qty like ar-invl.ship-qty format "->>>,>>9.999" decimals 3.
def var v-sq-ft like itemfg.t-sqft format "->>>9.999".
def var v-tot-sf-sht as dec format "->>,>>>,>>9".
def var v-tot-msf    as dec format "->>,>>9.99".
def var fdate as date format "99/99/9999".
def var tdate like fdate.
def var fcust as ch init " ".
def var tcust like fcust init "zzzzzzzz".
def var v-inc-fc as log init no no-undo.
def var v-procat like itemfg.procat no-undo.
def var v-amt as dec no-undo.
def var tot-qty like v-qty.
def var tot-sf-sht like v-tot-sf-sht.
def var tot-amt  like ar-inv.tot-ord.
def var cust-qty like v-qty no-undo.
def var cust-sf-sht like v-tot-sf-sht no-undo.
def var cust-amt like ar-inv.tot-ord no-undo.
def var w-qty       like v-qty column-label "Quantity!Shipped/M".
def var w-sq-ft     like v-sq-ft column-label "Sq Ft".
def var w-tot-sf-sht like v-tot-sf-sht column-label "Total!Sq Ft".
def var w-tot-msf   like v-tot-msf column-label "$/MSF".
def var w-procat    like itemfg.procat column-label "Prod!Code".
def var w-amt       like ar-invl.amt column-label "Invoice!Amount".
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.
DEFINE BUFFER bf-ar-inv FOR ar-inv.
 form w-data.w-cust-no
     cust.name          FORMAT "x(26)"
     cust.cr-rating     LABEL "C R"
     w-data.w-inv-date
     w-data.w-ord-no
     w-data.w-inv-no
     w-qty
     w-sq-ft SPACE(2)
     w-tot-sf-sht
     w-tot-msf
     w-procat
     w-amt
    with no-box frame itemx down STREAM-IO width 180.

 form w-data.w-cust-no
     cust.name          FORMAT "x(26)"
     w-data.w-bol-no    
     cust.cr-rating     LABEL "C R"
     w-data.w-inv-date
     w-data.w-ord-no
     w-data.w-inv-no
     w-qty
     w-sq-ft SPACE(2)
     w-tot-sf-sht
     w-tot-msf
     w-procat
     w-amt
    with no-box frame itemxb down STREAM-IO width 180.


 SESSION:SET-WAIT-STATE ("general").

 ASSIGN
  str-tit2 = c-win:title
  {sys/inc/ctrtext.i str-tit2 112}

  fcust      = begin_cust-no
  tcust      = end_cust-no
  fdate      = begin_inv-date
  tdate      = end_inv-date
  v-inc-fc   = tb_fin-chg.


{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

IF tb_excel THEN DO:
          OUTPUT STREAM excel TO VALUE(fi_file).
          excelheader = "Cust#,Name,BOL#,C/R,INV Date,Order#,Inv#," +
                        "QTY Shipped/M,Sq Ft,Total Sq Ft,$/MSF,Prod Code," +
                        "Inv Amount".


      PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' skip.
END. 

display "" with frame r-top.

FOR EACH ttCustList 
    WHERE ttCustList.log-fld
    NO-LOCK,
    each cust
        where cust.company eq cocode
          and cust.cust-no EQ ttCustList.cust-no /*fcust*/
      /*   and cust.cust-no le tcust*/
        no-lock:
      for each ar-inv
          where ar-inv.company  eq cocode
            and ar-inv.posted   eq yes
            and ar-inv.cust-no  eq cust.cust-no
            and ar-inv.inv-date ge fdate
            and ar-inv.inv-date le tdate
            and (ar-inv.type    ne "FC" or v-inc-fc)
          NO-LOCK:
          {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}

        IF tb_freight AND ar-inv.f-bill AND ar-inv.freight NE 0 THEN DO:
          RUN salrep/invfrate.p (ROWID(ar-inv), "allsalesmen", "").

          FOR EACH tt-report2
              WHERE tt-report2.key-10 EQ "ar-invf"
                AND tt-report2.inv-no EQ ar-inv.inv-no,
              FIRST ar-invl WHERE RECID(ar-invl) EQ tt-report2.rec-id NO-LOCK:
            ASSIGN
             tt-report2.cred   = IF tb_cred THEN cust.cr-rating ELSE ""
             tt-report2.key-01 = string(cust.name,"x(40)") + cust.cust-no
             tt-report2.key-02 = STRING(YEAR(ar-inv.inv-date),"9999") +
                                 STRING(MONTH(ar-inv.inv-date),"99")  +
                                 STRING(DAY(ar-inv.inv-date),"99")
             tt-report2.key-03 = IF tb_summary THEN ""
                                 ELSE STRING(ar-invl.ord-no,"999999")
             tt-report2.key-04 = STRING(ar-inv.inv-no,"9999999999")
             tt-report2.key-05 = "3"
             tt-report2.key-06 = cust.cust-no
             tt-report2.rec-id = RECID(ar-inv).
          END.
        END.

        FOR each ar-invl
            where ar-invl.x-no eq ar-inv.x-no
              and (ar-invl.billable or not ar-invl.misc)
            NO-LOCK:

          create tt-report.
          assign
           tt-report.term-id = ""
           tt-report.cred    = IF tb_cred THEN cust.cr-rating ELSE ""
           tt-report.rec-id  = recid(ar-invl)
           tt-report.key-01  = string(cust.name,"x(40)") + cust.cust-no
           tt-report.key-02  = string(year(ar-inv.inv-date),"9999") +
                               string(month(ar-inv.inv-date),"99")  +
                               string(day(ar-inv.inv-date),"99")
           tt-report.key-03  = IF tb_summary THEN ""
                               ELSE STRING(ar-invl.ord-no,"999999")
           tt-report.key-04  = string(ar-invl.inv-no,"9999999999")
           tt-report.key-05  = "1"
           tt-report.key-06  = cust.cust-no.
        end.
      END.

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

          FIND FIRST bf-ar-inv NO-LOCK 
              where bf-ar-inv.company  eq cocode
                and bf-ar-inv.posted   eq yes
                and bf-ar-inv.cust-no  eq cust.cust-no
                and bf-ar-inv.inv-no EQ ar-cashl.inv-no NO-ERROR.
            IF AVAIL bf-ar-inv THEN DO:
                IF NOT v-inc-fc THEN
                    IF bf-ar-inv.TYPE EQ "FC" THEN NEXT .
            END.

        RELEASE oe-retl.

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

        create tt-report.

        assign
         tt-report.term-id = ""
         tt-report.cred    = IF tb_cred THEN cust.cr-rating ELSE ""
         tt-report.rec-id  = recid(ar-cashl)
         tt-report.key-01  = string(cust.name,"x(40)") + cust.cust-no
         tt-report.key-02  = string(year(ar-cash.check-date),"9999") +
                             string(month(ar-cash.check-date),"99")  +
                             string(day(ar-cash.check-date),"99")
         tt-report.key-03  = IF tb_summary THEN ""
                             ELSE string(if avail oe-retl then oe-retl.ord-no
                                         else 0,"999999")
         tt-report.key-04  = string(ar-cashl.inv-no,"9999999999")
         tt-report.key-05  = "2"
         tt-report.key-06  = cust.cust-no.
      end.
    end.

    for each tt-report where tt-report.term-id eq "",

        first cust
        where cust.company eq cocode
          and cust.cust-no eq tt-report.key-06
        no-lock

        break by tt-report.cred
              by tt-report.key-01
              by tt-report.key-02
              by tt-report.key-03
              by tt-report.key-04

        with frame itemx down

        transaction:

        {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}

      create w-data.
      assign
       w-data.w-cust-no  = tt-report.key-06
       w-data.w-inv-date = date(int(substr(tt-report.key-02,5,2)),
                                int(substr(tt-report.key-02,7,2)),
                                int(substr(tt-report.key-02,1,4)))
       w-data.w-ord-no   = int(tt-report.key-03)
       w-data.w-inv-no   = int(tt-report.key-04)
       w-data.w-type     = int(tt-report.key-05)
       w-data.w-recid    = tt-report.rec-id
       v-sq-ft           = 0.

      if w-data.w-type eq 1 then do:
        find first ar-invl where recid(ar-invl) eq w-data.w-recid no-lock.

        assign
         v-amt    = ar-invl.amt
         v-qty    = ar-invl.inv-qty / 1000
         v-sq-ft  = ar-invl.amt-msf * 1000 / ar-invl.ship-qty
         v-procat = "MISC".

        if v-sq-ft eq ? then v-sq-ft = 0.

        if not ar-invl.misc then do:
          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq ar-invl.i-no
                no-lock no-error.
          if avail itemfg then do:
            v-procat = itemfg.procat.
            if v-sq-ft eq 0 then v-sq-ft = itemfg.t-sqft.
          end.

          else do:
            find first fgcat
                where fgcat.company eq cocode
                  and fgcat.glacc   eq ar-invl.actnum
                no-lock no-error.
            if avail fgcat then v-procat = fgcat.procat.
          end.
        end.  /* else  */

        v-tot-sf-sht = ar-invl.ship-qty * v-sq-ft.

        if v-tot-sf-sht ne 0 then
          v-tot-msf = (ar-invl.amt / (v-tot-sf-sht / 1000)).
        else
          v-tot-msf = 0.

        assign
         w-bol-no     = ar-invl.bol-no
         w-qty        = v-qty
         w-sq-ft      = v-sq-ft
         w-tot-sf-sht = v-tot-sf-sht
         w-tot-msf    = v-tot-msf
         w-procat     = v-procat
         w-amt        = ar-invl.amt.
      end.

      else
      if w-data.w-type eq 2 then do:
        find first ar-cashl where recid(ar-cashl) eq w-data.w-recid no-lock.

        assign
         w-bol-no     = 0
         w-qty        = 0
         w-sq-ft      = 0
         w-tot-sf-sht = 0
         w-tot-msf    = 0
         w-amt        = ar-cashl.amt-paid - ar-cashl.amt-disc
         w-procat     = if w-amt lt 0 then "CRMEM" else
                        if w-amt gt 0 then "DBMEM" else "MEMO".

        RELEASE itemfg.

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

        if avail oe-retl then do:
          w-procat = "MISC".

          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq oe-retl.i-no
              no-lock no-error.

          assign
           v-sq-ft      = if avail itemfg then itemfg.t-sqft else 0
           v-qty        = oe-retl.tot-qty-return / 1000
           v-tot-sf-sht = oe-retl.tot-qty-return * v-sq-ft

           w-qty        = w-qty + v-qty
           w-sq-ft      = w-sq-ft + v-sq-ft
           w-tot-sf-sht = w-tot-sf-sht + v-tot-sf-sht.
        END.

        assign
         w-qty        = - w-qty
         w-tot-sf-sht = - w-tot-sf-sht
         w-tot-msf    = if w-tot-sf-sht ne 0 then
                          (w-amt / (w-tot-sf-sht / 1000)) else 0
         w-procat     = if avail itemfg then itemfg.procat else "MISC".
      end.

      ELSE
      IF w-data.w-type EQ 3 THEN DO:
        ASSIGN
         w-bol-no     = 0
         w-qty        = 0
         w-sq-ft      = 0
         w-tot-sf-sht = 0
         w-tot-msf    = 0
         w-procat     = ""
         w-amt        = tt-report.freight.
      END.

      accumulate w-qty (total by tt-report.cred).
      accumulate w-tot-sf-sht (total by tt-report.cred).
      accumulate w-amt (total by tt-report.cred).

      accumulate w-qty (total by tt-report.key-01).
      accumulate w-tot-sf-sht (total by tt-report.key-01).
      accumulate w-amt (total by tt-report.key-01).

      accumulate w-qty (total by tt-report.key-04).
      accumulate w-sq-ft (total by tt-report.key-04).
      accumulate w-tot-sf-sht (total by tt-report.key-04).
      accumulate w-tot-msf (total by tt-report.key-04).
      accumulate w-amt (total by tt-report.key-04).

      if line-counter ge (page-size - 1) then page.

      if first-of(tt-report.key-01) and first-of(tt-report.key-03) then put skip(1).

      IF NOT tb_summary OR LAST-OF(tt-report.key-04) THEN DO:
       IF tb_prt-bol THEN DO:
        DISPLAY
          w-data.w-cust-no    WHEN FIRST-OF(tt-report.key-01)
                                OR tb_summary
          cust.name           WHEN FIRST-OF(tt-report.key-01)
                                OR tb_summary
          w-data.w-bol-no     WHEN NOT tb_summary
          cust.cr-rating      WHEN FIRST-OF(tt-report.key-01)
                                OR tb_summary
          w-data.w-inv-date   FORMAT "99/99/99"
                              WHEN FIRST-OF(tt-report.key-04)
                                OR tb_summary
          w-data.w-ord-no     WHEN NOT tb_summary
          w-data.w-inv-no
          w-qty
            (ACCUMULATE TOTAL BY tt-report.key-04 w-qty)
                              WHEN tb_summary @ w-qty 
          w-sq-ft
            (ACCUMULATE TOTAL BY tt-report.key-04 w-sq-ft)
                              WHEN tb_summary @ w-sq-ft 
          w-tot-sf-sht
            (ACCUMULATE TOTAL BY tt-report.key-04 w-tot-sf-sht)
                              WHEN tb_summary @ w-tot-sf-sht 
          w-tot-msf 
            (ACCUMULATE TOTAL BY tt-report.key-04 w-tot-msf)
                              WHEN tb_summary @ w-tot-msf           
          w-procat            WHEN NOT tb_summary
          w-amt
            (ACCUMULATE TOTAL BY tt-report.key-04 w-amt)
                              WHEN tb_summary @ w-amt
          WITH FRAME itemxb.
        DOWN WITH FRAME itemxb.
       END.

       ELSE DO:
        DISPLAY
          w-data.w-cust-no    WHEN FIRST-OF(tt-report.key-01)
                                OR tb_summary
          cust.name           WHEN FIRST-OF(tt-report.key-01)
                                OR tb_summary
          cust.cr-rating      WHEN FIRST-OF(tt-report.key-01)
                                OR tb_summary
          w-data.w-inv-date   FORMAT "99/99/99"
                              WHEN FIRST-OF(tt-report.key-04)
                                OR tb_summary
          w-data.w-ord-no     WHEN NOT tb_summary
          w-data.w-inv-no
          w-qty
            (ACCUMULATE TOTAL BY tt-report.key-04 w-qty)
                              WHEN tb_summary @ w-qty 
          w-sq-ft
            (ACCUMULATE TOTAL BY tt-report.key-04 w-sq-ft)
                              WHEN tb_summary @ w-sq-ft 
          w-tot-sf-sht
            (ACCUMULATE TOTAL BY tt-report.key-04 w-tot-sf-sht)
                              WHEN tb_summary @ w-tot-sf-sht 
          w-tot-msf 
            (ACCUMULATE TOTAL BY tt-report.key-04 w-tot-msf)
                              WHEN tb_summary @ w-tot-msf           
          w-procat            WHEN NOT tb_summary
          w-amt
            (ACCUMULATE TOTAL BY tt-report.key-04 w-amt)
                              WHEN tb_summary @ w-amt
          WITH FRAME itemx.
        DOWN WITH FRAME itemx.
       END.

       IF tb_excel THEN  
            PUT STREAM excel UNFORMATTED
               '"' w-data.w-cust-no '",' 
               '"' cust.name '",'
               '"' w-data.w-bol-no '",'
               '"' cust.cr-rating '",'
               '"' w-data.w-inv-date '",' 
               '"' w-data.w-ord-no '",'
               '"' w-data.w-inv-no '",' 
               '"' (IF tb_summary THEN (ACCUMULATE TOTAL BY tt-report.key-04 w-qty)
                    ELSE w-qty) '",' 
               '"' (IF tb_summary THEN (ACCUMULATE TOTAL BY tt-report.key-04 w-sq-ft)
                    ELSE w-sq-ft) '",'
               '"' (IF tb_summary THEN (ACCUMULATE TOTAL BY tt-report.key-04 w-tot-sf-sht)
                    ELSE w-tot-sf-sht) '",'
               '"' (IF tb_summary THEN (ACCUMULATE TOTAL BY tt-report.key-04 w-tot-msf)
                    ELSE w-tot-msf) '",'
               '"' w-procat '",'
               '"' (IF tb_summary THEN (ACCUMULATE TOTAL BY tt-report.key-04 w-amt)
                    ELSE w-amt) '"'     

               SKIP. 
      END. 

      if last-of(tt-report.key-01) then do:
        assign cust-qty = (accumulate total by tt-report.key-01 w-qty)
               cust-sf-sht =
                (accumulate total by tt-report.key-01 w-tot-sf-sht)
               cust-amt = (accumulate total by tt-report.key-01 w-amt).
        put skip.

        IF tb_prt-bol THEN DO:
          underline cust.name w-qty /* w-sq-ft */ w-tot-sf-sht w-amt
                  with frame itemxb.
          display "               CUSTOMER TOTALS" @ cust.name
              cust-qty    @ w-qty
              cust-sf-sht @ w-tot-sf-sht
              cust-amt    @ w-amt
              with frame itemxb.
        END.

        ELSE DO:
          underline cust.name w-qty /* w-sq-ft */ w-tot-sf-sht w-amt
                  with frame itemx.
          display "               CUSTOMER TOTALS" @ cust.name
              cust-qty    @ w-qty
              cust-sf-sht @ w-tot-sf-sht
              cust-amt    @ w-amt
              with frame itemx.
        END.
      end.

      if tb_cred AND last-of(tt-report.cred) then do:
        assign cust-qty = (accumulate total by tt-report.cred w-qty)
               cust-sf-sht =
                (accumulate total by tt-report.cred w-tot-sf-sht)
               cust-amt = (accumulate total by tt-report.cred w-amt).
        put skip.

        IF tb_prt-bol THEN DO:
          underline cust.name w-qty /* w-sq-ft */ w-tot-sf-sht w-amt
                  with frame itemxb.
          display "          CREDIT RATING TOTALS" @ cust.name
              cust-qty    @ w-qty
              cust-sf-sht @ w-tot-sf-sht
              cust-amt    @ w-amt
              with frame itemxb.
        END.

        ELSE DO:
          underline cust.name w-qty /* w-sq-ft */ w-tot-sf-sht w-amt
                  with frame itemx.
          display "          CREDIT RATING TOTALS" @ cust.name
              cust-qty    @ w-qty
              cust-sf-sht @ w-tot-sf-sht
              cust-amt    @ w-amt
              with frame itemx.
        END.
      end.

      assign
        tot-qty = tot-qty + w-qty
        /* tot-sq-ft     = tot-sq-ft     + w-sq-ft */
        tot-sf-sht = tot-sf-sht + w-tot-sf-sht
        tot-amt  = tot-amt  + w-amt.

      delete w-data.
      delete tt-report.
    end.  /* for each tt-report... */

    /* display final totals */
    put skip(2).

    IF tb_prt-bol THEN DO:
      underline cust.name w-qty /* w-sq-ft */ w-tot-sf-sht w-amt
        with frame itemxb.
      display "                  GRAND TOTALS"      @ cust.name
        tot-qty  @ w-qty
        /* tot-sq-ft      @ w-sq-ft */
        tot-sf-sht  @ w-tot-sf-sht
        tot-amt   @ w-amt
        with frame itemxb.
    END.

    ELSE DO:
      underline cust.name w-qty /* w-sq-ft */ w-tot-sf-sht w-amt
        with frame itemx.
      display "                  GRAND TOTALS"      @ cust.name
        tot-qty  @ w-qty
        /* tot-sq-ft      @ w-sq-ft */
        tot-sf-sht  @ w-tot-sf-sht
        tot-amt   @ w-amt
        with frame itemx.
    END.

  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

  IF tb_excel THEN DO:
     OUTPUT STREAM excel CLOSE.
     IF tb_runExcel THEN
         OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
 END.

  SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetCustRange C-Win 
PROCEDURE SetCustRange :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iplChecked AS LOGICAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
        begin_cust-no:SENSITIVE = NOT iplChecked
        end_cust-no:SENSITIVE = NOT iplChecked
        begin_cust-no:VISIBLE = NOT iplChecked
        end_cust-no:VISIBLE = NOT iplChecked
        btnCustList:SENSITIVE = iplChecked
       .
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

