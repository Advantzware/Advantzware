&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: aprep\r-venlst.w

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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_vend end_vend begin_type ~
end_type begin_buyer end_buyer tb_detailed lv-ornt lines-per-page rd-dest ~
lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_vend end_vend begin_type end_type ~
begin_buyer end_buyer tb_detailed lv-ornt lines-per-page rd-dest lv-font-no ~
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

DEFINE VARIABLE begin_buyer AS CHARACTER FORMAT "XXX":U 
     LABEL "Beginning Buyer" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE begin_type AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning  Vendor Type" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE end_buyer AS CHARACTER FORMAT "XXX":U INITIAL "zzz" 
     LABEL "Ending Buyer" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE end_type AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Vendor Type" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-venlst.csv" 
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
"To File", 3
     SIZE 23 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 7.14.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 7.86.

DEFINE VARIABLE tb_detailed AS LOGICAL INITIAL no 
     LABEL "Detailed?" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY 1 NO-UNDO.

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
     begin_vend AT ROW 2.91 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Vendor Number"
     end_vend AT ROW 2.91 COL 67 COLON-ALIGNED HELP
          "Enter Ending Vendor Number"
     begin_type AT ROW 4.1 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Type"
     end_type AT ROW 4.1 COL 67 COLON-ALIGNED HELP
          "Enter Ending Type"
     begin_buyer AT ROW 5.29 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Buyer"
     end_buyer AT ROW 5.29 COL 67 COLON-ALIGNED HELP
          "Enter Ending Buyer"
     tb_detailed AT ROW 6.71 COL 39
     lv-ornt AT ROW 9.24 COL 30 NO-LABEL
     lines-per-page AT ROW 9.24 COL 83 COLON-ALIGNED
     rd-dest AT ROW 9.95 COL 6 NO-LABEL
     lv-font-no AT ROW 10.91 COL 33 COLON-ALIGNED
     lv-font-name AT ROW 11.86 COL 27 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 12.81 COL 29.2
     tb_excel AT ROW 13.76 COL 49.2 RIGHT-ALIGNED
     tb_runExcel AT ROW 13.76 COL 70.2 RIGHT-ALIGNED
     fi_file AT ROW 14.62 COL 27.2 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 16.14 COL 19
     btn-cancel AT ROW 16.14 COL 58
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 9.24 COL 4
     RECT-6 AT ROW 8.86 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 95.4 BY 16.57.


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
         TITLE              = "Vendor Master List"
         HEIGHT             = 16.67
         WIDTH              = 95.6
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
       begin_buyer:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_type:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_buyer:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_type:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_detailed:PRIVATE-DATA IN FRAME FRAME-A     = 
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

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Vendor Master List */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Vendor Master List */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_buyer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_buyer C-Win
ON LEAVE OF begin_buyer IN FRAME FRAME-A /* Beginning Buyer */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_type C-Win
ON LEAVE OF begin_type IN FRAME FRAME-A /* Beginning  Vendor Type */
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
  end case. 

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_buyer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_buyer C-Win
ON LEAVE OF end_buyer IN FRAME FRAME-A /* Ending Buyer */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_type C-Win
ON LEAVE OF end_type IN FRAME FRAME-A /* Ending Vendor Type */
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


&Scoped-define SELF-NAME tb_detailed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_detailed C-Win
ON VALUE-CHANGED OF tb_detailed IN FRAME FRAME-A /* Detailed? */
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
  IF access-close THEN DO:
    APPLY "close" TO THIS-PROCEDURE.
    RETURN.
  END.

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    APPLY "entry" TO begin_vend.
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
  DISPLAY begin_vend end_vend begin_type end_type begin_buyer end_buyer 
          tb_detailed lv-ornt lines-per-page rd-dest lv-font-no lv-font-name 
          td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_vend end_vend begin_type end_type begin_buyer 
         end_buyer tb_detailed lv-ornt lines-per-page rd-dest lv-font-no 
         td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
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
/*      DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.      */
/*                                                         */
/*      if init-dir = "" then init-dir = "c:\temp" .       */
/*      SYSTEM-DIALOG GET-FILE list-name                   */
/*          TITLE      "Enter Listing Name to SAVE AS ..." */
/*          FILTERS    "Listing Files (*.rpt)" "*.rpt",    */
/*                     "All Files (*.*)" "*.*"             */
/*          INITIAL-DIR init-dir                           */
/*          ASK-OVERWRITE                                  */
/*     /*     CREATE-TEST-FILE*/                           */
/*          SAVE-AS                                        */
/*          USE-FILENAME                                   */
/*                                                         */
/*          UPDATE OKpressed.                              */
/*                                                         */
/*      IF NOT OKpressed THEN  RETURN NO-APPLY.            */
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
  run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* --------------------------------------------------- ap/vendlist.p 11/94 RM */
/* Vendor Master List Report Program - A/P Module                             */
/* -------------------------------------------------------------------------- */

{sys/form/r-top.f}

def var fco as ch.
def var tco like fco.
def var fvend as ch.
def var tvend like fvend initial "ZZZZZZZZ".
def var ftype as ch.
def var ttype like ftype initial "ZZZZZZZZ".
def var fbuy like vend.buyer.
def var tbuy like vend.buyer initial "ZZZ".
def var detailed as logical format "Yes/No" initial no.
DEF VAR excelheader AS CHAR NO-UNDO.

form
   skip(1)
   "Vendor:" at 1
   vend.vend-no at 9 skip
   vend.name at 9
   "Type:" to 52 vend.type
   "Active ?:" to 73 vend.active skip
   vend.add1 at 9
   "Contact:" to 52 vend.contact skip
   vend.add2 at 9
   "Telephone:" to 52 vend.area-code vend.phone skip
   vend.city at 9
   "Fax:" to 52 vend.fax-area vend.fax skip
   vend.country at 9
   vend.postal at 21 
   "Default GL#:" to 52 vend.actnum skip
   with frame vend2 overlay no-labels stream-io width 80 down.

form
   skip(1)
   "Vendor:" at 1
   vend.vend-no at 9 skip
   vend.name at 9
   "Type:" to 52 vend.type
   "Active ?:" to 73 vend.active skip
   vend.add1 at 9
   "Contact:" to 52 vend.contact skip
   vend.add2 at 9
   "Telephone:" to 52 vend.area-code vend.phone skip
   vend.city at 9
   "Fax:" to 52 vend.fax-area vend.fax skip
   vend.country at 9
   vend.postal at 21 
   "Default GL#:" to 52 vend.actnum skip(1)
   "Buyer Code:" to 14 vend.buyer
   "Company Code:" to 52 vend.company skip
   "Buyer Name:" to 14 vend.buyer-n format "x(20)"
   "Carrier :" to 52 vend.carrier skip
   "Terms Code:" to 14 vend.terms  terms.dscr format "x(15)"
   "FOB Code:" to 52 vend.fob-code skip
   "Discount %:" to 14 vend.disc-%
   "1099 Code:" to 52 vend.code-1099 skip
   "Discount Days:" to 14 vend.disc-days
   "Date Last Paid:" to 52 vend.lpay-date skip
   skip(1)
   with frame vend overlay no-labels stream-io width 80 down.


assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 56}

 fco      = cocode
 tco      = cocode
 fvend    = begin_vend
 tvend    = end_vend
 ftype    = begin_type
 ttype    = end_type
 fbuy     = begin_buyer
 tbuy     = end_buyer
 detailed = tb_detailed. 

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "Vendor,Name,Type,Active,Contact,Address 1,Address 2,City,State,"
              + "Zip,Country,Postal Code,Telephone,Fax,Default GL#,".
  IF detailed THEN
     excelheader = excelheader
                 + "Buyer Code,Company Code,Buyer Name,Carrier,FOB Code,"
                 + "Terms Code,Terms Description,Discount %,1099 Code,Discount Days,Date Last Paid".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

if td-show-parm then run show-param.

  display "" with frame r-top.

  for each vend where (vend.company >= fco and vend.company <= tco)     and
                          (vend.vend-no >= fvend and vend.vend-no <= tvend) and
                          (vend.type >= ftype and vend.type <= ttype) and
                          (vend.buyer >= fbuy and vend.buyer <= tbuy)
                          NO-LOCK
                          break by vend.vend-no with frame vend2:

      {custom/statusMsg.i " 'Processing Vendor#  '  + string(vend.vend-no) "}

         if detailed then do with frame vend:


            find first terms where terms.t-code = vend.terms no-lock no-error.

            display
                  vend.vend-no
                  vend.name
                  vend.add1
                  vend.add2
                  vend.city + ", " + vend.state + " " + vend.zip
                       format "x(30)" @ vend.city
                  vend.type
                  vend.active
                  vend.contact
                  vend.area-code
                  vend.phone
                  vend.fax-area
                  vend.fax
                  vend.country
                  vend.postal
                  vend.actnum
                  vend.buyer
                  vend.company
                  vend.buyer-n
                  vend.carrier
                  vend.fob-code
                  vend.terms
                  "" @ terms.dscr
                  terms.dscr when available terms
                  vend.code-1099
                  vend.disc-%
                  vend.lpay-date
                  vend.disc-days
                  vend.actnum.
            down.

            IF tb_excel THEN
               PUT STREAM excel UNFORMATTED
                   '"' vend.vend-no                        '",'
                   '"' vend.NAME                           '",'
                   '"' vend.TYPE                           '",'
                   '"' vend.active                         '",'              
                   '"' vend.contact                        '",'
                   '"' vend.add1                           '",'
                   '"' vend.add2                           '",'
                   '"' vend.city                           '",'
                   '"' vend.state                          '",'
                   '"' vend.zip                            '",'
                   '"' vend.country                        '",'
                   '"' vend.postal                         '",'
                   '"' STRING(vend.area-code,"(999)") +
                       " " + STRING(vend.phone,"999-9999") '",'
                   '"' STRING(vend.fax-area,"(999)") +
                       " " STRING(vend.fax,"999-9999")     '",'
                   '"' vend.actnum                         '",'
                   '"' vend.buyer                          '",'
                   '"' vend.company                        '",'
                   '"' vend.buyer-n                        '",'
                   '"' vend.carrier                        '",'
                   '"' vend.fob-code                       '",'
                   '"' vend.terms                          '",'
                   '"' IF AVAIL terms THEN terms.dscr
                       ELSE ""                             '",'
                   '"' STRING(vend.disc-%,">>9.99%")       '",'
                   '"' vend.code-1099                      '",'
                   '"' vend.disc-days                      '",'
                   '"' IF vend.lpay-date NE ? THEN
                          STRING(vend.lpay-date) ELSE ""   '",'
                   SKIP.
         end.

         else
         DO:
            display 
                    vend.vend-no
                    vend.name
                    vend.type
                    vend.active
                    vend.add1
                    vend.contact
                    vend.add2
                    vend.area-code
                    vend.phone
                    vend.city + ", " + vend.state + " " + vend.zip
                       format "x(30)" @ vend.city
                    vend.fax-area
                    vend.fax
                    vend.country
                    vend.postal
                    vend.actnum.

            IF tb_excel THEN
               PUT STREAM excel UNFORMATTED
                   '"' vend.vend-no                        '",'
                   '"' vend.NAME                           '",'
                   '"' vend.TYPE                           '",'
                   '"' vend.active                         '",'
                   '"' vend.contact                        '",'
                   '"' vend.add1                           '",'
                   '"' vend.add2                           '",'
                   '"' vend.city                           '",'
                   '"' vend.state                          '",'
                   '"' vend.zip                            '",'
                   '"' vend.country                        '",'
                   '"' vend.postal                         '",'
                   '"' STRING(vend.area-code,"(999)") +
                       " " + STRING(vend.phone,"999-9999") '",'
                   '"' STRING(vend.fax-area,"(999)") +
                       " " + STRING(vend.fax,"999-9999")   '",'
                   '"' vend.actnum                         '",'
                   SKIP.
         END.
         down.

      end.

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

