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
def var v-types as char format "x(10)" NO-UNDO.
assign
 cocode = gcompany
 locode = gloc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 RECT-6 begin_date end_date begin_loc ~
end_loc tb_sort t-receipt t-issue t-trans t-adj t-phy tb_total rd-dest ~
lv-ornt lines-per-page lv-font-no td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date begin_loc end_loc ~
tb_sort t-receipt t-issue t-trans t-adj t-phy tb_total rd-dest lv-ornt ~
lines-per-page lv-font-no lv-font-name td-show-parm 

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U 
     LABEL "Begining Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_loc AS CHARACTER FORMAT "X(8)":U 
     LABEL "Begining Location" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_loc AS CHARACTER FORMAT "X(8)":U 
     LABEL "Ending Location" 
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
"To File", 3
     SIZE 23 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 12.86.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 6.19.

DEFINE VARIABLE t-adj AS LOGICAL INITIAL no 
     LABEL "Adjustments" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE t-issue AS LOGICAL INITIAL no 
     LABEL "Shipments" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE t-phy AS LOGICAL INITIAL no 
     LABEL "Credit Returns" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE t-receipt AS LOGICAL INITIAL no 
     LABEL "Receipts" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE t-trans AS LOGICAL INITIAL no 
     LABEL "Transfers" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_sort AS LOGICAL INITIAL no 
     LABEL "Sort By Job?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE tb_total AS LOGICAL INITIAL no 
     LABEL "Show Totals?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_date AT ROW 3.14 COL 22 COLON-ALIGNED
     end_date AT ROW 3.14 COL 63 COLON-ALIGNED
     begin_loc AT ROW 4.33 COL 22 COLON-ALIGNED
     end_loc AT ROW 4.33 COL 63 COLON-ALIGNED
     tb_sort AT ROW 5.76 COL 38
     t-receipt AT ROW 7.43 COL 37
     t-issue AT ROW 8.38 COL 37
     t-trans AT ROW 9.33 COL 37
     t-adj AT ROW 10.05 COL 37
     t-phy AT ROW 11 COL 37
     tb_total AT ROW 12.19 COL 38
     rd-dest AT ROW 15.29 COL 5 NO-LABEL
     lv-ornt AT ROW 15.52 COL 32 NO-LABEL
     lines-per-page AT ROW 15.52 COL 85 COLON-ALIGNED
     lv-font-no AT ROW 16.95 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 17.91 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 19.33 COL 5
     btn-ok AT ROW 21 COL 19
     btn-cancel AT ROW 21 COL 57
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 14.57 COL 3
     "Transaction Types" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 6.71 COL 24
          FONT 6
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-17 AT ROW 1 COL 1
     RECT-6 AT ROW 14.33 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 22.76.


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
         TITLE              = "Finished Goods Edit List"
         HEIGHT             = 23.05
         WIDTH              = 96.2
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
   FRAME-NAME                                                           */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Finished Goods Edit List */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Finished Goods Edit List */
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

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case. 

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


&Scoped-define SELF-NAME t-adj
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-adj C-Win
ON VALUE-CHANGED OF t-adj IN FRAME FRAME-A /* Adjustments */
DO:
      assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-issue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-issue C-Win
ON VALUE-CHANGED OF t-issue IN FRAME FRAME-A /* Shipments */
DO:
      assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-receipt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-receipt C-Win
ON VALUE-CHANGED OF t-receipt IN FRAME FRAME-A /* Receipts */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-trans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-trans C-Win
ON VALUE-CHANGED OF t-trans IN FRAME FRAME-A /* Transfers */
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

  ASSIGN
      begin_date  = TODAY
      END_date    = TODAY
      END_loc = "zzzzzzzz".

  /*for each fg-rctd where fg-rctd.company                  eq cocode
                     and fg-rctd.rita-code                ne "C"
                      no-lock
                      break by fg-rctd.rita-code:
      if first-of(fg-rctd.rita-code) then v-types = v-types + fg-rctd.rita-code.
  end.
  IF INDEX(v-types,"R") > 0 THEN t-receipt = YES.
  IF index(v-types,"S") > 0 THEN t-issue  = YES.
  IF INDEX(v-types,"T") > 0 THEN t-trans  = YES.
  IF index(v-types,"A") > 0 THEN t-adj = YES.
  IF index(v-types,"E") > 0 THEN t-phy    = YES.*/

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO begin_date.
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
  DISPLAY begin_date end_date begin_loc end_loc tb_sort t-receipt t-issue 
          t-trans t-adj t-phy tb_total rd-dest lv-ornt lines-per-page lv-font-no 
          lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 RECT-6 begin_date end_date begin_loc end_loc tb_sort t-receipt 
         t-issue t-trans t-adj t-phy tb_total rd-dest lv-ornt lines-per-page 
         lv-font-no td-show-parm btn-ok btn-cancel 
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
/*      DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.       */
/*                                                          */
/*      if init-dir = "" then init-dir = "c:\temp" .        */
/*      SYSTEM-DIALOG GET-FILE list-name                    */
/*          TITLE      "Enter Listing Name to SAVE AS ..."  */
/*          FILTERS    "Listing Files (*.rpt)" "*.rpt",     */
/*                     "All Files (*.*)" "*.*"              */
/*          INITIAL-DIR init-dir                            */
/*          ASK-OVERWRITE                                   */
/*     /*     CREATE-TEST-FILE*/                            */
/*          SAVE-AS                                         */
/*          USE-FILENAME                                    */
/*                                                          */
/*          UPDATE OKpressed.                               */
/*                                                          */
/*      IF NOT OKpressed THEN  RETURN NO-APPLY.             */

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
/* ------------------------------------------------ fg/rep/fg-edlst.p 9/91 cd */
/* finish goods transactions edit list                                        */
/* -------------------------------------------------------------------------- */
{sys/form/r-topw.f}

def var fdate like fg-rctd.rct-date format "99/99/9999" init TODAY NO-UNDO.
def var tdate like fdate init today NO-UNDO.
def var floc as char init "" NO-UNDO.
def var tloc like floc init "zzzzzzzzz" NO-UNDO.
def var v-sort as log init no NO-UNDO.
DEF VAR v-trans-time AS CHAR NO-UNDO.

def var v-fg-qty as dec format "->>,>>>,>>9.99<" NO-UNDO.
def var v-fg-value as dec format "->,>>>,>>9.99" NO-UNDO.
def var v-tran-type as char format "x(1)" NO-UNDO. 
def var v-totadj as dec format "->>>,>>>,>>9" NO-UNDO.
def var v-grd-totadj as dec format "->>>,>>>,>>9" NO-UNDO. 
def var v-price as dec format "->>>>>>>9.9999999" NO-UNDO.
def var v-cum-tot as de format "->>,>>>,>>9.99" NO-UNDO.
def var v-one as char format "x(1)" init "1" NO-UNDO.
def var v-tot-qty like fg-rctd.t-qty NO-UNDO.

def var v-whse like fg-rctd.loc NO-UNDO.                                        
def var v-pr-tots as log format "Y/N" init false no-undo. 


form header                                                                                           
  "ITEM" at 2 "DESCRIPTION" at 17 "FROM" at 70 "QTY" at 108 "SALES" at 131
   skip
  "DATE" at 4 "TIME" AT 15   "TY" at 20 "TAG #" at 23 "JOB #" at 45 
  "P.O. #" at 54 "VENDOR" at 61                                   
    "BIN" at 70  "CASES" at 81
  "QTY/CASE" at 88  "UOM" at 97 "TOTAL" at 108 "COST" AT 117 "VALUE" at 131
 fill("=",135) format "x(135)"  
with frame f-top PAGE-TOP no-box no-labels STREAM-IO width 135.

       form
           fg-rctd.rct-date at 4 space(1)
           v-trans-time      FORMAT "x(5)"
           v-tran-type
           fg-rctd.tag       FORMAT "x(20)" /* gdm - 12090821 */
           fg-rctd.job-no space(0) "-" space(0)
           fg-rctd.job-no2 format "99" space(2)
           fg-rctd.po-no        format "x(6)"  at 55                                                            
           po-ord.vend-no       at 61
           fg-rctd.loc-bin      at 70       
           fg-rctd.cases        to 85
           fg-rctd.qty-case     to 95
           fg-rctd.pur-uom     format "x(3)" at 97
           v-tot-qty            format "->>>,>>>,>>9"  at 101
       /*
           fg-rctd.t-qty        format "->>>,>>>,>>9"  at 99
       */
           fg-rctd.std-cost         AT 114 FORMAT "->>,>>9"
           v-fg-value           format "->,>>>,>>9.99" to 135
       with frame detail no-box no-labels down STREAM-IO width 135.

       form
           fg-rctd.rct-date at 4 space(1)
           v-trans-time      FORMAT "x(5)"
           v-tran-type
           fg-rctd.tag       FORMAT "x(20)" /* gdm - 12090821 */
           fg-rctd.job-no space(0) "-" space(0)
           fg-rctd.job-no2 format "99" space(2)
           fg-rctd.po-no        format "x(6)" at 55                                                            
           po-ord.vend-no       at 61                                                              
           fg-rctd.loc-bin      at 70  
           v-one                to 85
           fg-rctd.partial      to 95
           fg-rctd.pur-uom     format "x(3)" at 97                                                                    
           v-tot-qty            format "->>>,>>>,>>9"  at 101
       /*
           fg-rctd.t-qty        format "->>>,>>>,>>9"  at 99
       */ 
           fg-rctd.std-cost         AT 114 FORMAT "->>,>>9"
           v-fg-value           format "->,>>>,>>9.99" to 135
       with frame pdetail no-box no-labels down STREAM-IO width 135.

ASSIGN str-tit2 = c-win:title
       {sys/inc/ctrtext.i str-tit2 112} 
       fdate     = begin_date
       tdate     = end_date
       floc      = begin_loc
       tloc      = end_loc
       v-types   = (if t-receipt then "R" else "") +
                    (if t-issue then "S" else "") +
                    (if t-trans then "T" else "") +
                    (if t-adj then "A" else "") +
                    (if t-phy then "E" else "")                                 
       v-sort    = tb_sort              
       v-pr-tots = tb_total.

{sa/sa-sls01.i}

{sys/inc/print1.i}

SESSION:SET-WAIT-STATE ("general").

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

display "" with frame r-top.

    for each fg-rctd
        where fg-rctd.company                  eq cocode
          and fg-rctd.rct-date               ge fdate
          and fg-rctd.rct-date               le tdate
          and index(v-types,fg-rctd.rita-code) gt 0
          and fg-rctd.rita-code                ne "C"
          AND fg-rctd.loc GE floc 
          AND fg-rctd.loc LE tloc  NO-LOCK :
      create report.
      assign
       report.term-id = v-term
       report.key-01  = if v-sort then
                          fill(" ",6 - length(trim(fg-rctd.job-no))) +
                          trim(fg-rctd.job-no) + string(fg-rctd.job-no2,"99")
                        else fg-rctd.loc                                                               
       report.key-02  = fg-rctd.i-no
       report.key-03  = string( year(fg-rctd.rct-date),"9999") +
                        string(month(fg-rctd.rct-date),"99")   +
                        string(  day(fg-rctd.rct-date),"99")
       /*report.key-04  = string(recid(fg-rctd))*/
       report.rec-id  = recid(fg-rctd).
    end.

    for each report    where report.term-id  eq v-term,
        first fg-rctd where recid(fg-rctd) eq report.rec-id no-lock
        break by report.key-01
              by report.key-02
              by report.key-03
              by fg-rctd.r-no:

      /* if v-sort and first-of(report.key-01) then page. */                                /* begin ekwtest */
      if first-of(report.key-01) then do:
         assign v-whse = fg-rctd.loc.
        /* if first(report.key-01) then do:
            hide frame r-top.
            view frame r-top.
            page.
         end. /* if first(report.key-01) */

         else 
         */
            put skip(3) "WHSE:" v-whse skip.
            IF FIRST(report.key-01) THEN DISP WITH FRAME f-top.
       end.   /* if first-of(report.key-01) */                                                     /* end ekwtest */


      if first-of(report.key-02) then do:
        find first itemfg
            where itemfg.company eq fg-rctd.company
              and itemfg.i-no    eq fg-rctd.i-no
            no-lock no-error.
        if avail itemfg then do:
          find first uom
              where uom.uom  eq itemfg.sell-uom
                and uom.mult ne 0
              no-lock no-error.
            v-price = if itemfg.sell-uom eq "L" then itemfg.sell-price
                      else (itemfg.sell-price /
                            if avail uom then uom.mult else 1000).
            if v-price lt 0 then v-price = 0.
        end.

        /*djk*/
        put fg-rctd.i-no FORMAT "x(16)" 
            fg-rctd.i-name space(21)
/*
            fg-rctd.cases          format "->>>>9" to 75 /*space(2)*/
           fg-rctd.qty-case       format "->>>>9" to 85 /*space(4)*/
*/          

            /*"SELLING PRICE:"*/  "" at 122
            itemfg.sell-price itemfg.sell-uom skip.
      end.

      assign v-fg-value = fg-rctd.t-qty * v-price.

      if fg-rctd.rita-code eq "R" then
        assign
         v-totadj    = v-totadj + fg-rctd.t-qty
         v-cum-tot   = v-cum-tot + v-fg-value
         v-tran-type = "REC".

      else
      if fg-rctd.rita-code eq "T" then v-tran-type = "TRAN".

      else
      if fg-rctd.rita-code eq "A" then
        assign
         v-totadj    = v-totadj + fg-rctd.t-qty
         v-cum-tot   = v-cum-tot + v-fg-value
         v-tran-type = "ADJ".

      else
      if fg-rctd.rita-code eq "S" then
        assign
         v-totadj    = v-totadj - fg-rctd.t-qty
         v-cum-tot   = v-cum-tot - v-fg-value
         v-tran-type = "SHIP".

      else
      if fg-rctd.rita-code eq "E" then
        assign
         v-totadj    = v-totadj + fg-rctd.t-qty
         v-cum-tot   = v-cum-tot + v-fg-value
         v-tran-type = "CRED".

      else
        assign
         v-totadj    = v-totadj + fg-rctd.t-qty
         v-tran-type = "UNKN".

      if line-counter gt 56 then page.

      if fg-rctd.po-no <> " " then
        find po-ord where po-ord.po-no = int(fg-rctd.po-no) NO-LOCK NO-ERROR.
      ASSIGN
         v-tot-qty  = fg-rctd.cases * fg-rctd.qty-case
         v-fg-value = v-tot-qty * v-price
         v-trans-time = STRING(fg-rctd.trans-time, "HH:MM").

      display fg-rctd.rct-date when first-of(report.key-03)  
              v-trans-time     when first-of(report.key-03) 
              v-tran-type 
              fg-rctd.tag       FORMAT "x(20)" /* gdm - 12090821 */
              fg-rctd.job-no
              fg-rctd.job-no2
              fg-rctd.po-no
              po-ord.vend-no                     when avail po-ord
              fg-rctd.loc-bin 

              fg-rctd.cases        format "->>,>>9"

              fg-rctd.qty-case
              fg-rctd.pur-uom 
                      v-tot-qty  
              fg-rctd.std-cost    FORMAT ">>>,>>9"
              v-fg-value 

              with frame detail.                                                       
      down 2 with frame detail.

      if fg-rctd.partial <> 0 then do:
         ASSIGN
            v-tot-qty  = fg-rctd.partial
            v-fg-value = v-tot-qty * v-price
            v-trans-time = STRING(fg-rctd.trans-time, "HH:MM").

        display fg-rctd.rct-date when first-of(report.key-03) 
               v-trans-time       when first-of(report.key-03) 
              v-tran-type 
              fg-rctd.tag       FORMAT "x(20)" /* gdm - 12090821 */
              fg-rctd.job-no
              fg-rctd.job-no2
              fg-rctd.po-no
              po-ord.vend-no                     when avail po-ord
              fg-rctd.loc-bin  
             v-one
             fg-rctd.partial FORM "->>,>>9"
              fg-rctd.pur-uom 
/*            
              fg-rctd.t-qty 
*/
              v-tot-qty   
              fg-rctd.std-cost    FORMAT ">>>,>>9"
              v-fg-value

              with frame pdetail.                                                       
        down with frame pdetail.
      end.

      if v-tran-type begins "T" then
         put "TO: " at 66 fg-rctd.loc2 fg-rctd.loc-bin2 skip(1).

      if last-of(report.key-02) and v-pr-tots then do:                                             
        put "Total Adjustment: " at 83 v-totadj skip.                         
        v-grd-totadj = v-grd-totadj + v-totadj.                                   
        v-totadj = 0.
      end.

      if last(fg-rctd.r-no) then delete report.
    end. /* each fg-rctd */

    if v-pr-tots then                                                                         
    put skip "--------------" to 115 skip "Grand Total Adjustment: " to 100 v-grd-totadj  skip. 

   OUTPUT CLOSE.

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

