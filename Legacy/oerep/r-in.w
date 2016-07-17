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

DEF VAR lines-per-page AS INT NO-UNDO.

DEF var save_id AS RECID.

DEF var time_stamp AS ch.
ASSIGN time_stamp = string(TIME, "hh:mmam").

DEF var v-ford-no AS int FORMAT ">>>>>>" extent 2 no-undo.
def var v-orders as char format "x(78)" extent 10.
DEF var v-fitem AS char FORMAT "x(15)" extent 2 init ["","zzzzzzzzzzzzzzz"].
DEF var v-po-no-source AS char FORMAT "!" init "R".
def var v-stat as char format "!" init "O".

DEF var v-out AS char FORMAT "x(40)" NO-UNDO.
DEF var v-job AS char FORMAT "x(9)" NO-UNDO.
DEF var num-rec AS int init 0 NO-UNDO.
DEF var by-release AS log init NO NO-UNDO.

/* 9812 CAH: */
DEF var v-loadtag       AS char NO-UNDO initial "ASI".  /* sys ctrl option */
DEF var v-mult          AS int  NO-UNDO initial 0.  /* sys ctrl option */
DEF var v-count         AS int  NO-UNDO initial 0.

/* 9812 CAH: Variables for Intermec Support */
def var stx as char format 'x(01)' no-undo initial "~002".
def var etx as char format 'x(01)' no-undo initial "~003".
def var esc as char format 'x(01)' no-undo initial "~033".
def var etb as char format 'x(01)' no-undo initial "~027".
def var cr  as char format 'x(01)' no-undo initial "~015".
def var can as char format 'x(01)' no-undo initial "~030".
def var rs  as char format 'x(01)' no-undo initial "~036".
def var us  as char format 'x(01)' no-undo initial "~037".

def stream s-form.
def stream s-bar.

def var form_fid        as char no-undo initial "barcode.frm" FORMAT "X(40)".
def var form#           as int  no-undo format "9" initial 3.
def var char_units      as char no-undo.
def var copy_count      as int no-undo initial 2.
def var n               as int no-undo initial 0.


def workfile w-file field w-key like oe-ord.ord-no.

{oerep\r-loadtg.i NEW}

ASSIGN  
  tmpstore = FILL("_",50).


{sys/form/r-top3.f}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS v-ord-list begin_ord-no end_ord-no ~
begin_i-no end_i-no tb_rel rd_print rd_order-sts begin_form begin_labels ~
begin_filename btn-ok btn-cancel RECT-7 
&Scoped-Define DISPLAYED-OBJECTS v-ord-list begin_ord-no end_ord-no ~
begin_i-no end_i-no tb_rel rd_print rd_order-sts begin_form begin_labels ~
begin_filename lbl_rel lbl_po-no 

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

DEFINE VARIABLE v-ord-list AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 84 BY 5.24 NO-UNDO.

DEFINE VARIABLE begin_filename AS CHARACTER FORMAT "X(256)":U 
     LABEL "Form File" 
     VIEW-AS FILL-IN 
     SIZE 71 BY 1 NO-UNDO.

DEFINE VARIABLE begin_form AS INTEGER FORMAT ">>>":U INITIAL 3 
     LABEL "Printer Form#" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "From Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_labels AS INTEGER FORMAT ">>>>":U INITIAL 2 
     LABEL "# of Labels/Pallet" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "From Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "To Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "To Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_po-no AS CHARACTER FORMAT "X(256)":U INITIAL "Print PO from?" 
      VIEW-AS TEXT 
     SIZE 15 BY .62 NO-UNDO.

DEFINE VARIABLE lbl_rel AS CHARACTER FORMAT "X(256)":U INITIAL "Print by Release?" 
      VIEW-AS TEXT 
     SIZE 18 BY .62 NO-UNDO.

DEFINE VARIABLE rd_order-sts AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Open", "O",
"Close", "C",
"All", "A"
     SIZE 37 BY .95 NO-UNDO.

DEFINE VARIABLE rd_print AS CHARACTER INITIAL "Release" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Header", "Header",
"Line", "Line",
"Release", "Release"
     SIZE 35 BY .71 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 17.38.

DEFINE VARIABLE tb_rel AS LOGICAL INITIAL no 
     LABEL "Print by Release?" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     v-ord-list AT ROW 3.14 COL 8 NO-LABEL
     begin_ord-no AT ROW 9.1 COL 21 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_ord-no AT ROW 9.1 COL 65 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     begin_i-no AT ROW 10.05 COL 21 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_i-no AT ROW 10.05 COL 65 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     tb_rel AT ROW 11.71 COL 40
     rd_print AT ROW 12.43 COL 40 NO-LABEL
     rd_order-sts AT ROW 13.38 COL 40 NO-LABEL
     begin_form AT ROW 14.81 COL 19 COLON-ALIGNED
     begin_labels AT ROW 15.76 COL 19 COLON-ALIGNED
     begin_filename AT ROW 16.71 COL 19 COLON-ALIGNED
     btn-ok AT ROW 19.81 COL 19
     btn-cancel AT ROW 19.81 COL 57
     lbl_rel AT ROW 11.71 COL 20 COLON-ALIGNED NO-LABEL
     lbl_po-no AT ROW 12.43 COL 23 COLON-ALIGNED HELP
          "Print Customer's PO Number from Header, Line item or Release" NO-LABEL
     RECT-7 AT ROW 1 COL 1
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 2
     "Order Status:" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 13.38 COL 26
     " Enter Order(s) seperated by comma" VIEW-AS TEXT
          SIZE 36 BY .62 AT ROW 2.43 COL 9
          BGCOLOR 14 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 105.2 BY 21.38.


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
         TITLE              = "Loadtag Creation"
         HEIGHT             = 21.81
         WIDTH              = 105.8
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
       begin_filename:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_form:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_labels:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ord-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ord-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_po-no IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_po-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_print".

/* SETTINGS FOR FILL-IN lbl_rel IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_print:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_rel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       v-ord-list:RETURN-INSERTED IN FRAME FRAME-A  = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Loadtag Creation */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Loadtag Creation */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_filename
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_filename C-Win
ON LEAVE OF begin_filename IN FRAME FRAME-A /* Form File */
DO:
  assign begin_filename.
  
  if begin_filename gt "" and lastkey ne -1 then do:
    if search(begin_filename) eq ? then do:
      message "Form file does not exist"
              view-as alert-box error.
      return no-apply.
    end.
    
    begin_filename = search(begin_filename).
    display begin_filename WITH FRAME FRAME-A IN WINDOW C-Win.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_form
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_form C-Win
ON LEAVE OF begin_form IN FRAME FRAME-A /* Printer Form# */
DO:
  assign begin_form.
  
  begin_filename = "barcode" + string(begin_form) + ".frm".
  
  display begin_filename WITH FRAME FRAME-A IN WINDOW C-Win.
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
  assign v-ord-list
         begin_ord-no
         END_ord-no
         begin_i-no
         END_i-no
         tb_rel
         rd_print
         rd_order-sts
         begin_form
         begin_labels
         begin_filename.
       
  run run-report NO-ERROR. 
  IF ERROR-STATUS:ERROR THEN RETURN.

  
  /*
  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case. 
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_rel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_rel C-Win
ON VALUE-CHANGED OF tb_rel IN FRAME FRAME-A /* Print by Release? */
DO:
  assign tb_rel.
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


  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company eq gcompany
        AND sys-ctrl.name    eq "LOADTAG"
      NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN
  DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company  = gcompany
     sys-ctrl.name     = "LOADTAG"
     sys-ctrl.descrip  = "Special Load tag print options, e.g. barcode printer"
     sys-ctrl.char-fld = "ASI".
    MESSAGE "System control record NOT found.  Please enter the load tag option"
            UPDATE sys-ctrl.char-fld.
  END.

  assign
   v-loadtag = sys-ctrl.char-fld
   v-mult    = sys-ctrl.int-fld.
 
  if v-loadtag eq "TRIAD" then begin_form = 4.

  if v-mult le 0 then v-mult = 1.
  
  RUN enable_UI.
  
  if v-loadtag ne "TRIAD" then
    DISABLE begin_form begin_labels begin_filename WITH FRAME FRAME-A.
  
  {methods/nowait.i}
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
  DISPLAY v-ord-list begin_ord-no end_ord-no begin_i-no end_i-no tb_rel rd_print 
          rd_order-sts begin_form begin_labels begin_filename lbl_rel lbl_po-no 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE v-ord-list begin_ord-no end_ord-no begin_i-no end_i-no tb_rel rd_print 
         rd_order-sts begin_form begin_labels begin_filename btn-ok btn-cancel 
         RECT-7 
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
     DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.
          
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
    
    RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).
  */  
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
  run scr-rpt.w (list-name,c-win:title). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  meters:  <none>
  Notes:       
--------------------------*/
DEF VAR i AS INT NO-UNDO.
DEF VAR choice AS LOG NO-UNDO.

assign
 v-ford-no[1]   = begin_ord-no
 v-ford-no[2]   = end_ord-no
 v-fitem[1]     = begin_i-no
 v-fitem[2]     = end_i-no
 by-release     = tb_rel
 v-po-no-source = rd_print
 form#          = begin_form
 copy_count     = begin_labels
 form_fid       = begin_filename
 v-stat = rd_order-sts.


  for each w-file:
    delete w-file.
  end.
  
  build-file:
  do i = 1 to num-entries(v-ord-list).
    
    if int(entry(i,v-ord-list)) ne 0 then do:
      create w-file.
      w-key = int(entry(i,v-ord-list)).
    end.
  end.
  
  do i = v-ford-no[1] to v-ford-no[2]:
    if i ne 0 then do:
       create w-file.
       w-key = i.
    end.
  end.

  ASSIGN
   str-tit  = coname + " - " + loname
   str-tit2 = "DOWNLOAD LOADTAG DATA"
   x = (56 - length(str-tit)) / 2
   str-tit  = FILL(" ",x) + str-tit
   x = (56 - length(str-tit2)) / 2
   str-tit2 = FILL(" ",x) + str-tit2.


  FOR EACH w-ord:
    DELETE w-ord.
  END.

  for each w-file,
      each oe-ord
      where oe-ord.company eq cocode
        and oe-ord.ord-no  eq w-key
        and (v-stat eq "A"                                    or
             (v-stat eq "C" and index("CZ",oe-ord.stat) gt 0) or
             (v-stat eq "O" and index("CZ",oe-ord.stat) eq 0))
      use-index ord-no no-lock,
      
      first cust
      where cust.company eq cocode
        and cust.cust-no eq oe-ord.cust-no
      no-lock:

    FOR EACH oe-ordl
        WHERE oe-ordl.company eq oe-ord.company
        AND oe-ordl.i-no    ge v-fitem[1]
        AND oe-ordl.i-no    le v-fitem[2]
        AND oe-ordl.ord-no  eq oe-ord.ord-no
        use-index ord-no NO-LOCK BREAK BY oe-ordl.i-no:
        
      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq oe-ordl.i-no
          no-lock no-error.
          
      IF NOT by-release THEN
      DO:
        IF FIRST-OF(oe-ordl.i-no) THEN
        DO:
          CREATE w-ord.

          ASSIGN
            w-ord.ord-no       = oe-ord.ord-no
            w-ord.job-no       = oe-ordl.job-no
            w-ord.job-no2      = oe-ordl.job-no2
            w-ord.cust-no      = oe-ord.cust-no
            w-ord.cust-name    = oe-ord.cust-name
            w-ord.i-no         = oe-ordl.i-no
            w-ord.cust-part-no = oe-ordl.part-no
            w-ord.cust-po-no   = if v-po-no-source eq "L" THEN oe-ordl.po-no
                                 else oe-ord.po-no
            w-ord.ord-qty      = oe-ordl.qty
            w-ord.i-name       = oe-ordl.i-name
            w-ord.due-date     = if oe-ord.due-date ne ? then
                                   oe-ord.due-date
                                 else
                                 if oe-ordl.req-date ne ? then
                                   oe-ordl.req-date
                                 else today
            w-ord.mult         = if cust.int-field[1] ne 0 then
                                   cust.int-field[1] else v-mult
            num-rec            = num-rec + 1.
            
          if avail itemfg then w-ord.upc-no = itemfg.upc-no.

          FIND FIRST shipto
            WHERE shipto.company eq cocode
            AND shipto.cust-no eq oe-ord.cust-no
            AND shipto.ship-id eq oe-ord.cust-no
            USE-INDEX ship-id NO-LOCK NO-ERROR.
          IF AVAIL shipto THEN
          ASSIGN
            w-ord.ship-name  = shipto.ship-name
            w-ord.ship-add1  = shipto.ship-add[1]
            w-ord.ship-add2  = shipto.ship-add[2]
            w-ord.ship-city  = shipto.ship-city
            w-ord.ship-state = shipto.ship-state
            w-ord.ship-zip   = shipto.ship-zip.

          FIND FIRST est
            WHERE est.company eq oe-ordl.company
            AND est.est-no  eq oe-ordl.est-no
            NO-LOCK NO-ERROR.
          RELEASE eb.
          IF AVAIL est THEN
          FIND FIRST eb
            WHERE eb.company = est.company
            AND eb.est-no    eq est.est-no
            AND eb.form-no  eq oe-ordl.form-no
            AND eb.blank-no eq oe-ordl.blank-no
            NO-LOCK NO-ERROR.

          IF AVAIL eb THEN
          ASSIGN
            w-ord.pcs        = eb.cas-cnt
            w-ord.bundle     = eb.cas-pal
            w-ord.total-unit = w-ord.pcs * w-ord.bundle.

          /* Add .49 to round up and add 1 for extra tag   */
          w-ord.total-tags = ((oe-ordl.qty / w-ord.total-unit) + .49) + 1.
        END.  /* first-of */
      END.  /* not by-release */

      ELSE
      FOR EACH oe-rel
          WHERE oe-rel.company eq cocode
          AND oe-rel.i-no    eq oe-ordl.i-no
          AND oe-rel.ord-no  eq oe-ordl.ord-no
          AND oe-rel.link-no ne 0 NO-LOCK:

        CREATE w-ord.
        ASSIGN
          w-ord.ord-no       = oe-ord.ord-no
          w-ord.job-no       = oe-ordl.job-no
          w-ord.job-no2      = oe-ordl.job-no2
          w-ord.cust-no      = oe-ord.cust-no
          w-ord.cust-name    = oe-ord.cust-name
          w-ord.i-no         = oe-ordl.i-no
          w-ord.cust-part-no = oe-ordl.part-no
          w-ord.cust-po-no   = IF v-po-no-source eq "L" THEN oe-ordl.po-no
          ELSE
          IF v-po-no-source eq "R" THEN oe-rel.po-no
          ELSE oe-ord.po-no
          w-ord.ord-qty      = oe-rel.qty
          w-ord.ship-add1    = oe-rel.ship-add[1]
          w-ord.ship-add2    = oe-rel.ship-add[2]
          w-ord.ship-city    = oe-rel.ship-city
          w-ord.ship-state   = oe-rel.ship-state
          w-ord.ship-zip     = oe-rel.ship-zip
          w-ord.i-name       = oe-ordl.i-name
          w-ord.due-date     = 
            (if oe-ord.due-date <> ? 
             then oe-ord.due-date
             else if oe-ordl.req-date <> ?  /* 9901 CAH */
             then oe-ordl.req-date
             else today)
          w-ord.rel-date     = oe-rel.rel-date
          w-ord.mult         = if cust.int-field[1] ne 0 then
                                 cust.int-field[1] else v-mult
          num-rec            = num-rec + 1.
          
        if avail itemfg then w-ord.upc-no = itemfg.upc-no.
          
        ASSIGN
          w-ord.pcs        = oe-rel.qty-case
          w-ord.bundle     = oe-rel.cases.
          w-ord.total-unit = w-ord.pcs * w-ord.bundle.

        /* Add .49 to round up and add 1 for extra tag   */
        ASSIGN w-ord.total-tags = ((oe-rel.qty / w-ord.total-unit) + .49) + 1.
      END.
    END.
  END.

  RUN oerep/d-loadtg.w.

  /*====== replaced with oerep/d-loadtg.w 
  /* Edit the Work Records  */
  FOR EACH w-ord:
    DISPLAY w-ord.ord-no
      w-ord.cust-no
      w-ord.i-no
      w-ord.ord-qty
      w-ord.pcs
      w-ord.bundle
      w-ord.total-unit
      w-ord.total-tags
      WITH FRAME work-record.

    UPDATE w-ord.pcs w-ord.bundle w-ord.total-unit
      w-ord.total-tags WITH FRAME work-record
      EDITING:
      READKEY.
      IF KEYFUNCTION(LASTKEY) eq "end-error" THEN
      UNDO, LEAVE.
      ELSE
      IF FRAME-FIELD eq "pcs" OR FRAME-FIELD eq "bundle" THEN
      DO:
        IF KEYFUNCTION(LASTKEY) eq "return" OR
          KEYFUNCTION(LASTKEY) eq "go"     THEN
        DO:
          ASSIGN
            w-ord.pcs        = INPUT w-ord.pcs
            w-ord.bundle     = INPUT w-ord.bundle
            w-ord.total-unit = INPUT w-ord.pcs * INPUT w-ord.bundle
            w-ord.total-tags = ((w-ord.ord-qty / w-ord.total-unit) + .49) + 1.
          DISPLAY w-ord.total-unit w-ord.total-tags WITH FRAME work-record.
        END.  /* return/go */
        APPLY LASTKEY.
      END.
      ELSE
      IF FRAME-FIELD eq "total-unit" THEN
      DO:
        IF KEYFUNCTION(LASTKEY) eq "return" OR
          KEYFUNCTION(LASTKEY) eq "go"     THEN
        DO:
          ASSIGN
            w-ord.total-unit = INPUT w-ord.total-unit
            w-ord.total-tags = ((w-ord.ord-qty / w-ord.total-unit) + .49) + 1.
          DISPLAY w-ord.total-unit w-ord.total-tags WITH FRAME work-record.
        END.
        APPLY LASTKEY.
      END.
      ELSE
      APPLY LASTKEY.
    END. /* editing */
    DOWN WITH FRAME work-record.
  END.
  IF KEYFUNCTION(LASTKEY) eq "end-error" THEN
  UNDO, LEAVE.
  PAUSE.

  IF num-rec gt 12 THEN
  DO:
    CLEAR FRAME work-record ALL NO-PAUSE.

    FOR EACH w-ord:
      DISPLAY w-ord.ord-no
        w-ord.cust-no
        w-ord.i-no
        w-ord.ord-qty
        w-ord.pcs
        w-ord.bundle
        w-ord.total-unit
        w-ord.total-tags
        WITH FRAME work-record.
      DOWN WITH FRAME work-record.
    END.
    PAUSE.
  END.
  ==================================*/

  /* Create Text File */
  /* File will be created at c:\ba\label\loadtag.txt   */
  
  choice = NO.
  FIND FIRST w-ord  NO-ERROR.
  IF AVAIL w-ord THEN
     message "Are you Sure you Want to Create Loadtag File? " 
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice.
  
  IF NOT choice THEN RETURN ERROR.

  {sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}
/*
if td-show-parm then run show-param.
*/

      VIEW FRAME r-top.
      VIEW FRAME top.

v-out = "c:~\ba~\label~\loadtag.txt".

  IF choice THEN
  DO:
    IF OPSYS eq "UNIX" and v-loadtag ne "TRIAD" THEN
    DO:
      MESSAGE "Unable to Create Loadtag File for Non MSDos Platform.".
      PAUSE.
      RETURN.
    END.

    IF v-loadtag = "TRIAD" THEN
    DO:
  /*
      {sys/inc/print2.i}
      IF KEYFUNCTION(LASTKEY) eq "end-error" THEN
      UNDO, RETRY.  */
      IF choice THEN
      DO:
/*        {sys/msg/rproc.i}
        /* Removed 2/21/00 FWK for outprint change
        {sys/inc/outprint.i 0 "stream s-bar" "stream s-bar"}
        */

        if caps(printer.pr-port) eq "FILE" then do:
          assign prt-copies = 1
                 v-filname  = "rpt-file".

          update v-filname label " File Name"
              with frame pri-fil row 18 centered overlay no-box side-labels
                 color value(col-title).

          hide frame pri-fil no-pause.
          output stream s-bar to value(v-filname) page-size 0.
        end.

        else do:
          v-filname = "tmp" + STRING(time).
          output stream s-bar to value(v-filname) page-size 0.

          if lookup(printer.pr-port,"TERMINAL,FAX_MODEM") eq 0 then
            put control v-start-compress.
          else prt-copies = 1.
        end.
  */

        if form_fid > "" then do:   /* download the form file into the printer ~*/
          input stream s-form from value(form_fid) no-echo.
          _form: do while true:
                readkey stream s-form.
            if lastkey < 0 then leave _form.
              put stream s-bar CONTROL chr(lastkey).
          end.
          input stream s-form close.
        end.
      
        FOR EACH w-ord:
           v-job = w-ord.job-no + "-" + string(w-ord.job-no2,"99").
           IF v-job BEGINS "-" or v-job = ? /* 9901 CAH */
                THEN v-job = string(W-ORD.ORD-NO).   /* 9812 CAH in case blank */
         
           find first itemfg where itemfg.company = cocode
                    and itemfg.i-no = w-ord.i-no no-lock no-error.

        IF w-ord.total-tags gt -1 THEN
        DO:
          DO i = 1 TO (w-ord.total-tags + 1):
            /* select the form */
            put stream s-bar control stx esc "E" string(form#) ",1" can etx.
            /* 9901 CAH: done above ... 
            /* clear the variable data fields */
            put stream s-bar control stx can etx.
            */

            char_units = (if i <= w-ord.total-tags 
            then string(w-ord.total-unit) else "    ").  /* 9902 was .bundle */

            /* 9901 CAH: was coming up as ? in file for no good reason ... */
            def var char_date as char format 'x(10)' no-undo.
            /* 09.26.2001 CAH: Per Triad, use manufacturing date ...
            char_date = string(w-ord.due-date,"99/99/9999").
            if char_date = ? then char_date = string(today).
            */
            char_date = string(today,"99/99/9999").
            
            /* 9901 CAH: Only room for 19 chars in the standard 48 pt font */
            if length(w-ord.ship-name) > 19
            then w-ord.ship-name = substring(w-ord.ship-name,1,19).
            
            /* 07/2001 CAH: Add finished goods item number to the label
                and the n of m label counts */
            def var vcFGItem as char no-undo.
            vcFGItem = 
                if avail itemfg then itemfg.i-no else w-ord.i-no.
                    
                
           do n = copy_count to 1 by -1:

            /* send the variable data to the printer */
            put stream s-bar unformatted
                stx w-ord.cust-po-no    cr etx
                stx w-ord.cust-po-no    cr etx
                stx w-ord.cust-part-no  cr etx
                stx w-ord.cust-part-no  cr etx
                stx char_units          cr etx
                stx char_units          cr etx
                stx char_date           cr etx
                stx v-job               cr etx
                stx w-ord.ord-qty       cr etx /* 9902 CAH was total-unit */
                stx string(i)                   cr etx  /* 08.20 was n */
                stx string(w-ord.total-tags + 1) cr etx  /* 08.20 was copy_count */
                stx w-ord.ship-name     cr etx
                stx vcFGItem            cr etx
                .
                
            /* issue the print command */    
            put stream s-bar control     
                stx rs "1" us "1" etb etx.
           end.
                
          end.   /* tag count loop */
        end.  /* non zero */  

        END.    /* each w-ord */

      /*  {sys/inc/close.i "" "stream s-bar"} */
        OUTPUT CLOSE.
      END.    /* choice */

    END.    /* TRIAD INTERMEC BARCODE PRINT ROUTINE */
    ELSE
    DO:
    
      OUTPUT TO VALUE(v-out).
      PUT unformatted
          "CUSTOMER,JOBNUMBER,ITEM,CUSTPARTNO,CUSTPONO,PCS,BUNDLE,TOTAL," +
          "SHIPADD1,SHIPADD2,SHIPCITY,SHIPSTATE,SHIPZIP,INAME,DUEDATE," +
          "RELDATE,UPCNO".
      PUT SKIP.

      FOR EACH w-ord:
        v-job = w-ord.job-no + "-" + string(w-ord.job-no2,"99").
        IF v-job BEGINS "-" THEN
        v-job = "".

        IF w-ord.total-tags gt 0 THEN
        DO:
          DO i = 1 TO ((w-ord.total-tags - 1) * w-ord.mult):
            PUT "~""  w-ord.cust-name  "~","
              "~""  v-job  "~","
              "~""  w-ord.i-no  "~","
              "~""  w-ord.cust-part-no  "~","
              "~""  w-ord.cust-po-no  "~","
              w-ord.pcs  ","
              w-ord.bundle  ","
              w-ord.total-unit ","
              "~""  w-ord.ship-add1  "~","
              "~""  w-ord.ship-add2  "~","
              "~""  w-ord.ship-city  "~","
              "~""  w-ord.ship-state  "~","
              "~""  w-ord.ship-zip  "~","
              "~""  w-ord.i-name  "~","
              "~""  w-ord.due-date  "~","
              "~""  w-ord.rel-date  "~","
              "~""  w-ord.upc-no  "~",".
            put skip.
          end.
          
          do v-count = 1 to w-ord.mult:
            put "~""  w-ord.cust-name  "~","
              "~""  v-job  "~","
              "~""  w-ord.i-no  "~","
              "~""  w-ord.cust-part-no  "~","
              "~""  w-ord.cust-po-no  "~","
              w-ord.pcs  ","
              w-ord.bundle  ", ,"
              "~""  w-ord.ship-add1  "~","
              "~""  w-ord.ship-add2  "~","
              "~""  w-ord.ship-city  "~","
              "~""  w-ord.ship-state  "~","
              "~""  w-ord.ship-zip  "~","
              "~""  w-ord.i-name  "~","
              "~""  w-ord.due-date  "~","
              "~""  w-ord.rel-date  "~","
              "~""  w-ord.upc-no  "~",".
            put skip.
          end.
        end.
        delete w-ord.
      end.
      output close.
    end.    /* NOT TRIAD */
  
  end.

  


end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-old C-Win 
PROCEDURE run-report-old :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* ------------------------------------------------oe/rep/loadtag.p 03/97 FWK */
/* LoadTag Download Program                                                   */
/* -------------------------------------------------------------------------- */
/*
{sys/form/r-top.f}


assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 v-ford-no[1]   = begin_ord-no
 v-ford-no[2]   = end_ord-no
 v-fitem[1]     = begin_i-no
 v-fitem[2]     = end_i-no
 by-release     = tb_rel
 v-po-no-source = rd_print
 form#          = begin_form
 copy_count     = begin_labels
 form_fid       = begin_filename.
 
  FOR EACH w-ord:
    DELETE w-ord.
  END.

  for each oe-ord
      where oe-ord.company eq cocode
        and oe-ord.ord-no  ge v-ford-no[1]
        and oe-ord.ord-no  le v-ford-no[2]
      no-lock,
      
      first cust
      where cust.company eq cocode
        and cust.cust-no eq oe-ord.cust-no
      no-lock
      
      break by oe-ord.ord-no:

    FOR EACH oe-ordl
        WHERE oe-ordl.company eq oe-ord.company
        AND oe-ordl.i-no    ge v-fitem[1]
        AND oe-ordl.i-no    le v-fitem[2]
        AND oe-ordl.ord-no  eq oe-ord.ord-no
        NO-LOCK BREAK BY oe-ordl.i-no:
        
      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq oe-ordl.i-no
          no-lock no-error.
          
      IF NOT by-release THEN
      DO:
        IF FIRST-OF(oe-ordl.i-no) THEN
        DO:
          CREATE w-ord.

          ASSIGN
            w-ord.ord-no       = oe-ord.ord-no
            w-ord.job-no       = oe-ordl.job-no
            w-ord.job-no2      = oe-ordl.job-no2
            w-ord.cust-no      = oe-ord.cust-no
            w-ord.cust-name    = oe-ord.cust-name
            w-ord.i-no         = oe-ordl.i-no
            w-ord.cust-part-no = oe-ordl.part-no
            w-ord.cust-po-no   = if v-po-no-source eq "L" THEN oe-ordl.po-no
                                 else oe-ord.po-no
            w-ord.ord-qty      = oe-ordl.qty
            w-ord.i-name       = oe-ordl.i-name
            w-ord.due-date     = if oe-ord.due-date ne ? then
                                   oe-ord.due-date
                                 else
                                 if oe-ordl.req-date ne ? then
                                   oe-ordl.req-date
                                 else today
            w-ord.mult         = if cust.int-field[1] ne 0 then
                                   cust.int-field[1] else v-mult
            num-rec            = num-rec + 1.
            
          if avail itemfg then w-ord.upc-no = itemfg.upc-no.

          FIND FIRST shipto
            WHERE shipto.company eq cocode
            AND shipto.cust-no eq oe-ord.cust-no
            AND shipto.ship-id eq oe-ord.cust-no
            USE-INDEX ship-id NO-LOCK NO-ERROR.
          IF AVAIL shipto THEN
          ASSIGN
            w-ord.ship-name  = shipto.ship-name
            w-ord.ship-add1  = shipto.ship-add[1]
            w-ord.ship-add2  = shipto.ship-add[2]
            w-ord.ship-city  = shipto.ship-city
            w-ord.ship-state = shipto.ship-state
            w-ord.ship-zip   = shipto.ship-zip.

          FIND FIRST est
            WHERE est.company eq oe-ordl.company
            AND est.est-no  eq oe-ordl.est-no
            NO-LOCK NO-ERROR.
          RELEASE eb.
          IF AVAIL est THEN
          FIND FIRST eb
            WHERE eb.e-num    eq est.e-num
            AND eb.form-no  eq oe-ordl.form-no
            AND eb.blank-no eq oe-ordl.blank-no
            NO-LOCK NO-ERROR.

          IF AVAIL eb THEN
          ASSIGN
            w-ord.pcs        = eb.cas-cnt
            w-ord.bundle     = eb.cas-pal
            w-ord.total-unit = w-ord.pcs * w-ord.bundle.

          /* Add .49 to round up and add 1 for extra tag   */
          w-ord.total-tags = ((oe-ordl.qty / w-ord.total-unit) + .49) + 1.
        END.  /* first-of */
      END.  /* not by-release */

      ELSE
      FOR EACH oe-rel
          WHERE oe-rel.company eq cocode
          AND oe-rel.i-no    eq oe-ordl.i-no
          AND oe-rel.ord-no  eq oe-ordl.ord-no
          AND oe-rel.link-no ne 0 NO-LOCK:

        CREATE w-ord.
        ASSIGN
          w-ord.ord-no       = oe-ord.ord-no
          w-ord.job-no       = oe-ordl.job-no
          w-ord.job-no2      = oe-ordl.job-no2
          w-ord.cust-no      = oe-ord.cust-no
          w-ord.cust-name    = oe-ord.cust-name
          w-ord.i-no         = oe-ordl.i-no
          w-ord.cust-part-no = oe-ordl.part-no
          w-ord.cust-po-no   = IF v-po-no-source eq "L" THEN oe-ordl.po-no
          ELSE
          IF v-po-no-source eq "R" THEN oe-rel.po-no
          ELSE oe-ord.po-no
          w-ord.ord-qty      = oe-rel.qty
          w-ord.ship-add1    = oe-rel.ship-add[1]
          w-ord.ship-add2    = oe-rel.ship-add[2]
          w-ord.ship-city    = oe-rel.ship-city
          w-ord.ship-state   = oe-rel.ship-state
          w-ord.ship-zip     = oe-rel.ship-zip
          w-ord.i-name       = oe-ordl.i-name
          w-ord.due-date     = 
            (if oe-ord.due-date <> ? 
             then oe-ord.due-date
             else if oe-ordl.req-date <> ?  /* 9901 CAH */
             then oe-ordl.req-date
             else today)
          w-ord.rel-date     = oe-rel.rel-date
          w-ord.mult         = if cust.int-field[1] ne 0 then
                                 cust.int-field[1] else v-mult
          num-rec            = num-rec + 1.
          
        if avail itemfg then w-ord.upc-no = itemfg.upc-no.
          
        ASSIGN
          w-ord.pcs        = oe-rel.qty-case
          w-ord.bundle     = oe-rel.cases.
          w-ord.total-unit = w-ord.pcs * w-ord.bundle.

        /* Add .49 to round up and add 1 for extra tag   */
        ASSIGN w-ord.total-tags = ((oe-rel.qty / w-ord.total-unit) + .49) + 1.
      END.
    END.
  END.
  RUN oerep/d-loadtg.w .
 /* === replaced with oe/d-loadtg.w 
  /* Edit the Work Records  */
  FOR EACH w-ord:
    DISPLAY w-ord.ord-no
      w-ord.cust-no
      w-ord.i-no
      w-ord.ord-qty
      w-ord.pcs
      w-ord.bundle
      w-ord.total-unit
      w-ord.total-tags
      WITH FRAME work-record.

    UPDATE w-ord.pcs w-ord.bundle w-ord.total-unit
      w-ord.total-tags WITH FRAME work-record
      EDITING:
    READKEY.
      IF KEYFUNCTION(LASTKEY) eq "end-error" THEN
      UNDO, LEAVE.
      ELSE
      IF FRAME-FIELD eq "pcs" OR FRAME-FIELD eq "bundle" THEN
      DO:
        IF KEYFUNCTION(LASTKEY) eq "return" OR
          KEYFUNCTION(LASTKEY) eq "go"     THEN
        DO:
          ASSIGN
            w-ord.pcs        = INPUT w-ord.pcs
            w-ord.bundle     = INPUT w-ord.bundle
            w-ord.total-unit = INPUT w-ord.pcs * INPUT w-ord.bundle
            w-ord.total-tags = ((w-ord.ord-qty / w-ord.total-unit) + .49) + 1.
          DISPLAY w-ord.total-unit w-ord.total-tags WITH FRAME work-record.
        END.  /* return/go */
        APPLY LASTKEY.
      END.
      ELSE
      IF FRAME-FIELD eq "total-unit" THEN
      DO:
        IF KEYFUNCTION(LASTKEY) eq "return" OR
          KEYFUNCTION(LASTKEY) eq "go"     THEN
        DO:
          ASSIGN
            w-ord.total-unit = INPUT w-ord.total-unit
            w-ord.total-tags = ((w-ord.ord-qty / w-ord.total-unit) + .49) + 1.
          DISPLAY w-ord.total-unit w-ord.total-tags WITH FRAME work-record.
        END.
        APPLY LASTKEY.
      END.
      ELSE
      APPLY LASTKEY.
    END. /* editing */
    DOWN WITH FRAME work-record.
  END.
  IF KEYFUNCTION(LASTKEY) eq "end-error" THEN
  UNDO, LEAVE.
  PAUSE.

  IF num-rec gt 12 THEN
  DO:
    CLEAR FRAME work-record ALL NO-PAUSE.

    FOR EACH w-ord:
      DISPLAY w-ord.ord-no
        w-ord.cust-no
        w-ord.i-no
        w-ord.ord-qty
        w-ord.pcs
        w-ord.bundle
        w-ord.total-unit
        w-ord.total-tags
        WITH FRAME work-record.
      DOWN WITH FRAME work-record.
    END.
    PAUSE.
  END.
  ========== */
  /* Create Text File */
  /* File will be created at c:\ba\label\loadtag.txt   */
  choice = NO.
  message "Are you Sure you Want to Create Loadtag File? " 
    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice.
IF NOT choice THEN RETURN ERROR.

IF choice THEN
DO:
    IF OPSYS eq "UNIX" and v-loadtag ne "TRIAD" THEN
    DO:
      MESSAGE "Unable to Create Loadtag File for Non MSDos Platform.".
      PAUSE.
      RETURN.
    END.


{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}
/*
if td-show-parm then run show-param.
*/
display "" with frame r-top.

v-out = "c:~\ba~\label~\loadtag.txt".

  
  
    IF v-loadtag = "TRIAD" THEN
    DO:
  /*
      {sys/inc/print2.i}
      IF KEYFUNCTION(LASTKEY) eq "end-error" THEN
      UNDO, RETRY.
  */    
      IF choice THEN
      DO:
      /*
        if caps(printer.pr-port) eq "FILE" then do:
          assign prt-copies = 1
                 v-filname  = "rpt-file".

          update v-filname label " File Name"
              with frame pri-fil row 18 centered overlay no-box side-labels
                 color value(col-title).

          hide frame pri-fil no-pause.
          output stream s-bar to value(v-filname) page-size 0.
        end.

        else do:
          v-filname = "tmp" + STRING(time).
          output stream s-bar to value(v-filname) page-size 0.

          if lookup(printer.pr-port,"TERMINAL,FAX_MODEM") eq 0 then
            put control v-start-compress.
          else prt-copies = 1.
        end.
    */

        if form_fid > "" then do:   /* download the form file into the printer ~*/
          input stream s-form from value(form_fid) no-echo.
          _form: do while true:
                readkey stream s-form.
            if lastkey < 0 then leave _form.
              put stream s-bar CONTROL chr(lastkey).
          end.
          input stream s-form close.
        end.
    
        
        FOR EACH w-ord:
            v-job = w-ord.job-no + "-" + string(w-ord.job-no2,"99").
            IF v-job BEGINS "-" or v-job = ? /* 9901 CAH */
                 THEN v-job = string(W-ORD.ORD-NO).   /* 9812 CAH in case blank */
        
            find first itemfg where itemfg.company = cocode
                                and itemfg.i-no = w-ord.i-no no-lock no-error.

           IF w-ord.total-tags gt -1 THEN DO:
              DO i = 1 TO (w-ord.total-tags + 1):
              /* select the form */
              put stream s-bar control stx esc "E" string(form#) ",1" can etx.
              /* 9901 CAH: done above ... 
              /* clear the variable data fields */
              put stream s-bar control stx can etx.
              */

              char_units = (if i <= w-ord.total-tags 
                            then string(w-ord.total-unit) else "    ").  /* 9902 was .bundle */

              /* 9901 CAH: was coming up as ? in file for no good reason ... */
              def var char_date as char format 'x(10)' no-undo.
              /* 09.26.2001 CAH: Per Triad, use manufacturing date ...
              char_date = string(w-ord.due-date,"99/99/9999").
              if char_date = ? then char_date = string(today).
              */
              char_date = string(today,"99/99/9999").
            
              /* 9901 CAH: Only room for 19 chars in the standard 48 pt font */
              if length(w-ord.ship-name) > 19
                 then w-ord.ship-name = substring(w-ord.ship-name,1,19).
            
              /* 07/2001 CAH: Add finished goods item number to the label
                  and the n of m label counts */
              DEF var vcFGItem as char no-undo.
              vcFGItem = if avail itemfg then itemfg.i-no else w-ord.i-no.                    
                
              do n = copy_count to 1 by -1:
                 /* send the variable data to the printer */
                 put stream s-bar unformatted
                     stx w-ord.cust-po-no    cr etx
                     stx w-ord.cust-po-no    cr etx
                     stx w-ord.cust-part-no  cr etx
                     stx w-ord.cust-part-no  cr etx
                     stx char_units          cr etx
                     stx char_units          cr etx
                     stx char_date           cr etx
                     stx v-job               cr etx
                     stx w-ord.ord-qty       cr etx /* 9902 CAH was total-unit */
                     stx string(i)                   cr etx  /* 08.20 was n */
                     stx string(w-ord.total-tags + 1) cr etx  /* 08.20 was copy_count */
                     stx w-ord.ship-name     cr etx
                     stx vcFGItem            cr etx
                     .
                
                 /* issue the print command */    
                 put stream s-bar control     
                     stx rs "1" us "1" etb etx.
              end.                
          end.   /* tag count loop */
        end.  /* non zero */  

        END.    /* each w-ord */

        /*{sys/inc/close.i "" "stream s-bar"}*/
        OUTPUT CLOSE.
      END.    /* choice */

    END.    /* TRIAD INTERMEC BARCODE PRINT ROUTINE */
    ELSE
    DO:

      OUTPUT TO VALUE(v-out).
      PUT "CUSTOMER,JOBNUMBER,ITEM,CUSTPARTNO,CUSTPONO,PCS,BUNDLE,TOTAL,SHIPADD~1,SHIPADD2,SHIPCITY,SHIPSTATE,SHIPZIP,INAME,DUEDATE,RELDATE,UPCNO".
      PUT SKIP.

      FOR EACH w-ord:
        v-job = w-ord.job-no + "-" + string(w-ord.job-no2,"99").
        IF v-job BEGINS "-" THEN
        v-job = "".

        IF w-ord.total-tags gt 0 THEN
        DO:
          DO i = 1 TO ((w-ord.total-tags - 1) * w-ord.mult):
            PUT "~""  w-ord.cust-name  "~","
              "~""  v-job  "~","
              "~""  w-ord.i-no  "~","
              "~""  w-ord.cust-part-no  "~","
              "~""  w-ord.cust-po-no  "~","
              w-ord.pcs  ","
              w-ord.bundle  ","
              w-ord.total-unit ","
              "~""  w-ord.ship-add1  "~","
              "~""  w-ord.ship-add2  "~","
              "~""  w-ord.ship-city  "~","
              "~""  w-ord.ship-state  "~","
              "~""  w-ord.ship-zip  "~","
              "~""  w-ord.i-name  "~","
              "~""  w-ord.due-date  "~","
              "~""  w-ord.rel-date  "~","
              "~""  w-ord.upc-no  "~",".
          put skip.
        end.
        do v-count = 1 to w-ord.mult:
          put "~""  w-ord.cust-name  "~","
              "~""  v-job  "~","
              "~""  w-ord.i-no  "~","
              "~""  w-ord.cust-part-no  "~","
              "~""  w-ord.cust-po-no  "~","
              w-ord.pcs  ","
              w-ord.bundle  ", ,"
              "~""  w-ord.ship-add1  "~","
              "~""  w-ord.ship-add2  "~","
              "~""  w-ord.ship-city  "~","
              "~""  w-ord.ship-state  "~","
              "~""  w-ord.ship-zip  "~","
              "~""  w-ord.i-name  "~","
              "~""  w-ord.due-date  "~","
              "~""  w-ord.rel-date  "~","
              "~""  w-ord.upc-no  "~",".
          put skip.
        end.
      end.
      delete w-ord.
    end.
    output close.
    end.    /* NOT TRIAD */
    hide frame cr-load no-pause.
  end.

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */



*/

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

