&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oerep\r-ldtagi.w

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

def TEMP-TABLE w-file NO-UNDO field w-key AS ROWID.

{oerep/r-loadtg.i NEW}

{fg/fullset.i NEW}

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
&Scoped-Define ENABLED-OBJECTS begin_i-no end_i-no tb_16ths rd_comps ~
begin_form begin_labels begin_filename btn-ok btn-cancel RECT-7 
&Scoped-Define DISPLAYED-OBJECTS begin_i-no end_i-no tb_16ths rd_comps ~
begin_form begin_labels begin_filename 

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
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_labels AS INTEGER FORMAT ">>>>":U INITIAL 2 
     LABEL "# of Labels/Pallet" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "To Item#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE rd_comps AS CHARACTER INITIAL "B" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Assembled", "A",
"Unassembled", "U",
"Both", "B"
     SIZE 43 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 104 BY 12.38.

DEFINE VARIABLE tb_16ths AS LOGICAL INITIAL no 
     LABEL "Show LWD in 16ths?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_i-no AT ROW 3.62 COL 20 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_i-no AT ROW 3.62 COL 64 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     tb_16ths AT ROW 5.29 COL 40
     rd_comps AT ROW 6.71 COL 37 NO-LABEL
     begin_form AT ROW 8.62 COL 20 COLON-ALIGNED
     begin_labels AT ROW 9.57 COL 20 COLON-ALIGNED
     begin_filename AT ROW 10.76 COL 20 COLON-ALIGNED
     btn-ok AT ROW 21.71 COL 24
     btn-cancel AT ROW 21.71 COL 66
     RECT-7 AT ROW 1 COL 1
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 2
     "Print Set Components for:" VIEW-AS TEXT
          SIZE 25 BY 1 AT ROW 6.71 COL 9
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 105.2 BY 23.38.


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
         TITLE              = "Loadtag Creation from Stock"
         HEIGHT             = 23.71
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
IF NOT C-Win:LOAD-ICON("images\progress":U) THEN
    MESSAGE "Unable to load icon: images\progress"
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
       end_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_16ths:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Loadtag Creation from Stock */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Loadtag Creation from Stock */
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
  assign begin_i-no
         END_i-no
         rd_comps
         tb_16ths
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


&Scoped-define SELF-NAME tb_16ths
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_16ths C-Win
ON VALUE-CHANGED OF tb_16ths IN FRAME FRAME-A /* Show LWD in 16ths? */
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

  FIND FIRST company WHERE company.company EQ gcompany NO-LOCK.

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ gcompany
        AND sys-ctrl.name    EQ "CEMENU"
      NO-LOCK NO-ERROR.
  ASSIGN
   tb_16ths  = AVAIL sys-ctrl AND sys-ctrl.char-fld EQ "Corrware".

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

  APPLY "entry" TO begin_i-no IN FRAME {&FRAME-NAME}.

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
  DISPLAY begin_i-no end_i-no tb_16ths rd_comps begin_form begin_labels 
          begin_filename 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_i-no end_i-no tb_16ths rd_comps begin_form begin_labels 
         begin_filename btn-ok btn-cancel RECT-7 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE from-job C-Win 
PROCEDURE from-job :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

    
    FIND FIRST job
        WHERE ROWID(job) EQ ip-rowid
          AND (v-stat EQ "A"                      OR
               (v-stat EQ "C" AND NOT job.opened) OR
               (v-stat EQ "O" AND job.opened))
        NO-LOCK NO-ERROR.

    IF AVAIL job THEN
    FOR EACH job-hdr
        WHERE job-hdr.company EQ job.company
          /*AND job-hdr.job     EQ job.job*/
          AND job-hdr.job-no  EQ job.job-no
          AND job-hdr.job-no2 EQ job.job-no2
          AND job-hdr.i-no    GE v-fitem[1]
          AND job-hdr.i-no    LE v-fitem[2]
          AND job-hdr.ord-no  EQ 0
        USE-INDEX ord-no NO-LOCK,
        FIRST cust
        WHERE cust.company eq cocode
          AND cust.cust-no eq job-hdr.cust-no
        NO-LOCK,
        FIRST itemfg
        WHERE itemfg.company eq cocode
          AND itemfg.i-no    eq job-hdr.i-no
        NO-LOCK:
     MESSAGE 3 VIEW-AS ALERT-BOX.
          CREATE w-ord.

          ASSIGN
            w-ord.ord-no       = job-hdr.ord-no
            w-ord.job-no       = job-hdr.job-no
            w-ord.job-no2      = job-hdr.job-no2
            w-ord.cust-no      = cust.cust-no
            w-ord.cust-name    = cust.name
            w-ord.i-no         = job-hdr.i-no
            w-ord.cust-part-no = itemfg.part-no
            w-ord.ord-qty      = job-hdr.qty
            w-ord.i-name       = itemfg.i-name
            w-ord.upc-no       = itemfg.upc-no
            w-ord.due-date     = job.start-date
            w-ord.est-no       = job.est-no
            w-ord.form-no      = job-hdr.frm
            w-ord.upc-no       = itemfg.upc-no
            w-ord.box-len      = itemfg.l-score[50]
            w-ord.box-wid      = itemfg.w-score[50]
            w-ord.box-dep      = itemfg.d-score[50]
            w-ord.vendor       = company.name
            w-ord.tare-wt      = 10
            w-ord.uom          = "EA"
            w-ord.mult         = if cust.int-field[1] ne 0 then
                                   cust.int-field[1] else v-mult
            num-rec            = num-rec + 1.

          FIND FIRST shipto
              WHERE shipto.company eq cocode
                AND shipto.cust-no eq job-hdr.cust-no
                AND shipto.ship-id eq job-hdr.cust-no
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
              WHERE est.company eq job.company
                AND est.est-no  eq job.est-no
              NO-LOCK NO-ERROR.
          RELEASE eb.
          IF AVAIL est THEN
          FIND FIRST eb
              WHERE eb.company   EQ est.company
                AND eb.est-no    EQ est.est-no
                AND eb.form-no   EQ job-hdr.frm
                AND (eb.blank-no EQ job-hdr.blank-no OR job-hdr.blank-no EQ 0)
              NO-LOCK NO-ERROR.

          IF AVAIL eb THEN
            ASSIGN
             w-ord.flute      = eb.flute
             w-ord.test       = eb.test
             w-ord.pcs        = eb.cas-cnt
             w-ord.bundle     = eb.cas-pal
             w-ord.total-unit = w-ord.pcs * w-ord.bundle.

          /* Add .49 to round up and add 1 for extra tag   */
          w-ord.total-tags = ((job-hdr.qty / w-ord.total-unit) + .49) + 1.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE from-ord C-Win 
PROCEDURE from-ord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

    
    FIND FIRST oe-ord
        WHERE ROWID(oe-ord) EQ ip-rowid
          AND (v-stat EQ "A"                                    OR
               (v-stat EQ "C" AND INDEX("CZ",oe-ord.stat) GT 0) OR
               (v-stat EQ "O" AND INDEX("CZ",oe-ord.stat) EQ 0))
        NO-LOCK NO-ERROR.

    IF AVAIL oe-ord THEN
    FIND FIRST cust
        WHERE cust.company eq cocode
          AND cust.cust-no eq oe-ord.cust-no
        NO-LOCK NO-ERROR.

    IF AVAIL cust THEN
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

      IF oe-ordl.est-no NE "" THEN
      FIND FIRST eb
          WHERE eb.company  EQ oe-ordl.company
            AND eb.est-no   EQ oe-ordl.est-no
            AND eb.stock-no EQ oe-ordl.i-no
          NO-LOCK NO-ERROR.
          
      IF NOT by-release OR NOT AVAIL oe-ordl THEN
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
            w-ord.est-no       = oe-ordl.est-no
            w-ord.form-no      = oe-ordl.form-no
            w-ord.vendor       = company.name
            w-ord.tare-wt      = 10
            w-ord.uom          = "EA"
            w-ord.mult         = if cust.int-field[1] ne 0 then
                                   cust.int-field[1] else v-mult
            num-rec            = num-rec + 1.
            
          IF AVAIL itemfg THEN
            ASSIGN
             w-ord.upc-no   = itemfg.upc-no
             w-ord.box-len  = itemfg.l-score[50]
             w-ord.box-wid  = itemfg.w-score[50]
             w-ord.box-dep  = itemfg.d-score[50].

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

          IF NOT AVAIL eb AND AVAIL itemfg AND itemfg.est-no NE "" THEN
          FIND FIRST eb
              WHERE eb.company  EQ itemfg.company
                AND eb.est-no   EQ itemfg.est-no
                AND eb.stock-no EQ itemfg.i-no
              NO-LOCK NO-ERROR.

          IF AVAIL eb THEN
            ASSIGN
             w-ord.flute      = eb.flute
             w-ord.test       = eb.test
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
          w-ord.est-no       = oe-ordl.est-no
          w-ord.form-no      = oe-ordl.form-no
          w-ord.vendor       = company.name
          w-ord.tare-wt      = 10
          w-ord.uom          = "EA"
          w-ord.mult         = if cust.int-field[1] ne 0 then
                                 cust.int-field[1] else v-mult
          num-rec            = num-rec + 1.
          
        IF AVAIL itemfg THEN
          ASSIGN
           w-ord.upc-no   = itemfg.upc-no
           w-ord.box-len  = itemfg.l-score[50]
           w-ord.box-wid  = itemfg.w-score[50]
           w-ord.box-dep  = itemfg.d-score[50].

        IF NOT AVAIL eb AND AVAIL itemfg AND itemfg.est-no NE "" THEN
        FIND FIRST eb
            WHERE eb.company  EQ itemfg.company
              AND eb.est-no   EQ itemfg.est-no
              AND eb.stock-no EQ itemfg.i-no
            NO-LOCK NO-ERROR.

        IF AVAIL eb THEN
          ASSIGN
           w-ord.flute = eb.flute
           w-ord.test  = eb.test.
          
        ASSIGN
          w-ord.pcs        = oe-rel.qty-case
          w-ord.bundle     = oe-rel.cases.
          w-ord.total-unit = w-ord.pcs * w-ord.bundle.

        /* Add .49 to round up and add 1 for extra tag   */
        ASSIGN w-ord.total-tags = ((oe-rel.qty / w-ord.total-unit) + .49) + 1.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE temp-create C-Win 
PROCEDURE temp-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
  
  CREATE w-file.
  w-key = ip-rowid.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE temp-job C-Win 
PROCEDURE temp-job :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-job-no LIKE job.job-no NO-UNDO.

  FOR EACH job
      WHERE job.company EQ cocode
        AND job.job-no  EQ SUBSTR(ip-job-no,1,6)
        AND job.job-no2 EQ INT(SUBSTR(ip-job-no,7,2))
      NO-LOCK:
    RUN temp-create (ROWID(job)).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE temp-ord C-Win 
PROCEDURE temp-ord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-ord-no LIKE oe-ord.ord-no NO-UNDO.

  FOR EACH oe-ord
      WHERE oe-ord.company EQ cocode
        AND oe-ord.ord-no  EQ ip-ord-no
      NO-LOCK:
    RUN temp-create (ROWID(oe-ord)).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

