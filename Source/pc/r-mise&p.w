&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: pc\r-mise&p.w

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
&IF DEFINED(UIB_IS_RUNNING) NE 0 &THEN
DEF VAR ip-post AS LOG INIT NO NO-UNDO.
&ELSE
DEF INPUT PARAMETER ip-post AS LOG NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */
DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/VAR.i new shared}

assign
 cocode = gcompany
 locode = gloc.

{oe/oe-bolp1.i NEW}

def buffer xfg-bin for fg-bin.

def var v-back like itemfg.q-back.
def var v-prt  as log init yes.
def var v-bol-bal like oe-boll.qty.
def var v-ref-no as int.
def var v-rcpth-no as int.
def var v-frst as log init no.
def var v-ext-price like inv-line.t-price.
def var olinecnt as int init 0.
def var frtcnt as log init no.
def var v-create as log.
def var v-bo-ch as log no-undo.
def var v-close-qty like oe-ordl.qty.

def var f as int no-undo.
def var v-assign-comm as log init no no-undo.
def var exist-amt as dec no-undo.
def var exist-flag as log init no no-undo.
def var exist-comm as dec extent 3 init 0 no-undo.
def var temp-tax as dec init 0 no-undo.
def var v-fg-qty like oe-boll.qty.
def var v-po-no like oe-rel.po-no.
def var v-rcpt-no as int.
def var v-ship-inst as char extent 2.
def var v-check-qty as log no-undo.

def TEMP-TABLE w-fg-bin NO-UNDO like fg-bin.

DEF VAR v-invalid AS LOG NO-UNDO.

FORMAT
  oe-bolh.bol-date space(2)
  oe-bolh.bol-no   format ">>>>>9" space(2)
  oe-bolh.carrier  space(4)
  oe-bolh.trailer  space(2)
  oe-bolh.freight  space(2)
  oe-bolh.cwt      space(3)
  oe-bolh.tot-wt   space(2)
  oe-bolh.cust-no  space(4)
  oe-bolh.ship-id  space(2)
  oe-bolh.deleted AT 106 format "*DELETED*/ "
  skip(1)

  header "Date         BOL.#  Carrier  Trailer     Freight    Rate  Tot WT  Cust#       Ship#   "
         "----------  ------  -------  --------  ---------  ------  ------  --------    --------"

  with stream-io width 132 no-labels no-box no-underline frame bolh.

format
  space(5)
  oe-boll.i-no
  itemfg.i-name    format "x(20)"
  oe-boll.po-no
  oe-boll.ord-no
  oe-boll.rel-no   format ">>9" space(0) "-" space(0)
  oe-boll.b-ord-no format "99"
  oe-boll.loc
  oe-boll.loc-bin
  oe-boll.tag
  oe-boll.cases    format ">>>>,>>9"
  oe-boll.qty-case format ">>>>,>>9"
  oe-boll.partial  format ">>>,>>9"
  oe-boll.weight   format ">>,>>9"

  header
  space(5) "Item#           Item Name            P.O. #            Ord#  Rel.# Whse. Bin Loc  Tag         Cases Qty/Case  Partial Weight" skip
  space(5) "--------------- -------------------- --------------- ------ ------ ----- -------- -------- -------- --------- ------- ------"
  with stream-io width 132 DOWN no-labels no-box no-underline frame boll.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tran-date begin_job-no begin_job-no2 ~
end_job-no end_job-no2 begin_date end_date rd-dest lines-per-page ~
td-show-parm btn-ok btn-cancel RECT-6 RECT-7 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period begin_job-no ~
begin_job-no2 end_job-no end_job-no2 begin_date end_date rd-dest ~
lines-per-page td-show-parm 

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "00" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "99" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 55 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tran-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Transaction Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tran-period AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Period" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3
     SIZE 23 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 5.48.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 10.24.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tran-date AT ROW 2.43 COL 36 COLON-ALIGNED
     tran-period AT ROW 2.43 COL 64 COLON-ALIGNED
     begin_job-no AT ROW 3.86 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 3.86 COL 41 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_job-no AT ROW 3.86 COL 72 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 3.86 COL 84 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     begin_date AT ROW 4.81 COL 28 COLON-ALIGNED HELP
          "Enter the beginning BOL date"
     end_date AT ROW 4.81 COL 72 COLON-ALIGNED HELP
          "Enter the ending BOL date"
     rd-dest AT ROW 12.43 COL 11 NO-LABEL
     lines-per-page AT ROW 12.91 COL 72 COLON-ALIGNED
     td-show-parm AT ROW 14.57 COL 58
     btn-ok AT ROW 18.14 COL 23
     btn-cancel AT ROW 18.14 COL 58
     RECT-6 AT ROW 11.48 COL 1
     RECT-7 AT ROW 1 COL 1
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11.71 COL 6
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 19.71.

DEFINE FRAME FRAME-E
     "The Edit List will show all available bills of lading to be" VIEW-AS TEXT
          SIZE 65 BY 1.19 AT ROW 1.24 COL 15
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "posted to all orders." VIEW-AS TEXT
          SIZE 26 BY 1.19 AT ROW 2.43 COL 15
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "Bills of Lading MUST BE printed prior to posting!" VIEW-AS TEXT
          SIZE 61 BY .95 AT ROW 3.86 COL 15
          BGCOLOR 11 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 7.19
         SIZE 94 BY 4.29
         BGCOLOR 11 .


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
         TITLE              = "BOL Edit List & Posting"
         HEIGHT             = 20.19
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-E:FRAME = FRAME FRAME-A:HANDLE.

/* SETTINGS FOR FRAME FRAME-A
                                                                        */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tran-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN tran-period IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-E
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* BOL Edit List  Posting */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* BOL Edit List  Posting */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no C-Win
ON LEAVE OF begin_job-no IN FRAME FRAME-A /* Beginning Job# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no2 C-Win
ON LEAVE OF begin_job-no2 IN FRAME FRAME-A
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
  DEF VAR lv-post AS LOG NO-UNDO.


  run check-date.
  if v-invalid then return no-apply.

  assign
   rd-dest
   tran-period
   v-s-date   = begin_date
   v-e-date   = end_date
   v-no-post  = 0
   v-tot-post = 0
   v-tried    = no.

  run run-report. 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case.

  IF ip-post THEN DO:
    IF v-tot-post GT 0 THEN DO:
      lv-post = NO.

      MESSAGE "Post BOLs?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE lv-post.

      IF lv-post THEN do:
        RUN post-bols.

        MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
      END.
    END.

    ELSE MESSAGE "No BOLs available for posting..." VIEW-AS ALERT-BOX ERROR.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no C-Win
ON LEAVE OF end_job-no IN FRAME FRAME-A /* Ending Job# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no2 C-Win
ON LEAVE OF end_job-no2 IN FRAME FRAME-A
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


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-A
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


&Scoped-define SELF-NAME tran-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-date C-Win
ON LEAVE OF tran-date IN FRAME FRAME-A /* Transaction Date */
DO:
  assign {&self-name}.

  if lastkey ne -1 then do:
    run check-date.
    if v-invalid then return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-period C-Win
ON LEAVE OF tran-period IN FRAME FRAME-A /* Period */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */    
{sys/inc/f3helpw.i}
DEF VAR choice AS LOG NO-UNDO.

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

  assign
   tran-date   = today
   begin_date  = TODAY
   end_date    = date(month(TODAY), 1, year(TODAY)).
/*
   c-win:TITLE = IF ip-post THEN "BOL Posting/Create Invoice"
                            ELSE "BOL Edit List".

  find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.
  v-u-inv = oe-ctrl.u-inv.

  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "BOLPOST"
      no-lock no-error.
  if not avail sys-ctrl then do transaction:
    create sys-ctrl.
    assign
     sys-ctrl.company = cocode
     sys-ctrl.name    = "BOLPOST"
     sys-ctrl.descrip = "Post BOL if BOL Qty > Bin Qty"
     choice           = yes.

    MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE choice.

    if not choice then sys-ctrl.char-fld eq "Bin>Qty".
  end.
  v-check-qty = sys-ctrl.char-fld eq "Bin>Qty".

  RUN enable_UI.

  RUN check-date.

  IF NOT ip-post THEN DO WITH FRAME {&FRAME-NAME}:  
    DISABLE tran-date tran-period.
  END.

  {methods/nowait.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.         */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-date C-Win 
PROCEDURE check-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO with frame {&frame-name}:
    v-invalid = no.

    find first period                   
        where period.company eq cocode
          and period.pst     le tran-date
          and period.pend    ge tran-date
        no-lock no-error.
    if avail period then tran-period:SCREEN-VALUE = string(period.pnum).

    else
    IF ip-post THEN DO:
      message "No Defined Period Exists for" tran-date view-as alert-box error.
      v-invalid = yes.
    end.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-nopost C-Win 
PROCEDURE create-nopost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-reason LIKE w-nopost.reason NO-UNDO.


  create w-nopost.
  assign
   w-nopost.ord-no   = oe-boll.ord-no
   w-nopost.bol-date = oe-bolh.BOL-date
   w-nopost.bol-no   = oe-bolh.BOL-no
   w-nopost.rel-no   = oe-boll.REL-no
   w-nopost.b-ord-no = oe-boll.b-ord-no
   w-nopost.cust-no  = oe-bolh.cust-no
   w-nopost.po-no    = oe-boll.PO-NO
   w-nopost.reason   = ip-reason.

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
  DISPLAY tran-date tran-period begin_job-no begin_job-no2 end_job-no 
          end_job-no2 begin_date end_date rd-dest lines-per-page td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE tran-date begin_job-no begin_job-no2 end_job-no end_job-no2 begin_date 
         end_date rd-dest lines-per-page td-show-parm btn-ok btn-cancel RECT-6 
         RECT-7 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-E IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-E}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exception-rpt C-Win 
PROCEDURE exception-rpt :
/* -------------------------------------------------- oe/oe-bolp7.p 11/01 JLF */
/* BOL posting Exception Report                                               */
/* -------------------------------------------------------------------------- */

{sys/form/r-top3w.f}

FORM HEADER SKIP(1) WITH FRAME r-top.


  FIND first period                   
      where period.company eq gcompany
        and period.pst     le tran-date
        and period.pend    ge tran-date
      no-lock no-error.

  assign
   str-tit2 = "BOL - Insufficient Inventory Report"
   {sys/inc/ctrtext.i str-tit2 112}

   str-tit3 = "Period " + STRING(tran-period,"99") + " - " +
              IF AVAIL period THEN
                (STRING(period.pst) + " to " + STRING(period.pend)) ELSE ""
   {sys/inc/ctrtext.i str-tit3 132}.

  {sys/inc/print1.i}

  {sys/inc/outprint.i value(lines-per-page)}

  display with frame r-top.

  for each w-except,

      first oe-bolh
      where oe-bolh.company eq cocode
        and oe-bolh.bol-no  eq w-except.bol-no
      no-lock

     break by w-except.bol-no
           by w-except.ord-no
           by w-except.rel-no
           by w-except.b-ord-no:

    if first-of(w-except.bol-no) then do:
      display oe-bolh.bol-date
              oe-bolh.bol-no
              oe-bolh.carrier
              oe-bolh.trailer
              oe-bolh.freight
              oe-bolh.cwt
              oe-bolh.tot-wt
              oe-bolh.cust-no
              oe-bolh.ship-id
              oe-bolh.deleted
          with frame bolh.
      down with frame bolh.
    end.

    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq w-except.i-no
        no-lock no-error.

    display w-except.i-no       @ oe-boll.i-no
            itemfg.i-name       when avail itemfg
            w-except.po-no      @ oe-boll.po-no
            w-except.ord-no     @ oe-boll.ord-no
            w-except.rel-no     @ oe-boll.rel-no
            w-except.b-ord-no   @ oe-boll.b-ord-no
            w-except.loc        @ oe-boll.loc
            w-except.loc-bin    @ oe-boll.loc-bin
            w-except.tag        @ oe-boll.tag
            w-except.cases      @ oe-boll.cases
            w-except.qty-case   @ oe-boll.qty-case
            w-except.partial    @ oe-boll.partial
            w-except.weight     @ oe-boll.weight
        with frame boll.
    down with frame boll.

    put skip(1).
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
     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
     DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
     DEFINE VARIABLE result AS LOGICAL NO-UNDO.

  /* Use Progress Print. Always use Font#9 in Registry (set above) */
     RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result).
                                    /* use-dialog(1) and landscape(2) */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-bols C-Win 
PROCEDURE post-bols :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lv-exception AS LOG NO-UNDO.


{sa/sa-sls01.i}

/**********************  POSTING BLOCK  ****************************/
post-blok:
do TRANSACTION.
  bolh:
  for each oe-bolh
      where oe-bolh.company  eq cocode
        and oe-bolh.posted   eq no
        and oe-bolh.printed  eq YES
        AND oe-bolh.deleted  EQ NO
        and oe-bolh.bol-no   ge v-s-bol
        and oe-bolh.bol-no   le v-e-bol
        and oe-bolh.bol-date ge v-s-date
        and oe-bolh.bol-date le v-e-date
        AND oe-bolh.cust-no  GE v-s-cust
        AND oe-bolh.cust-no  LE v-e-cust
        and oe-bolh.trailer  ne "HOLD"
        and not can-find(FIRST oe-boll
                         WHERE oe-boll.company  EQ oe-bolh.company
                           AND oe-boll.b-no     EQ oe-bolh.b-no
                           AND (oe-boll.loc     EQ "" OR
                                oe-boll.loc-bin EQ ""))
      use-index post,

      first cust
      where cust.company eq cocode
        and cust.cust-no eq oe-bolh.cust-no
      no-lock

      break by oe-bolh.bol-no
            by oe-bolh.ord-no
            by oe-bolh.rel-no.

    if first-of(oe-bolh.bol-no) and v-u-inv AND v-check-qty then
    for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK,

        first oe-ord
        where oe-ord.company eq cocode
          and oe-ord.ord-no  eq oe-boll.ord-no
          AND oe-ord.TYPE    NE "T"
        NO-LOCK

        break by oe-boll.i-no
              by oe-boll.job-no
              by oe-boll.job-no2
              by oe-boll.loc
              by oe-boll.loc-bin
              by oe-boll.tag:

      find first w-fg-bin
          where w-fg-bin.company eq cocode
            and w-fg-bin.i-no    eq oe-boll.i-no
            and w-fg-bin.job-no  eq oe-boll.job-no
            and w-fg-bin.job-no2 eq oe-boll.job-no2
            and w-fg-bin.loc     eq oe-boll.loc
            and w-fg-bin.loc-bin eq oe-boll.loc-bin
            and w-fg-bin.tag     eq oe-boll.tag
          no-error.
      if not avail w-fg-bin then do:
        create w-fg-bin.
        assign
         w-fg-bin.company = cocode
         w-fg-bin.i-no    = oe-boll.i-no
         w-fg-bin.job-no  = oe-boll.job-no
         w-fg-bin.job-no2 = oe-boll.job-no2
         w-fg-bin.loc     = oe-boll.loc
         w-fg-bin.loc-bin = oe-boll.loc-bin
         w-fg-bin.tag     = oe-boll.tag.
      end.
      w-fg-bin.qty = w-fg-bin.qty + oe-boll.qty.

      if last-of(oe-boll.tag) then do:
        find first fg-bin
            where fg-bin.company eq cocode
              and fg-bin.i-no    eq oe-boll.i-no
              and fg-bin.job-no  eq oe-boll.job-no
              and fg-bin.job-no2 eq oe-boll.job-no2
              and fg-bin.loc     eq oe-boll.loc
              and fg-bin.loc-bin eq oe-boll.loc-bin
              and fg-bin.tag     eq oe-boll.tag
            no-lock no-error.

        if not avail fg-bin or fg-bin.qty lt w-fg-bin.qty then do:
          create w-except.
          buffer-copy oe-boll to w-except.
        end.
      end.
    end.

    find first w-except where w-except.bol-no eq oe-bolh.bol-no no-error.
    if avail w-except then next bolh.

    find first shipto
        where shipto.company eq cocode
          and shipto.cust-no eq oe-bolh.cust-no
          and shipto.ship-id eq oe-bolh.ship-id
          and can-find(first fg-bin where fg-bin.company eq cocode
                                      and fg-bin.i-no    eq ""
                                      and fg-bin.loc     eq shipto.loc
                                      and fg-bin.loc-bin eq shipto.loc-bin)
        no-lock no-error.

    olinecnt = olinecnt + 1.

    for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no,

        first oe-ord
        where oe-ord.company eq cocode
          and oe-ord.ord-no  eq oe-boll.ord-no
        no-lock,

        first oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.ord-no  eq oe-boll.ord-no
          and oe-ordl.line    eq oe-boll.line
          and oe-ordl.i-no    eq oe-boll.i-no
        use-index ord-no no-lock,

        first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq oe-boll.i-no
        no-lock:

      if oe-ord.type eq "T" then do:            /* Process in-house transfer */
        if avail shipto then do:
          v-rcpt-no = 0.

          FOR EACH fg-rctd no-lock BY fg-rctd.r-no DESC:
            LEAVE.
          END.
          if avail fg-rctd then v-rcpt-no = fg-rctd.r-no.

          find last fg-rcpth use-index r-no no-lock no-error.
          if avail fg-rcpth and fg-rcpth.r-no gt v-rcpt-no THEN v-rcpt-no = fg-rcpth.r-no.

          create fg-rctd.
          assign
           fg-rctd.r-no      = v-rcpt-no + 1
           fg-rctd.company   = cocode
           fg-rctd.rct-date  = oe-bolh.bol-date
           fg-rctd.trans-time = TIME
           fg-rctd.i-no      = oe-boll.i-no
           fg-rctd.rita-code = "T"
           fg-rctd.job-no    = oe-boll.job-no
           fg-rctd.job-no2   = oe-boll.job-no2
           fg-rctd.loc       = oe-boll.loc
           fg-rctd.loc-bin   = oe-boll.loc-bin
           fg-rctd.tag       = oe-boll.tag
           fg-rctd.partial   = oe-boll.partial
           fg-rctd.cases     = oe-boll.cases
           fg-rctd.qty-case  = oe-boll.qty-case
           fg-rctd.t-qty     = oe-boll.qty
           fg-rctd.loc2      = shipto.loc
           fg-rctd.loc-bin2  = shipto.loc-bin.

          find first fg-bin         /* Make sure we have a bin to relieve */
              where fg-bin.company eq cocode
                and fg-bin.i-no    eq oe-boll.i-no
                and fg-bin.job-no  eq oe-boll.job-no
                and fg-bin.job-no2 eq oe-boll.job-no2
                and fg-bin.loc     eq oe-boll.loc
                and fg-bin.loc-bin eq oe-boll.loc-bin
                and fg-bin.tag     eq oe-boll.tag
              no-lock no-error.

          if not avail fg-bin then do:
            create fg-bin.
            assign
             fg-bin.company      = cocode
             fg-bin.i-no         = oe-boll.i-no
             fg-bin.job-no       = oe-boll.job-no
             fg-bin.job-no2      = oe-boll.job-no2
             fg-bin.loc          = oe-boll.loc
             fg-bin.loc-bin      = oe-boll.loc-bin
             fg-bin.tag          = oe-boll.tag
             fg-bin.case-count   = oe-boll.qty-case
             fg-bin.pur-uom      = itemfg.prod-uom
             fg-bin.std-tot-cost = itemfg.std-tot-cost
             fg-bin.std-mat-cost = itemfg.std-mat-cost
             fg-bin.std-lab-cost = itemfg.std-lab-cost
             fg-bin.std-var-cost = itemfg.std-var-cost
             fg-bin.std-fix-cost = itemfg.std-fix-cost.
          end.

          find first xfg-bin        /* Make sure we have a bin to receive */
              where xfg-bin.company eq cocode
                and xfg-bin.i-no    eq oe-boll.i-no
                and xfg-bin.job-no  eq oe-boll.job-no
                and xfg-bin.job-no2 eq oe-boll.job-no2
                and xfg-bin.loc     eq shipto.loc
                and xfg-bin.loc-bin eq shipto.loc-bin
                and xfg-bin.tag     eq ""
              no-lock no-error.

          if not avail xfg-bin then do:
            create xfg-bin.
            assign
             xfg-bin.company      = cocode
             xfg-bin.i-no         = oe-boll.i-no
             xfg-bin.job-no       = oe-boll.job-no
             xfg-bin.job-no2      = oe-boll.job-no2
             xfg-bin.loc          = shipto.loc
             xfg-bin.loc-bin      = shipto.loc-bin
             xfg-bin.case-count   = oe-boll.qty-case
             xfg-bin.pur-uom      = fg-bin.pur-uom
             xfg-bin.std-tot-cost = fg-bin.std-tot-cost
             xfg-bin.std-mat-cost = fg-bin.std-mat-cost
             xfg-bin.std-lab-cost = fg-bin.std-lab-cost
             xfg-bin.std-var-cost = fg-bin.std-var-cost
             xfg-bin.std-fix-cost = fg-bin.std-fix-cost.
          end.

          ASSIGN 
           fg-rctd.pur-uom  = fg-bin.pur-uom
           fg-rctd.std-cost = fg-bin.std-tot-cost.

          if fg-rctd.pur-uom eq "EA" then
            fg-rctd.ext-cost = fg-rctd.std-cost.
          else
            run sys/ref/convcuom.p(fg-rctd.pur-uom, "EA", 0, 0, 0, 0,
                                   fg-rctd.std-cost, output fg-rctd.ext-cost).

          assign
           fg-rctd.ext-cost = fg-rctd.ext-cost * fg-rctd.t-qty

           oe-bolh.posted = yes
           oe-boll.posted = yes.
        end.  
      end.

      else do:
         {oe/seq-bolh.i}
      end.
    end.      
  end. /* for each oe-bolh */

  EMPTY TEMP-TABLE w-fg-bin.

  run oe/oe-bolp3.p (v-term).

  hide frame post no-pause.
end. /* post-blok*/

delete-blok:
for each oe-bolh
    where oe-bolh.company  eq cocode
      and oe-bolh.deleted  eq YES
      and oe-bolh.bol-no   ge v-s-bol
      and oe-bolh.bol-no  le v-e-bol
      and oe-bolh.bol-date ge v-s-date
      and oe-bolh.bol-date le v-e-date
      AND oe-bolh.cust-no  GE v-s-cust
      AND oe-bolh.cust-no  LE v-e-cust
      and oe-bolh.trailer  ne "HOLD"
    use-index deleted:

  for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no:
    delete oe-boll.
  end. /* each oe-boll */

  delete oe-bolh.
end. /* each oe-bolh */

find first w-except no-error.
if avail w-except then do:
  lv-exception = YES.
  MESSAGE "  Bill(s) of Lading have been found that do not have  "     skip
          "  sufficient inventory for posting to be completed.   "     skip
          "  Do you wish to print the exception report?          "
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE lv-exception.

  IF lv-exception THEN do:
    run exception-rpt.

    case rd-dest:
         when 1 then run output-to-printer.
         when 2 then run output-to-screen.
         when 3 then run output-to-file.
    end case.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ----------------------------------------------- pc/rep/miscedit.p 8/94 gb  */
/* Production Control -transactions edit list                                 */
/* -------------------------------------------------------------------------- */

{sys/form/r-top3w.f}

    def var v-date like pc-misc.misc-date extent 2 format "99/99/9999" initial today no-undo.
    def var v-job-no like job.job-no extent 2 initial ["", "999999"] no-undo.
    def var v-job-no2 like job.job-no2 extent 2 initial [0, 99] no-undo.
    def var v-value as dec format "->>,>>>,>>9.99".

    form pc-misc.misc-date column-label "TRANS DATE"
     pc-misc.ml column-label "MATL/LABOR"
     pc-misc.job-no column-label "JOB #" space(0) "-" space(0)
     pc-misc.job-no2 no-label format "99"
     pc-misc.frm column-label "F /" space(0) "/" space(0)
     pc-misc.blank-no column-label "B"
     pc-misc.i-no column-label "ITEM #"
     pc-misc.m-code column-label "MACH"
     pc-misc.dscr column-label "DESCRIPTION" format "x(20)"
     pc-misc.cost-type column-label "COST!TYPE"
     pc-misc.dept column-label "DEPT"
     pc-misc.cost column-label "STD COST"
     pc-misc.cost-m column-label "COST/M"
 WITH FRAME miscedit NO-BOX DOWN STREAM-IO WIDTH 132.

   assign
      str-tit2 = c-win:title
      {sys/inc/ctrtext.i str-tit2 112}


     v-job-no[1]   = fill(" ",6 - length(trim(begin_job-no))) +
                     trim(begin_job-no) + string(int(begin_job-no2),"99")
     v-job-no[2]   = fill(" ",6 - length(trim(end_job-no)))   +
                     trim(end_job-no)   + string(int(end_job-no2),"99") 

     v-date[1]     = begin_date
     v-date[2]     = END_date.

   do x = 1 to 2:
      v-job-no[x] = fill(" ", 6 - integer(length(trim(v-job-no[x])))) +
                    trim(v-job-no[x]).
  END.


   do x = 1 to 2:
      v-job-no[x] = fill(" ", 6 - integer(length(trim(v-job-no[x])))) +
                    trim(v-job-no[x]).
   end.

        FORM HEADER SKIP(1) WITH FRAME r-top.

          FIND first period                   
              where period.company eq gcompany
                and period.pst     le tran-date
                and period.pend    ge tran-date
              no-lock no-error.

          assign
           str-tit2 = c-win:TITLE
           {sys/inc/ctrtext.i str-tit2 112}

           str-tit3 = "Period " + STRING(tran-period,"99") + " - " +
                      IF AVAIL period THEN
                        (STRING(period.pst) + " to " + STRING(period.pend)) ELSE ""
           {sys/inc/ctrtext.i str-tit3 132}.

          {sys/inc/print1.i}

          {sys/inc/outprint.i value(lines-per-page)}

          if td-show-parm then run show-param.

          display with frame r-top.

          for each pc-misc where pc-misc.company = cocode and
                         pc-misc.opn     = yes and
                         pc-misc.misc-date >= v-date[1] and
                         pc-misc.misc-date <= v-date[2]
                         no-lock
                         break by pc-misc.misc-date with frame miscedit:

                 if first-of(pc-misc.misc-date) then
                    put " " skip.

                 if pc-misc.job-no < v-job-no[1] or pc-misc.job-no > v-job-no[2]
                    or pc-misc.job-no2 < v-job-no2[1] or
                   pc-misc.job-no2 > v-job-no2[2]
                   then next.

                 display pc-misc.misc-date
                     pc-misc.ml
                     pc-misc.job-no
                     pc-misc.job-no2
                     pc-misc.frm
                     pc-misc.blank-no
                     pc-misc.i-no
                     pc-misc.m-code
                     pc-misc.dscr
                     pc-misc.cost-type
                     pc-misc.dept
                     pc-misc.cost
                     pc-misc.cost-m
                  with frame miscedit.
                 down.
              end.

              blok:
  do on error undo blok, leave blok:
     pause 0.
  postit:   

     do transaction on error undo postit, leave postit:

        transblok:
        for each pc-misc where pc-misc.company = cocode
                   on error undo postit, leave:

       {pc/pcmscact.i}

       delete pc-misc.
        end. /* for each pc-misc */


     end. /* postit */
  end. /* blok */

        v-no-post = 0.
/*
             for each w-nopost break by w-nopost.bol-no:
               if first(w-nopost.bol-no) then
                 put skip(1)
                     "** Bills Of Lading Unable To Be Posted. **" skip.

               DISPLAY w-nopost.bol-no     COLUMN-LABEL "BOL.#"
                       w-nopost.bol-date   COLUMN-LABEL "Date"
                       w-nopost.ord-no     COLUMN-LABEL "Order#"
                       string(w-nopost.rel-no,">>>9") + "-" +
                       string(w-nopost.b-ord-no,"99")
                                           COLUMN-LABEL "Rel#-BO#"    FORMAT "x(7)"
                       w-nopost.cust-no    COLUMN-LABEL "Cust.#"
                       w-nopost.po-no      COLUMN-LABEL "PO#"
                       w-nopost.i-no       COLUMN-LABEL "Item"
                       w-nopost.i-name     COLUMN-LABEL "Name"         format "x(20)"
                       w-nopost.reason     COLUMN-LABEL "Reason"       skip
                   with down STREAM-IO width 132 frame nopost2.
               down with frame nopost2.

               v-no-post = v-no-post + 1.

               delete w-nopost.
             end.
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
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:label + "," .
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

