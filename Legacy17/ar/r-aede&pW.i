&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-orde&p.w

  Description: Order Edit List & Posting

  Input Parameters: ip-post

  Output Parameters:
      <none>

  Author: JLF

  Created: 03/05/2002

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
DEF VAR ip-post AS LOG NO-UNDO.
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
def shared workfile wkdistrib no-undo
  field actnum like account.actnum column-label "Account"
  field tr-dscr like gltrans.tr-dscr
  field amount as dec format "->>>,>>>,>>>.99"
  field recs   as int column-label "Records" format ">>,>>>"
  field ar-type-credit as log.

def var ws_debit  like wkdistrib.amount column-label "Debit" no-undo.
def var ws_credit like wkdistrib.amount column-label "Credit" no-undo.
def shared var ws_net    like wkdistrib.amount column-label "Net"   no-undo.
def var num-recs  like wkdistrib.recs no-undo.
def var tot-debit  like wkdistrib.amount no-undo.
def var tot-credit like wkdistrib.amount no-undo.

def var posting as log no-undo init no.

if program-name(2) matches "*ar-inreg.*" then posting = true. else posting = no.

form with frame f-distrib down width 132 no-box.

def var save_id as recid.
def var time_stamp as ch.
def var qfirst as log.
def var g1 as dec format "->>,>>>,>>9.99".
def var t1 like g1.
def var g2 like g1.
def var g3 like g1.
def var g4 like g1.
def var t3 like g1.
def var v1 like g1.
def var v2 like g1.
def var t2 like g1.
def var tot like g1.
def shared var xtrnum as int.
def shared var xar-acct like account.actnum no-undo.
def var xar-stax like account.actnum.
def var xar-freight like account.actnum.
def var v-sort as log init yes format "Y/N".

def shared var v-s-inv-no like ar-inv.inv-no init 0 no-undo.
def shared var v-e-inv-no like v-s-inv-no init 999999.
def shared var v-s-date   like ar-inv.inv-date format "99/99/9999"
                                          init 01/01/0001 no-undo.
def shared var v-e-date   like v-s-date init today.

do for ar-ctrl:
  find first ar-ctrl where ar-ctrl.company = cocode NO-LOCK.
  xar-acct = ar-ctrl.receivables.
  find first account
      where account.company eq cocode
        and account.actnum  eq ar-ctrl.receivables
      no-error.
  if not avail account then do:
    bell.
    message "A/R Control Files has a null or invalid Receivables Account.".
    hide message.
  end.
  xar-freight = ar-ctrl.freight.
  find first account
      where account.company eq cocode
        and account.actnum  eq ar-ctrl.freight
      no-error.
  if not avail account then do:
    bell.
    message "A/R Control Files has a null or invalid Freight Account.".
    hide message.
  end.
  xar-stax = ar-ctrl.stax.
  find first account
      where account.company eq cocode
        and account.actnum  eq ar-ctrl.stax
      no-error.
  if not avail account then do:
    bell.
    message "A/R Control Files has a null or invalid Sales Tax Account.".
    hide message.
  end.
  release ar-ctrl.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tran-date tb_detailed rd-dest lines-per-page ~
td-show-parm btn-ok btn-cancel RECT-6 RECT-7 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period lbl_detailed ~
tb_detailed rd-dest lines-per-page td-show-parm 

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

DEFINE VARIABLE lbl_detailed AS CHARACTER FORMAT "X(256)":U INITIAL "Detailed?" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 55 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tran-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Transaction Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tran-period AS INTEGER FORMAT ">>":U INITIAL 0 
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

DEFINE VARIABLE tb_detailed AS LOGICAL INITIAL no 
     LABEL "Detailed?" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tran-date AT ROW 4.1 COL 45 COLON-ALIGNED
     tran-period AT ROW 5.29 COL 45 COLON-ALIGNED
     lbl_detailed AT ROW 7.67 COL 34 COLON-ALIGNED NO-LABEL
     tb_detailed AT ROW 7.67 COL 47
     rd-dest AT ROW 12.43 COL 11 NO-LABEL
     lines-per-page AT ROW 12.91 COL 72 COLON-ALIGNED
     td-show-parm AT ROW 14.57 COL 58
     btn-ok AT ROW 18.38 COL 23
     btn-cancel AT ROW 18.38 COL 58
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
         TITLE              = "Order Edit List & Posting"
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
/* SETTINGS FOR FILL-IN lbl_detailed IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_detailed:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_detail".

ASSIGN 
       tb_detailed:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tran-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN tran-period IN FRAME FRAME-A
   NO-ENABLE                                                            */
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
ON END-ERROR OF C-Win /* Order Edit List  Posting */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Order Edit List  Posting */
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
  DEF VAR lv-post AS LOG NO-UNDO.


  assign
   rd-dest
   tran-period.

  run check-date.
  if v-invalid then return no-apply.

  run run-report. 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case.

  IF ip-post THEN DO:
    IF v-postable THEN DO:
      lv-post = NO.

      MESSAGE "Post Orders?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE lv-post.

      IF lv-post THEN do:
        RUN list-post ("post").

        MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
      END.
    END.

    ELSE MESSAGE "No orders available for posting..." VIEW-AS ALERT-BOX ERROR.
  END.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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


&Scoped-define SELF-NAME tb_detailed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_detailed C-Win
ON VALUE-CHANGED OF tb_detailed IN FRAME FRAME-A /* Detailed? */
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

  assign
   tran-date   = today
   c-win:TITLE = IF ip-post THEN "Order Posting/Purge Deleted"
                            ELSE "Order Edit List".

  RUN enable_UI.

  RUN check-date.

  IF NOT ip-post THEN DO WITH FRAME {&FRAME-NAME}:  
    DISABLE tran-date tran-period.
  END.

  {methods/nowait.i}
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
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
  DISPLAY tran-date tran-period lbl_detailed tb_detailed rd-dest lines-per-page 
          td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE tran-date tb_detailed rd-dest lines-per-page td-show-parm btn-ok 
         btn-cancel RECT-6 RECT-7 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE list-post C-Win 
PROCEDURE list-post :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-list-post AS CHAR NO-UNDO.


  for each w-report where w-report.term-id eq v-term no-lock,

      first xoe-ord where recid(xoe-ord) eq w-report.rec-id,

      first cust {sys/ref/custW.i} and cust.cust-no eq xoe-ord.cust-no

      by w-report.key-01:

    find first stax
        {sys/ref/staxW.i}
          and stax.tax-group eq xoe-ord.tax-gr
        no-lock no-error.
    v-tax-rate = IF AVAIL stax THEN
                   stax.tax-rate[1] + stax.tax-rate[2] + stax.tax-rate[3]
                 ELSE 0.

    IF ip-list-post EQ "list" THEN do:
      v-postable = yes.

      /******* CALCULATE TOTALS FOR ORDER ***************/
      assign
       v-tot-ord = 0
       v-tot-qty = 0.

      FOR EACH oe-ordl
          WHERE oe-ordl.company EQ xoe-ord.company
            AND oe-ordl.ord-no  EQ xoe-ord.ord-no
          no-lock break by oe-ordl.ord-no:

        if first-of(oe-ordl.ord-no) and v-detail  then
          put skip.

        assign
         v-qty-lft = oe-ordl.qty - oe-ordl.inv-qty.
         v-ext-price = 0.

        if oe-ordl.pr-uom Begins "L" then
          assign
           v-ext-price = oe-ordl.price -
                         round( (oe-ordl.price * oe-ordl.disc) / 100, 2).

        else
        if oe-ordl.pr-uom EQ "CS" THEN DO:
          FIND FIRST itemfg
              {sys/look/itemfgrlW.i}
                and itemfg.i-no EQ oe-ordl.i-no
              no-lock no-error.
          if avail itemfg and itemfg.case-count ne 0 then
            assign
             v-ext-price = ((v-qty-lft / itemfg.case-count) * oe-ordl.price) -
                           round((((v-qty-lft / itemfg.case-count) *
                                   oe-ordl.price) * oe-ordl.disc) / 100, 2).
          else
            assign
             v-ext-price = (v-qty-lft * oe-ordl.price) -
                           round(((v-qty-lft * oe-ordl.price) * oe-ordl.disc) / 100, 2).
        end.

        ELSE
        if oe-ordl.pr-uom EQ "C" then
          assign
           v-ext-price = ((v-qty-lft / 100) *
                          oe-ordl.price) - round((((v-qty-lft / 100) *
                          oe-ordl.price) * oe-ordl.disc) / 100, 2).
        else
        if oe-ordl.pr-uom EQ "EA" then
          v-ext-price = (v-qty-lft * oe-ordl.price) -
                        round(( (v-qty-lft * oe-ordl.price) * oe-ordl.disc) / 100, 2).

        ELSE
          assign
           v-ext-price = ((v-qty-lft / 1000) *
                          oe-ordl.price) - round(( ((v-qty-lft / 1000) *
                          oe-ordl.price) * oe-ordl.disc) / 100, 2).

                                           /** CALCULATE FREIGHT CHARGES **/
        v-tot-freight = v-tot-freight +
                        (round(oe-ordl.t-freight / oe-ordl.qty, 2) * v-qty-lft).

                                           /** CALCULATE TAX CHARGES **/
        if oe-ordl.tax and v-tax-rate > 0 then assign
          v-tot-tax = v-tot-tax +
                                round((v-ext-price * v-tax-rate) / 100,2).

        create w-ord-line.
        assign
         w-ord-line.ord-no = oe-ordl.ord-no
         w-ord-line.i-no = oe-ordl.i-no
         w-ord-line.i-name = oe-ordl.i-name
         w-ord-line.qty-lft = v-qty-lft
         w-ord-line.price = oe-ordl.price
         w-ord-line.uom = oe-ordl.pr-uom
         w-ord-line.t-price = v-ext-price.

        assign
         v-tot-ord = v-tot-ord +
                     if xoe-ord.stat ne "D" then v-ext-price
                                           else - ( v-ext-price )
         v-tot-qty = v-tot-qty +
                     if xoe-ord.stat ne "D" then v-qty-lft
                                           else - ( v-qty-lft ).  
      end. /* each oe-ordl */

      for each oe-ordm
          where oe-ordm.company EQ xoe-ord.company
            AND oe-ordm.ord-no  EQ xoe-ord.ord-no
          no-lock

          break by oe-ordm.ord-no:

        if oe-ordm.bill EQ "Y" then do:
          v-tot-ord = v-tot-ord + oe-ordm.amt.

          if oe-ordm.tax and v-tax-rate > 0 THEN
            v-tot-tax = v-tot-tax + round((oe-ordm.amt * v-tax-rate) / 100,2).
        end. /* = "Y" */

        create w-ord-misc.
        assign
         w-ord-misc.ord-no = oe-ordm.ord-no
         w-ord-misc.charge = oe-ordm.charge
         w-ord-misc.dscr = oe-ordm.dscr
         w-ord-misc.amt = oe-ordm.amt
         w-ord-misc.tax = oe-ordm.tax
         w-ord-misc.bill = oe-ordm.bill.
      end. /* each oe-ordm */

      DISPLAY xoe-ord.ord-no
              xoe-ord.est-no
              xoe-ord.job-no
              xoe-ord.job-no2
              xoe-ord.due-date 
              xoe-ord.cust-no
              xoe-ord.cust-name
              xoe-ord.sold-id
              v-tot-ord

          with frame ord.

      for each w-ord-line break by w-ord-line.ord-no:
        if v-detail THEN do:
          if first(w-ord-line.ord-no) then put skip(1).

          DISPLAY w-ord-line.i-no
                  w-ord-line.i-name
                  w-ord-line.qty-lft
                  w-ord-line.price
                  w-ord-line.uom
                  w-ord-line.t-price

              with frame ordl.
          down with frame ordl.
        end.

        delete w-ord-line.
      end. /* each w-ord-line */

      for each w-ord-misc break by w-ord-misc.ord-no:
        if v-detail THEN do:
          if first(w-ord-misc.ord-no) then put skip(1).

          DISPLAY w-ord-misc.charge
                  w-ord-misc.dscr w-ord-misc.amt

              with frame ordm.

          if w-ord-misc.bill = "N" then
            DISPLAY "       N/C" @ w-ord-misc.amt with frame ordm.

          down with frame ordm.
        end. /* v-detail */

        delete w-ord-misc.
      end. /* each w-ord-misc */

      if xoe-ord.stat EQ "H" then
        put "** THIS ORDER IS ON CREDIT HOLD **" to 39.
      else
      if xoe-ord.stat EQ "D" then
        put "** THIS ORDER IS DELETED **" to 39.
      else
      if xoe-ord.stat EQ "C" OR xoe-ord.stat EQ "Z" then
        put "** THIS ORDER IS CLOSED **" to 39.

      if xoe-ord.stat ne "D" then
        assign
         v-g-tot-ord = v-g-tot-ord + v-tot-ord
         v-g-tot-qty = v-g-tot-qty + v-tot-qty.
      else
        assign
         v-g-del-tot-ord = v-g-del-tot-ord + v-tot-ord
         v-g-del-tot-qty = v-g-del-tot-qty + v-tot-qty.

      put skip(1).
    end.

    IF ip-list-post EQ "post" THEN DO TRANSACTION:
      assign
       xoe-ord.t-comm    = 0
       xoe-ord.t-cost    = 0
       xoe-ord.t-revenue = 0
       xoe-ord.tax       = 0
       v-inv             = NO
       v-ship            = NO.

      FOR EACH oe-ordl
          WHERE oe-ordl.company EQ xoe-ord.company
            AND oe-ordl.ord-no  EQ xoe-ord.ord-no:

        if oe-ordl.stat eq "I" or oe-ordl.stat eq "B" then v-inv = yes.
        else v-ship = yes.

        assign
         xoe-ord.t-revenue = xoe-ord.t-revenue + oe-ordl.t-price
         xoe-ord.t-cost    = xoe-ord.t-cost    + oe-ordl.t-cost.

        if oe-ordl.tax and v-tax-rate gt 0 then
          assign
           xoe-ord.tax = xoe-ord.tax +
                        round((oe-ordl.t-price * v-tax-rate) / 100,2).

        if xoe-ord.stat eq "D" then delete oe-ordl.
      END.

      for each oe-ordm
          where oe-ordm.company eq xoe-ord.company
            and oe-ordm.ord-no  eq xoe-ord.ord-no:

        IF oe-ordm.bill EQ "Y" THEN do:
          xoe-ord.t-revenue = xoe-ord.t-revenue + oe-ordm.amt.

          if oe-ordm.tax and v-tax-rate gt 0 then
            xoe-ord.tax = xoe-ord.tax +
                         round((oe-ordm.amt * v-tax-rate) / 100,2).
        END.

        if xoe-ord.stat eq "D" then delete oe-ordm.
      END.

      run oe/oe-comm.p.  /* Calculate Commissions */

      if xoe-ord.f-bill THEN
        xoe-ord.t-revenue = xoe-ord.t-revenue + xoe-ord.t-freight.

      if v-fr-tax THEN
        xoe-ord.tax = xoe-ord.tax +
                     round((oe-ord.t-freight * v-tax-rate) / 100,2).

      IF xoe-ord.stat eq "H" then xoe-ord.posted = yes.

      ELSE
      IF xoe-ord.stat eq "D" then do:
        FOR EACH oe-rel
            WHERE oe-rel.company EQ xoe-ord.company
              AND oe-rel.ord-no  EQ xoe-ord.ord-no:
          DELETE oe-rel.
        END.

        for each oe-relh
            where oe-relh.company eq xoe-ord.company
              and oe-relh.ord-no  eq xoe-ord.ord-no
              and oe-relh.posted  eq no
            use-index relh:

          for each oe-rell where oe-rell.r-no eq oe-relh.r-no:
            delete oe-rell.
          end.

          delete oe-relh.
        end.

        find first oe-ctrl WHERE oe-ctrl.company EQ xoe-ord.company
            exclusive-lock no-error.
        if xoe-ord.ord-no eq oe-ctrl.n-ord - 1 then
           oe-ctrl.n-ord = xoe-ord.ord-no.

        delete xoe-ord.
      end.

      else
      if xoe-ord.stat eq "C" then xoe-ord.stat = "Z".

      ELSE DO:   
        if xoe-ord.stat eq "U" THEN
          xoe-ord.stat = IF v-inv THEN "I" ELSE
                        IF v-ship THEN "S" ELSE "R".

        else
          assign
           cust.n-sales[13]          = cust.n-sales[13] + 1
           cust.n-sales[tran-period] = cust.n-sales[tran-period] + 1
           xoe-ord.stat              = "R".

        xoe-ord.tot-ord = xoe-ord.t-revenue.
      end.
    END.
  END.

  IF ip-list-post EQ "list" THEN DO:
    if v-detail then put  "---------" at 56.

    put "------------" at 89 skip.

    if v-detail THEN do:
      put "Total New Orders:" to 53
          v-g-tot-qty         to 64
          v-g-tot-ord         to 100.

      put "  Deleted Orders:" to 53
          v-g-del-tot-qty     to 64
          v-g-del-tot-ord     to 100.

      put "    Grand Totals:"                                     to 53
          (v-g-tot-qty + v-g-del-tot-qty) format "->>,>>>,>>9"    to 64
          (v-g-tot-ord + v-g-del-tot-ord) format "->>,>>>,>>9.99" to 100.
    end.

    ELSE do:
      put "Total New Orders:" to 68
          v-g-tot-ord         to 100.
      put "  Deleted Orders:" to 68
          v-g-del-tot-ord     to 100.
      put "    Grand Totals:"                                     to 68
          (v-g-tot-ord + v-g-del-tot-ord) format "->>,>>>,>>9.99" to 100.
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
     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
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
/* ------------------------------------------------- oe/rep/neword.p 8/93 rd  */
/* Order Entry - Edit Register                                                */
/*  FOR: Order Status of - (N)ew, (A)pproved Credit, (U)pdated, (D)eleted,    */
/*                          & (C)losed                                        */
/* -------------------------------------------------------------------------- */

format header
  "CUST.#   Name                          INVOICE# INV.DATE"
  "        AMOUNT  LINE DESCRIPTION" skip FILL("_",130) format "x(130)"
  with no-labels no-box no-underline frame f-top page-top STREAM-IO width 132.


def var head as char format 'X(78)' no-undo.
if not posting then
  head = "C U S T O M E R   I N V O I C E   E D I T   R E G I S T E R".
else
  head = "I N V O I C E   P O S T I N G".


view frame ireg.
pause 0.
 {sys/inc/period.i}

if not posting then do:
  assign
   v-s-inv-no = 0
   v-e-inv-no = 999999.
  update v-sort with frame ar-inved.
end.
else do:
  assign
   v-s-inv-no = 0
   v-e-inv-no = 999999.
  for each ar-inv
      where ar-inv.company eq cocode
        and ar-inv.posted  eq no
        and ar-inv.printed eq true
        and ar-inv.loc     ne "Zqw"
      use-index posted by inv-no:
    v-s-inv-no = ar-inv.inv-no.
    leave.
  end.

  v-sort = no.
  update v-s-inv-no v-e-inv-no v-s-date v-e-date with frame ar-inv-range.
end.

{sys/inc/print2.i}
 assign
   str-tit2 = c-win:title 
   {sys/inc/ctrtext.i str-tit2 112}

   str-tit3 = "Period " + string(tran-period,"99") + " - " +
              IF AVAIL period THEN
                (string(period.pst) + " to " + string(period.pend)) ELSE ""
   {sys/inc/ctrtext.i str-tit3 132}

   v-detail   = tb_detailed
   v-postable = NO.


  {sys/inc/print1.i}

  {sys/inc/outprint.i value(lines-per-page)}

  if td-show-parm then run show-param.

  display with frame r-top.
 for each cust {sys/ref/custW.i} no-lock,
      each ar-inv
      where ar-inv.company  eq cocode
        and ar-inv.posted   eq no
        and (if posting then ar-inv.printed eq true else true)
        and ar-inv.cust-no  eq cust.cust-no
        and ar-inv.loc      ne "Zqw"
        and ar-inv.inv-no   ge v-s-inv-no
        and ar-inv.inv-no   le v-e-inv-no
        and ar-inv.inv-date ge v-s-date
        and ar-inv.inv-date le v-e-date
      no-lock

      break by (if v-sort then cust.name else "")
            by cust.cust-no
            by ar-inv.inv-no
      with frame f-det:

    put screen row lorow columns 70 string(ar-inv.inv-no,">>>>>9") .
    if first-of(cust.cust-no) then put cust.cust-no space(1) cust.name.

    put ar-inv.inv-no to 47
        ar-inv.inv-date at 49
        ar-inv.net      at 58.

    assign
     v2 = v2 + net
     v1 = v1 + ar-inv.disc-taken.

    for each ar-invl
        where ar-invl.x-no eq ar-inv.x-no
        no-lock break by ar-invl.line
        with frame f-det no-box no-labels width 132:

      {sys/inc/gldstsum.i ar-invl.actnum "-1 * ar-invl.amt"}

      put screen row lorow columns 77 string(ar-invl.line,">9") .
      put ar-invl.line format ">>9" at 75
          ar-invl.i-name at 79 space(1)
          ar-invl.amt skip.
      if ar-invl.i-dscr ne "" then put ar-invl.i-dscr at 79 skip.
    end.

    if ar-inv.freight ne 0 then do:
      put "FREIGHT" at 79 space(1)
          ar-inv.freight format "->>,>>>,>>9.99" at 110 skip.

      {sys/inc/bldwkfl.i xar-freight "-1 * ar-inv.freight"}
    end.

    if ar-inv.tax-amt ne 0 then do:
      find first stax
          {sys/ref/staxW.i}
            and stax.tax-group eq ar-inv.tax-code
          no-lock no-error.

      put "TAX" at 79 space(1)
          ar-inv.tax-amt format "->>,>>>,>>9.99" at 110 skip.

      if avail stax then do:
        def var ws_taxacct as char no-undo.
        def var v-jd-taxamt as dec no-undo.
        def var v-tax-rate as dec no-undo decimals 10 extent 4.

        v-tax-rate = 0.

        do i = 1 to 3:
          v-tax-rate[i] = stax.tax-rate[i].

          if stax.company eq "yes" and i gt 1 then
          do k = 1 to i - 1:
            v-tax-rate[i] = v-tax-rate[i] +
                            (v-tax-rate[i] * (stax.tax-rate[k] / 100)).
          end.
        end.

        v-tax-rate[4] = v-tax-rate[1] + v-tax-rate[2] + v-tax-rate[3].

        do i = 1 to 3:
          find first account
              where account.company eq cocode
                and account.actnum  eq stax.tax-acc[i]
              no-lock no-error.
          assign    
           ws_taxacct  = if avail account then stax.tax-acc[i] else xar-stax
           v-jd-taxamt = v-tax-rate[i] / v-tax-rate[4] * ar-inv.tax-amt.

          {sys/inc/bldwkfl.i ws_taxacct "-1 * v-jd-taxamt"}
        end.
      end. /* avail stax */    

      else do:
        {sys/inc/bldwkfl.i xar-stax "-1 * ar-inv.tax-amt"}
      end.
    end.  /* non-zero tax amount */
    if last-of(cust.cust-no) then do:
      display  "*  CUSTOMER TOTALS" to 56 v2 at 58 " *" skip(1)
          with frame vtot{&frame} no-box no-labels width 132.
      g1 = g1 + v1.
      g2 = g2 + v2.
      v1 = 0.
      v2 = 0.
    end.
    g3 = g3 + ar-inv.tax-amt.
    g4 = g4 + ar-inv.freight.
  end. /* each invoice */

  display "** GRAND TOTAL  "  to 54  g2 at 58 " **"
      with no-labels no-underline width 132 frame GT.

  hide frame f-top.

  str-tit3 = string(udate) + " - Period " + string(uperiod,"99") + " - " +
             "Summary by Account".
  {sys/inc/ctrtext.i str-tit3 132}.
  page.

  do with frame f-distrib:
    ws_net = 0.
    assign
     num-recs = 0
     ws_debit = 0
     ws_credit = 0.
    for each wkdistrib break by wkdistrib.actnum:
      num-recs = num-recs + 1.

      if wkdistrib.actnum eq xar-acct then
        assign
         ws_credit  = ws_credit + (-1 * wkdistrib.amount)
         tot-credit = tot-credit + ( -1 * wkdistrib.amount)
         ws_debit   = ws_debit + (-1 * wkdistrib.amount)
         tot-debit  = tot-debit + (-1 * wkdistrib.amount).
      else
        assign
         ws_debit  = ws_debit + (-1 * wkdistrib.amount)
         tot-debit = tot-debit + (-1 * wkdistrib.amount).
    end.

    if avail wkdistrib then do:
      find account
          where account.company eq cocode
            and account.actnum  eq xar-acct
          no-lock no-error.
      display xar-acct @ wkdistrib.actnum
              account.dscr when avail account
              num-recs
              ws_debit when ws_debit ne 0
              ws_credit when ws_credit ne 0
           with frame f-distrib.
      if not avail account then
        display "*NOT IN ACCOUNT FILE*" @ account.dscr
            with frame f-distrib.
      down 1 with frame f-distrib.
    end.

    for each wkdistrib where wkdistrib.actnum ne xar-acct
        break by wkdistrib.actnum:
      if first-of(wkdistrib.actnum) then
        assign
         num-recs = 0
         ws_debit = 0
         ws_credit = 0.

      num-recs = num-recs + 1.

      assign
       ws_debit   = 0
       ws_credit  = ws_credit + ( -1 * wkdistrib.amount)
       tot-credit = tot-credit + ( -1 * wkdistrib.amount).

      find first account
          where account.company eq cocode
            and account.actnum  eq wkdistrib.actnum
          no-lock no-error.
      if last-of(wkdistrib.actnum) then do:
        display wkdistrib.actnum
                account.dscr when avail account
                num-recs
                ws_debit when ws_debit ne 0
                ws_credit when ws_credit ne 0
            with frame f-distrib.

        if not avail account then
          display "*NOT IN ACCOUNT FILE*" @ account.dscr with frame f-distrib.
        down 1 with frame f-distrib.
      end.
    end. /* for each */

    underline ws_debit ws_credit.
    down 1 with frame f-distrib.
    display "Totals" @ account.dscr
            tot-debit @ ws_debit
            tot-credit @ ws_credit.
    down 1 with frame f-distrib.

    ws_net = tot-debit - tot-credit.

    display "Net" @ account.dscr.
    if ws_net gt 0 then display ws_net @ ws_debit.
    else display (-1 * ws_net) @ ws_credit.
    down 1 with frame f-distrib.
  end.


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

