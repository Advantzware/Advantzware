&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-inve&p.w

  Description: Invoice Edit List & Posting

  Input Parameters: ip-post

  Output Parameters:
      <none>

  Author: JLF

  Created: 05/07/02

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

def var v-pct          as   dec  format ">9.99" NO-UNDO.
def var v-date         as   date format "99/99/9999" init today NO-UNDO.

def var v-amt          as   dec NO-UNDO.
def var v-cr-db-amt    as   dec NO-UNDO.
def var v-disc-amt     as   dec NO-UNDO.
def var save_id        as   recid NO-UNDO.
DEF VAR udate AS DATE INIT TODAY NO-UNDO.
DEF VAR uperiod AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 tran-date fin-apr end_cust begin_cust ~
btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date fin-apr end_cust begin_cust 

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

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Cust #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Cust #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fin-apr AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Finance Charge APR" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE tran-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "As of" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 5.71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tran-date AT ROW 1.95 COL 37 COLON-ALIGNED
     fin-apr AT ROW 3.43 COL 37 COLON-ALIGNED
     end_cust AT ROW 5.14 COL 62 COLON-ALIGNED HELP
          "Enter Ending Customer Number" WIDGET-ID 6
     begin_cust AT ROW 5.19 COL 22 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 8
     btn-ok AT ROW 18.38 COL 23
     btn-cancel AT ROW 18.38 COL 58
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 19.71
         FONT 6.


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
         TITLE              = "Finance Charge Creation"
         HEIGHT             = 20.19
         WIDTH              = 95.4
         MAX-HEIGHT         = 46.48
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 46.48
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
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tran-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

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
ON END-ERROR OF C-Win /* Finance Charge Creation */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Finance Charge Creation */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Cust # */
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
   DEF VAR choice AS LOG NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:
     ASSIGN {&DISPLAYED-OBJECTS}.
   END.

   choice = yes.
   message "Are you sure you wish to create finance charges?" VIEW-AS ALERT-BOX question
        BUTTON YES-NO update choice.
   if choice THEN RUN bill-finance.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Cust # */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fin-apr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fin-apr C-Win
ON LEAVE OF fin-apr IN FRAME FRAME-A /* Finance Charge APR */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-date C-Win
ON LEAVE OF tran-date IN FRAME FRAME-A /* As of */
DO:
  assign {&self-name}.
  DO with frame {&frame-name}:

    find first period                   
        where period.company eq cocode
          and period.pst     le tran-date
          and period.pend    ge tran-date
        no-lock no-error.
    if avail period then uperiod = (period.pnum).

    ELSE DO:
      message "No Defined Period Exists for" tran-date view-as alert-box error.
      RETURN NO-APPLY.
    end.
  END.

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

    IF access-close THEN DO:
       APPLY "close" TO THIS-PROCEDURE.
       RETURN .
    END.

  tran-date = TODAY.

  RUN enable_UI.

  {methods/nowait.i}
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images1.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images1.p */
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bill-finance C-Win 
PROCEDURE bill-finance :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  SESSION:SET-WAIT-STATE("general").

  DEF VAR X AS INT NO-UNDO.
  ASSIGN v-date = tran-date
         v-pct = fin-apr.

  FOR EACH cust NO-LOCK
      WHERE cust.company EQ cocode
        /* gdm - 11050901 */
        AND cust.cust-no GE begin_cust
        AND cust.cust-no LE end_cust
        /* gdm - 11050901 */
        AND cust.fin-chg EQ YES
        AND LOOKUP(cust.active,"A,E") GT 0:

    v-amt = 0.

    for each ar-inv
        where ar-inv.company  eq cocode
          and ar-inv.posted   eq yes
          and ar-inv.cust-no  eq cust.cust-no
          and ar-inv.inv-date le v-date
          and ar-inv.terms    ne "CASH"
          and ar-inv.terms    ne "FCHG"
        no-lock,

        first terms
        where terms.t-code    eq ar-inv.terms
          and v-date          gt ar-inv.inv-date + terms.net-days
        no-lock:

      status default "Processing...  Customer: " + trim(cust.cust-no) + "   " +
                                     "Invoice: " + trim(string(ar-inv.inv-no)).

      /* Inserted because AR stores gross wrong */
      v-amt = v-amt +
              if ar-inv.net eq ar-inv.gross + ar-inv.freight + ar-inv.tax-amt
              then ar-inv.net else ar-inv.gross.

      for each ar-cashl
          where ar-cashl.company  eq ar-inv.company
            and ar-cashl.posted   eq yes
            and ar-cashl.cust-no  eq ar-inv.cust-no
            and ar-cashl.inv-no   eq ar-inv.inv-no
          use-index inv-no no-lock,

          each ar-cash
          where ar-cash.c-no       eq ar-cashl.c-no
            and ar-cash.check-date le v-date
          use-index c-no no-lock:

        if ar-cashl.memo then
          if ar-cashl.dscr MATCHES "*CREDIT MEMO CREATED FROM OE RETURN*" then
            v-amt = v-amt - ar-cashl.amt-disc.
          else
          if ar-cashl.amt-paid + ar-cashl.amt-disc ne 0 then
            v-amt = v-amt + (ar-cashl.amt-paid + ar-cashl.amt-disc).
          else
            v-amt = v-amt + (ar-cashl.amt-paid + (- (ar-cashl.amt-disc))).
        else
          v-amt = v-amt + ((ar-cashl.amt-paid * -1) + (ar-cashl.amt-disc * -1)).
      end.
    end. /* each ar-inv */

    for each ar-cash
        where ar-cash.company     eq cust.company
          and ar-cash.cust-no     eq cust.cust-no
          and ar-cash.posted      eq yes
          and (ar-cash.check-date le v-date or
               ar-cash.check-date eq ?)
        use-index ar-cash no-lock,

        each ar-cashl
        where ar-cashl.c-no       eq ar-cash.c-no
          and ar-cashl.inv-no     eq 0
          and ar-cashl.posted     eq yes
        use-index c-no no-lock:

      if ar-cashl.inv-no ne 0 then do:
        find first ar-inv
            where ar-inv.company     eq cust.company
              and ar-inv.inv-no      eq ar-cashl.inv-no
              and ar-inv.inv-date    gt v-date
            use-index inv-no no-lock no-error.
        if not avail ar-inv then next.
      end.

      if ar-cashl.memo then do:
         /* CTS CM/DM signs are reversed *****************************/
        if (ar-cashl.amt-paid + ar-cashl.amt-disc) lt 0 then
          assign
           v-cr-db-amt = ar-cashl.amt-paid
           v-disc-amt = ar-cashl.amt-disc.

        else
        if (ar-cashl.amt-paid + ar-cashl.amt-disc) gt 0 then
          assign
           v-cr-db-amt = ar-cashl.amt-paid
           v-disc-amt = ar-cashl.amt-disc.
      end.

      else
        assign
         v-cr-db-amt = ar-cashl.amt-paid * -1
         v-disc-amt  = ar-cashl.amt-disc * -1.

      v-amt = v-amt + v-cr-db-amt - v-disc-amt.
    end. /* for each ar-cashl record */

    if v-amt * (v-pct / 100 / 12) gt 0 then do transaction:
      find last ar-inv use-index x-no no-lock no-error.
      if avail ar-inv then x = ar-inv.x-no + 1.

      find first shipto
          where shipto.company eq cocode
            and shipto.cust-no eq cust.cust-no
            and shipto.ship-id eq cust.cust-no
          no-lock no-error.
      if not avail shipto then
      find first shipto
          where shipto.company eq cocode
            and shipto.cust-no eq cust.cust-no
          no-lock no-error.      

      create ar-inv.
      assign
       ar-inv.type     = "FC"
       ar-inv.terms    = "FCHG"
       ar-inv.terms-d  = "Finance Charge"
       ar-inv.x-no     = x
       ar-inv.company  = cocode
       ar-inv.inv-date = v-date
       ar-inv.due-date = v-date
       ar-inv.cust-no  = cust.cust-no
       ar-inv.cust-name = cust.name
       ar-inv.addr[1]  = cust.addr[1]
       ar-inv.addr[2]  = cust.addr[2]
       ar-inv.city     = cust.city
       ar-inv.state    = cust.state
       ar-inv.zip      = cust.zip
       ar-inv.ship-id  = if avail shipto then shipto.ship-id else ""
       ar-inv.carrier  = cust.carrier
       ar-inv.period   = uperiod
       ar-inv.net      = v-amt * (v-pct / 100 / 12)
       ar-inv.due      = ar-inv.net
       ar-inv.gross    = ar-inv.net
       ar-inv.curr-code[1] = cust.curr-code    .

      find first ar-ctrl where ar-ctrl.company eq cocode exclusive-lock.

      assign
       ar-inv.inv-no    = ar-ctrl.last-inv + 1
       ar-ctrl.last-inv = ar-inv.inv-no.

     /* {ar/ar-invl.a} */
     x = 1.

     find last ar-invl where ar-invl.x-no = ar-inv.x-no use-index x-no
     no-lock no-error.
     if available ar-invl then x = ar-invl.line + 1.

     create ar-invl.
     ASSIGN ar-invl.x-no    = ar-inv.x-no
            ar-invl.company = cocode
            ar-invl.cust-no = ar-inv.cust-no
            ar-invl.inv-no  = ar-inv.inv-no
            ar-invl.qty     = 0
            ar-invl.line    = x
            ar-invl.po-no   = ar-inv.po-no.

      assign
       ar-invl.qty        = 1
       ar-invl.unit-pr    = ar-inv.net
       ar-invl.pr-qty-uom = "EA"
       ar-invl.amt        = ar-inv.net
       ar-invl.actnum     = ar-ctrl.onac.

    end.
  END.
  SESSION:SET-WAIT-STATE("").
  MESSAGE "Finance Charge Creation Process is completed." VIEW-AS ALERT-BOX.

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
  DISPLAY tran-date fin-apr end_cust begin_cust 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-7 tran-date fin-apr end_cust begin_cust btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

