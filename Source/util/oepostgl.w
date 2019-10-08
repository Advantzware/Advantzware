&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*----------------------------------------------------------------------*/
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
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

DEF VAR v-invalid AS LOG NO-UNDO.
DEF VAR ll-warned AS LOG NO-UNDO.

DO TRANSACTION:
   {sys/inc/postdate.i}
END.

{oe/invwork.i NEW}

def new shared var v-ar-acct like ar-ctrl.receivables.
def new shared var v-ar-freight like ar-ctrl.freight.
def new shared var v-ar-stax like ar-ctrl.stax.
def new shared var v-ar-sales like ar-ctrl.sales.
def new shared var v-ar-disc like ar-ctrl.discount.
def new shared var v-return as log init no.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 tran-date begin_inv end_inv ~
btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period begin_inv end_inv 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE begin_inv AS INTEGER FORMAT ">>>>>>>>>" INITIAL 0 
     LABEL "Beginning Invoice #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_inv AS INTEGER FORMAT ">>>>>>>>>" INITIAL 999999999 
     LABEL "Ending Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE tran-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Post Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tran-period AS INTEGER FORMAT ">>":U INITIAL 0 
     LABEL "Period" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 96 BY 9.05.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tran-date AT ROW 2.43 COL 35.6 COLON-ALIGNED
     tran-period AT ROW 3.62 COL 35.6 COLON-ALIGNED
     begin_inv AT ROW 5.1 COL 25.6 COLON-ALIGNED HELP
          "Enter Beginning Invoice Number"
     end_inv AT ROW 5.1 COL 66.6 COLON-ALIGNED HELP
          "Enter Ending Invoice Number"
     btn-process AT ROW 11 COL 26
     btn-cancel AT ROW 11 COL 57
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .95 AT ROW 1.24 COL 4
          FONT 4
     RECT-17 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96.8 BY 12.14
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
         TITLE              = "Post OE Invoice to G/L"
         HEIGHT             = 12.24
         WIDTH              = 96.8
         MAX-HEIGHT         = 26.62
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 26.62
         VIRTUAL-WIDTH      = 160
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         FONT               = 6
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
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_inv:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_inv:PRIVATE-DATA IN FRAME FRAME-A     = 
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





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Post OE Invoice to G/L */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Post OE Invoice to G/L */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_inv C-Win
ON LEAVE OF begin_inv IN FRAME FRAME-A /* Beginning Invoice # */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
  DEF VAR v-process AS LOG INIT NO NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN tran-period tran-date begin_inv END_inv.
  END.

  MESSAGE "Are you sure you want to " + TRIM(c-win:TITLE) + " " +
           "?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE v-process.

  IF v-process THEN RUN run-process.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_inv C-Win
ON LEAVE OF end_inv IN FRAME FRAME-A /* Ending Invoice# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-date C-Win
ON LEAVE OF tran-date IN FRAME FRAME-A /* Post Date */
DO:
  assign {&self-name}.

  if lastkey ne -1 then do:
    run check-date.
    if v-invalid then return no-apply.
    RUN valid-date NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-date C-Win
ON VALUE-CHANGED OF tran-date IN FRAME FRAME-A /* Post Date */
DO:
  ll-warned = NO.
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

  RUN enable_UI.

  IF postdate-log THEN DO:
      ASSIGN
         tran-date:SCREEN-VALUE = STRING(TODAY)
         tran-date              = TODAY.

      RUN check-date.
  END.

  ELSE
     ASSIGN
        tran-date:SCREEN-VALUE   = ""
        tran-period:SCREEN-VALUE = "".

  {methods/nowait.i}

  DO WITH FRAME {&frame-name}:

    {custom/usrprint.i}
    APPLY "entry" TO tran-date.

  END.

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
   if avail period then do:
       IF NOT period.pstat THEN DO:
          MESSAGE "Period Already Closed. " VIEW-AS ALERT-BOX ERROR.
          v-invalid = YES.
       END.
        tran-period:SCREEN-VALUE = string(period.pnum).
    END.

    ELSE DO:
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
  DISPLAY tran-date tran-period begin_inv end_inv 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 tran-date begin_inv end_inv btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-gl C-Win 
PROCEDURE post-gl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN
   v-ar-acct = ""
   v-ar-freight = ""
   v-ar-stax = ""
   v-ar-sales = ""
   v-ar-disc = ""
   v-return = NO.

DEF VAR v-u-cost LIKE ar-invl.cost NO-UNDO.
DEF VAR v-t-cost LIKE ar-invl.t-cost NO-UNDO.
def var v-inv-disc as dec format "->>,>>9.99".
def var v-line-tot like inv-line.t-price.
def var v-misc-tot like ar-invl.amt.
def var v-cas-cnt like itemfg.case-count.
def var v-cost as dec extent 4.
def var v-invl-pric as dec.
def var v-tax-rate as dec extent 4.
DEFINE VARIABLE cCostUOM AS CHARACTER.
DEFINE VARIABLE cCostSource AS CHARACTER.

FOR EACH ar-inv
    WHERE ar-inv.company EQ g_company
      AND ar-inv.posted  EQ YES
      AND ar-inv.inv-no  GE begin_inv
      AND ar-inv.inv-no  LE END_inv
    NO-LOCK,

    FIRST ar-ledger
    WHERE ar-ledger.company EQ ar-inv.company
      AND ar-ledger.ref-num EQ "INV# " + string(ar-inv.inv-no)
    NO-LOCK,

    FIRST period
    WHERE period.company EQ ar-ledger.company
      AND period.pstat   EQ YES
      AND period.pst     LE ar-ledger.tr-date
      AND period.pend    GE ar-ledger.tr-date
    NO-LOCK

    BREAK BY tr-num:

        ASSIGN
         cocode     = ar-ledger.company
         v-inv-disc = 0
         v-line-tot = 0
         v-misc-tot = 0.

        RUN oe/getacct.p.

        FIND FIRST ar-ctrl WHERE ar-ctrl.company EQ cocode NO-LOCK.

/************ line ITEMS ************************************************/
        FOR EACH ar-invl
            WHERE ar-invl.x-no EQ ar-inv.x-no
              AND ar-invl.misc EQ NO
            NO-LOCK,

            FIRST itemfg
            {sys/look/itemfgrlW.i}
              AND itemfg.i-no eq ar-invl.i-no
            NO-LOCK,

            FIRST fgcat
            WHERE fgcat.company EQ cocode
              and fgcat.procat  EQ itemfg.procat
            NO-LOCK:

          FIND FIRST uom
              WHERE uom.uom  EQ ar-invl.pr-uom
                AND uom.mult NE 0
              NO-LOCK NO-ERROR.

          FIND FIRST oe-ordl
               WHERE oe-ordl.company EQ cocode
                 AND oe-ordl.ord-no  EQ ar-invl.ord-no
                 AND oe-ordl.i-no    EQ ar-invl.i-no
               USE-INDEX ord-no NO-LOCK NO-ERROR.

          ASSIGN
           v-cas-cnt = if ar-invl.cas-cnt ne 0 then
                         ar-invl.cas-cnt
                       else
                       if avail oe-ordl and oe-ordl.cas-cnt ne 0 then
                         oe-ordl.cas-cnt
                       else
                       if avail itemfg and itemfg.case-count ne 0 then
                         itemfg.case-count
                       else 1.

          run oe/GetCostInvl.p (ROWID(ar-invl),
                             output v-cost[1], output v-cost[2],
                             output v-cost[3], output v-cost[4],
                             output v-u-cost, OUTPUT cCostUOM, 
                             output v-t-cost, OUTPUT cCostSource).

          run oe/invposty.p (ar-inv.inv-no, ar-invl.i-no, ar-invl.inv-qty,
                             "M", v-cost[1], v-cost[2], v-cost[3], v-cost[4]).

          create tt-report.
          assign
           tt-report.term-id = ""
           tt-report.key-01  = "work-line"
           tt-report.key-02  = if avail fgcat and fgcat.glacc ne ""
                               then fgcat.glacc else v-ar-sales
           tt-report.key-03  = string(ar-inv.inv-no,"999999")
           tt-report.key-04  = ar-invl.i-no
           v-invl-pric       = ar-invl.amt.

          IF ar-invl.disc NE 0 THEN
            ASSIGN
             v-invl-pric = ROUND((if ar-invl.pr-uom begins "L" then
                                    if ar-invl.inv-qty lt 0 then -1 else 1
                                  else
                                  if ar-invl.pr-uom eq "CS" then
                                    ar-invl.inv-qty / v-cas-cnt
                                  else
                                  if avail uom then
                                    ar-invl.inv-qty / uom.mult
                                  else
                                    ar-invl.inv-qty / 1000) *
                                 ar-invl.unit-pr,2)
             v-inv-disc  = v-inv-disc + (v-invl-pric - ar-invl.amt).

          tt-report.key-05 = STRING(v-invl-pric).

          v-line-tot = v-line-tot + ar-invl.amt.
        END. /* each ar-invl */

  /******************* MISCELLANEOUS ITEMS ***********************************/
  /* Be aware that job nos are not stored in ar-invl records for misc charges*/

        FOR EACH ar-invl
            WHERE ar-invl.x-no     EQ ar-inv.x-no
              AND ar-invl.misc     EQ YES
              AND ar-invl.billable EQ YES
            NO-LOCK:

          create tt-report.
          assign
           tt-report.term-id = ""
           tt-report.key-01  = "work-misc"
           tt-report.key-02  = if ar-invl.actnum ne ""
                               then ar-invl.actnum else v-ar-sales
           tt-report.key-03  = string(ar-inv.inv-no,"999999")
           tt-report.key-04  = ar-invl.prep-charge
           tt-report.key-05  = string(ar-invl.amt).

          v-misc-tot = v-misc-tot + ar-invl.amt.
        END. /* each ar-invl */

        v-post-disc = v-post-disc + v-inv-disc.

  /******************* MISCELLANEOUS ITEMS ***********************************/
        create tt-report.
        assign
         tt-report.term-id = ""
         tt-report.key-01  = "work-disc"
         tt-report.key-02  = string(ar-inv.inv-no,"999999")
         tt-report.key-05  = string(v-inv-disc).

        if ar-inv.tax-amt ne 0 then do:
          if ar-inv.tax-code ne "" then do:
            find first stax
                {sys/ref/stax1W.i}
                  and {sys/ref/taxgroup.i stax} eq ar-inv.tax-code
                no-lock no-error.
            if not avail stax then
            find first stax
                where stax.company = ar-inv.company AND
                stax.tax-group eq ar-inv.tax-code
                no-lock no-error.

            if avail stax then do:
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
                v-tax-rate[i] = round(v-tax-rate[i] / v-tax-rate[4] *
                                      ar-inv.tax-amt,2).
              end.

              v-tax-rate[4] = v-tax-rate[1] + v-tax-rate[2] + v-tax-rate[3].

              if ar-inv.tax-amt ne v-tax-rate[4] then
                v-tax-rate[1] = v-tax-rate[1] +
                                (ar-inv.tax-amt - v-tax-rate[4]).

              do i = 1 to 3:
                find first account
                    where account.company eq cocode
                      and account.actnum  eq stax.tax-acc[i]
                    no-lock no-error.

                if avail account then do:
                  create tt-report.
                  assign
                   tt-report.term-id = ""
                   tt-report.key-01  = "work-tax"
                   tt-report.key-02  = account.actnum
                   tt-report.key-03  = string(ar-inv.inv-no,"999999")
                   tt-report.key-04  = ar-inv.tax-code
                   tt-report.key-05  = string(v-tax-rate[i]).
                end. /* avail account */


              end. /* 1 to 3 */

            end. /* avail stax */
          end.

          else do:
            find first account
                where account.company eq cocode
                  and account.actnum  eq v-ar-stax
                no-lock no-error.
            create tt-report.
            assign
             tt-report.term-id = ""
             tt-report.key-01  = "work-tax"
             tt-report.key-02  = account.actnum
             tt-report.key-03  = string(ar-inv.inv-no,"999999")
             tt-report.key-05  = string(ar-inv.tax-amt).
          end.
        end.

        v-post-total = v-post-total + ar-inv.gross.

        /** if Freight Is Billable then Post to GL **/
        if ar-inv.f-bill then do:
          v-post-freight = v-post-freight - ar-inv.freight.

          create tt-report.
          assign
           tt-report.term-id = ""
           tt-report.key-01  = "work-freight"
           tt-report.key-02  = string(ar-inv.inv-no,"999999")
           tt-report.key-05  = string(- ar-inv.freight).
        end.

        if ar-inv.terms eq "CASH" then do:
          assign
           v-post-cash  = v-post-cash  + ar-inv.gross
           v-post-total = v-post-total - ar-inv.gross.

          create tt-report.
          assign
           tt-report.term-id = ""
           tt-report.key-01  = "work-cash"
           tt-report.key-02  = string(ar-inv.inv-no,"999999")
           tt-report.key-05  = string(ar-inv.gross).
        end.

  IF LAST-OF(tr-num) THEN DO:
    for each tt-report
        where tt-report.term-id eq ""
          and tt-report.key-01  eq "work-line"
        no-lock
        break by tt-report.key-02:

      accumulate dec(tt-report.key-05) (total by tt-report.key-02).

      if last-of(tt-report.key-02) then do:
        create gltrans.
        assign
         gltrans.company = cocode
         gltrans.actnum  = tt-report.key-02
         gltrans.jrnl    = "OEINV"
         gltrans.tr-dscr = "ORDER ENTRY INVOICE LINES"
         gltrans.tr-date = ar-ledger.tr-date
         gltrans.tr-amt  = - (accumulate total by tt-report.key-02 dec(tt-report.key-05))
         gltrans.period  = period.pnum
         gltrans.trnum   = ar-ledger.tr-num.
      end. /* last actnum */
    end. /* each work-line */
                                              /** POST MISC. TO G/L TRANS **/
    for each tt-report
        where tt-report.term-id eq ""
          and tt-report.key-01  eq "work-misc"
        no-lock
        break by tt-report.key-02:

      accumulate dec(tt-report.key-05) (total by tt-report.key-02).

      if last-of(tt-report.key-02) then do:
        create gltrans.
        assign
         gltrans.company = cocode
         gltrans.jrnl    = "OEINV"
         gltrans.tr-dscr = "ORDER ENTRY INVOICE MISC."
         gltrans.tr-date = ar-ledger.tr-date
         gltrans.actnum  = tt-report.key-02
         gltrans.tr-amt  = - (accumulate total by tt-report.key-02 dec(tt-report.key-05))
         gltrans.period  = period.pnum
         gltrans.trnum   = ar-ledger.tr-num.
      end. /* last actnum */
    end. /* each work-misc */
                                           /** POST SALES TAX TO G/L TRANS **/
    for each tt-report
        where tt-report.term-id eq ""
          and tt-report.key-01  eq "work-tax"
        no-lock
        break by tt-report.key-02:

      accumulate dec(tt-report.key-05) (total by tt-report.key-02).

      if last-of(tt-report.key-02) then do:
        create gltrans.
        assign
         gltrans.company = cocode
         gltrans.actnum  = tt-report.key-02
         gltrans.jrnl    = "OEINV"
         gltrans.tr-dscr = "ORDER ENTRY INVOICE TAX"
         gltrans.tr-date = ar-ledger.tr-date
         gltrans.tr-amt  = - (accumulate total by tt-report.key-02 dec(tt-report.key-05))
         gltrans.period  = period.pnum
         gltrans.trnum   = ar-ledger.tr-num.
      end. /* last actnum */
    end. /* each work-tax */

    for each work-job break by work-job.actnum:
      create gltrans.
      assign
       gltrans.company = cocode
       gltrans.actnum  = work-job.actnum
       gltrans.jrnl    = "OEINV"
       gltrans.tr-date = ar-ledger.tr-date
       gltrans.period  = period.pnum
       gltrans.trnum   = ar-ledger.tr-num.

      if work-job.fg then
        assign
         gltrans.tr-amt  = - work-job.amt
         gltrans.tr-dscr = "ORDER ENTRY INVOICE FG".
      else
        assign
         gltrans.tr-amt  = work-job.amt
         gltrans.tr-dscr = "ORDER ENTRY INVOICE COGS".
    end. /* each work-job */

                                          /** POST FREIGHT TO G/L TRANS **/
    create gltrans.
    assign
     gltrans.company = cocode
     gltrans.actnum  = v-ar-freight
     gltrans.jrnl    = "OEINV"
     gltrans.tr-dscr = "ORDER ENTRY INVOICE FREIGHT"
     gltrans.tr-date = ar-ledger.tr-date
     gltrans.tr-amt  = v-post-freight
     gltrans.period  = period.pnum
     gltrans.trnum   = ar-ledger.tr-num.

                                           /** POST DISCOUNT TO G/L TRANS **/
    create gltrans.
    assign
     gltrans.company = cocode
     gltrans.actnum  = v-ar-disc
     gltrans.jrnl    = "OEINV"
     gltrans.tr-dscr = "ORDER ENTRY INVOICE DISCOUNT"
     gltrans.tr-date = ar-ledger.tr-date
     gltrans.tr-amt  = v-post-disc
     gltrans.period  = period.pnum
     gltrans.trnum   = ar-ledger.tr-num.
                                           /** POST CASH TO G/L TRANS **/
    if v-post-cash ne 0 then do:
      create gltrans.
      assign
       gltrans.company = cocode
       gltrans.actnum  = ar-ctrl.cash-act
       gltrans.jrnl    = "CASHR"
       gltrans.tr-dscr = "CASH RECEIPT - INVOICE"
       gltrans.tr-date = ar-ledger.tr-date
       gltrans.tr-amt  = v-post-cash
       gltrans.period  = period.pnum
       gltrans.trnum   = ar-ledger.tr-num.
    end.
                                                  /** OFFSET ENTRY TO G/L **/
    create gltrans.
    assign
     gltrans.company = cocode
     gltrans.actnum  = v-ar-acct
     gltrans.jrnl    = "OEINV"
     gltrans.tr-dscr = "ORDER ENTRY INVOICE"
     gltrans.tr-date = ar-ledger.tr-date
     gltrans.tr-amt  = v-post-total
     gltrans.period  = period.pnum
     gltrans.trnum   = ar-ledger.tr-num.
  END.
END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
run check-date.
  if v-invalid then return no-apply.

  SESSION:SET-WAIT-STATE("GENERAL").

  RUN post-gl.

  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

  SESSION:SET-WAIT-STATE("").

  MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-date C-Win 
PROCEDURE valid-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR ll AS LOG NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:
     IF NOT ll-warned THEN DO:
        ll = NO.

        FOR EACH period NO-LOCK
            WHERE period.company EQ cocode
              AND period.pst     LE TODAY
              AND period.pend    GE TODAY
            BY period.pst:

          IF period.pst  GT DATE(tran-date:SCREEN-VALUE) OR
             period.pend LT DATE(tran-date:SCREEN-VALUE) THEN DO:
            ll = YES.
            MESSAGE TRIM(tran-date:LABEL) + " is not in current period, " +
                    "would you like to re-enter..."
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE ll.
          END.

          IF ll THEN DO:
            APPLY "entry" TO tran-date.
            RETURN ERROR.
          END.

          LEAVE.
        END.

        ll-warned = YES.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

