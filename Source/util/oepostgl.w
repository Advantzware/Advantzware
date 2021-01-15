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

ASSIGN
 cocode = gcompany
 locode = gloc.

DEF VAR v-invalid AS LOG NO-UNDO.
DEF VAR ll-warned AS LOG NO-UNDO.

DO TRANSACTION:
   {sys/inc/postdate.i}
END.

{oe/invwork.i NEW}

DEF NEW SHARED VAR v-ar-acct LIKE ar-ctrl.receivables.
DEF NEW SHARED VAR v-ar-freight LIKE ar-ctrl.freight.
DEF NEW SHARED VAR v-ar-stax LIKE ar-ctrl.stax.
DEF NEW SHARED VAR v-ar-sales LIKE ar-ctrl.sales.
DEF NEW SHARED VAR v-ar-disc LIKE ar-ctrl.discount.
DEF NEW SHARED VAR v-return AS LOG INIT NO.

DEFINE VARIABLE dCostFreight AS DECIMAL NO-UNDO.
DEFINE VARIABLE dCostWarehouse AS DECIMAL NO-UNDO.
DEFINE VARIABLE dCostDeviation AS DECIMAL NO-UNDO.
DEFINE VARIABLE dCostManufacture AS DECIMAL NO-UNDO.

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
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = YES
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         FONT               = 6
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
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
THEN C-Win:HIDDEN = NO.

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
  ASSIGN {&self-name}.
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
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-date C-Win
ON LEAVE OF tran-date IN FRAME FRAME-A /* Post Date */
DO:
  ASSIGN {&self-name}.

  IF LASTKEY NE -1 THEN DO:
    RUN check-date.
    IF v-invalid THEN RETURN NO-APPLY.
    RUN valid-date NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
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
  ASSIGN {&self-name}.
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
   DO WITH FRAME {&frame-name}:
    v-invalid = NO.

    FIND FIRST period                   
        WHERE period.company EQ cocode
          AND period.pst     LE tran-date
          AND period.pend    GE tran-date
        NO-LOCK NO-ERROR.
   IF AVAIL period THEN DO:
       IF NOT period.pstat THEN DO:
          MESSAGE "Period Already Closed. " VIEW-AS ALERT-BOX ERROR.
          v-invalid = YES.
       END.
        tran-period:SCREEN-VALUE = STRING(period.pnum).
    END.

    ELSE DO:
      MESSAGE "No Defined Period Exists for" tran-date VIEW-AS ALERT-BOX ERROR.
      v-invalid = YES.
    END.
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
DEF VAR v-inv-disc AS DEC FORMAT "->>,>>9.99".
DEF VAR v-line-tot LIKE inv-line.t-price.
DEF VAR v-misc-tot LIKE ar-invl.amt.
DEF VAR v-cas-cnt LIKE itemfg.case-count.
DEF VAR v-cost AS DEC EXTENT 4.
DEF VAR v-invl-pric AS DEC.
DEF VAR v-tax-rate AS DEC EXTENT 4.
DEFINE VARIABLE cCostUOM AS CHARACTER.
DEFINE VARIABLE cCostSource AS CHARACTER.
DEFINE VARIABLE cRecAccount AS CHARACTER NO-UNDO.

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
        
     
      cRecAccount = STRING(DYNAMIC-FUNCTION("GL_GetAccountAR", ar-ledger.company, ar-ledger.cust-no)).      

        FIND FIRST ar-ctrl WHERE ar-ctrl.company EQ cocode NO-LOCK.

/************ line ITEMS ************************************************/
        FOR EACH ar-invl
            WHERE ar-invl.x-no EQ ar-inv.x-no
              AND ar-invl.misc EQ NO
            NO-LOCK,

            FIRST itemfg
            {sys/look/itemfgrlW.i}
              AND itemfg.i-no EQ ar-invl.i-no
            NO-LOCK,

            FIRST fgcat
            WHERE fgcat.company EQ cocode
              AND fgcat.procat  EQ itemfg.procat
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
           v-cas-cnt = IF ar-invl.cas-cnt NE 0 THEN
                         ar-invl.cas-cnt
                       ELSE
                       IF AVAIL oe-ordl AND oe-ordl.cas-cnt NE 0 THEN
                         oe-ordl.cas-cnt
                       ELSE
                       IF AVAIL itemfg AND itemfg.case-count NE 0 THEN
                         itemfg.case-count
                       ELSE 1.

          RUN oe/GetCostInvl.p (ROWID(ar-invl),
                             OUTPUT v-cost[1], OUTPUT v-cost[2],
                             OUTPUT v-cost[3], OUTPUT v-cost[4],
                             OUTPUT v-u-cost, OUTPUT cCostUOM, 
                             OUTPUT v-t-cost, OUTPUT cCostSource,
                             OUTPUT dCostFreight, OUTPUT dCostWarehouse, OUTPUT dCostDeviation, OUTPUT dCostManufacture).

          RUN oe/invposty.p (ar-inv.inv-no, ar-invl.i-no, ar-invl.inv-qty,
                             "M", v-cost[1], v-cost[2], v-cost[3], v-cost[4]).

          CREATE tt-report.
          ASSIGN
           tt-report.term-id = ""
           tt-report.key-01  = "work-line"
           tt-report.key-02  = IF AVAIL fgcat AND fgcat.glacc NE ""
                               THEN fgcat.glacc ELSE v-ar-sales
           tt-report.key-03  = STRING(ar-inv.inv-no,"999999")
           tt-report.key-04  = ar-invl.i-no
           v-invl-pric       = ar-invl.amt.

          IF ar-invl.disc NE 0 THEN
            ASSIGN
             v-invl-pric = ROUND((IF ar-invl.pr-uom BEGINS "L" THEN
                                    IF ar-invl.inv-qty LT 0 THEN -1 ELSE 1
                                  ELSE
                                  IF ar-invl.pr-uom EQ "CS" THEN
                                    ar-invl.inv-qty / v-cas-cnt
                                  ELSE
                                  IF AVAIL uom THEN
                                    ar-invl.inv-qty / uom.mult
                                  ELSE
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

          CREATE tt-report.
          ASSIGN
           tt-report.term-id = ""
           tt-report.key-01  = "work-misc"
           tt-report.key-02  = IF ar-invl.actnum NE ""
                               THEN ar-invl.actnum ELSE v-ar-sales
           tt-report.key-03  = STRING(ar-inv.inv-no,"999999")
           tt-report.key-04  = ar-invl.prep-charge
           tt-report.key-05  = STRING(ar-invl.amt).

          v-misc-tot = v-misc-tot + ar-invl.amt.
        END. /* each ar-invl */

        v-post-disc = v-post-disc + v-inv-disc.

  /******************* MISCELLANEOUS ITEMS ***********************************/
        CREATE tt-report.
        ASSIGN
         tt-report.term-id = ""
         tt-report.key-01  = "work-disc"
         tt-report.key-02  = STRING(ar-inv.inv-no,"999999")
         tt-report.key-05  = STRING(v-inv-disc).

        IF ar-inv.tax-amt NE 0 THEN DO:
          IF ar-inv.tax-code NE "" THEN DO:
            FIND FIRST stax
                {sys/ref/stax1W.i}
                  AND {sys/ref/taxgroup.i stax} EQ ar-inv.tax-code
                NO-LOCK NO-ERROR.
            IF NOT AVAIL stax THEN
            FIND FIRST stax
                WHERE stax.company = ar-inv.company AND
                stax.tax-group EQ ar-inv.tax-code
                NO-LOCK NO-ERROR.

            IF AVAIL stax THEN DO:
              DO i = 1 TO 3:
                v-tax-rate[i] = stax.tax-rate[i].

                IF stax.company EQ "yes" AND i GT 1 THEN
                DO k = 1 TO i - 1:
                  v-tax-rate[i] = v-tax-rate[i] +
                                  (v-tax-rate[i] * (stax.tax-rate[k] / 100)).
                END.
              END.

              v-tax-rate[4] = v-tax-rate[1] + v-tax-rate[2] + v-tax-rate[3].

              DO i = 1 TO 3:
                v-tax-rate[i] = ROUND(v-tax-rate[i] / v-tax-rate[4] *
                                      ar-inv.tax-amt,2).
              END.

              v-tax-rate[4] = v-tax-rate[1] + v-tax-rate[2] + v-tax-rate[3].

              IF ar-inv.tax-amt NE v-tax-rate[4] THEN
                v-tax-rate[1] = v-tax-rate[1] +
                                (ar-inv.tax-amt - v-tax-rate[4]).

              DO i = 1 TO 3:
                FIND FIRST account
                    WHERE account.company EQ cocode
                      AND account.actnum  EQ stax.tax-acc[i]
                    NO-LOCK NO-ERROR.

                IF AVAIL account THEN DO:
                  CREATE tt-report.
                  ASSIGN
                   tt-report.term-id = ""
                   tt-report.key-01  = "work-tax"
                   tt-report.key-02  = account.actnum
                   tt-report.key-03  = STRING(ar-inv.inv-no,"999999")
                   tt-report.key-04  = ar-inv.tax-code
                   tt-report.key-05  = STRING(v-tax-rate[i]).
                END. /* avail account */


              END. /* 1 to 3 */

            END. /* avail stax */
          END.

          ELSE DO:
            FIND FIRST account
                WHERE account.company EQ cocode
                  AND account.actnum  EQ v-ar-stax
                NO-LOCK NO-ERROR.
            CREATE tt-report.
            ASSIGN
             tt-report.term-id = ""
             tt-report.key-01  = "work-tax"
             tt-report.key-02  = account.actnum
             tt-report.key-03  = STRING(ar-inv.inv-no,"999999")
             tt-report.key-05  = STRING(ar-inv.tax-amt).
          END.
        END.

        v-post-total = v-post-total + ar-inv.gross.

        /** if Freight Is Billable then Post to GL **/
        IF ar-inv.f-bill THEN DO:
          v-post-freight = v-post-freight - ar-inv.freight.

          CREATE tt-report.
          ASSIGN
           tt-report.term-id = ""
           tt-report.key-01  = "work-freight"
           tt-report.key-02  = STRING(ar-inv.inv-no,"999999")
           tt-report.key-05  = STRING(- ar-inv.freight).
        END.

        IF ar-inv.terms EQ "CASH" THEN DO:
          ASSIGN
           v-post-cash  = v-post-cash  + ar-inv.gross
           v-post-total = v-post-total - ar-inv.gross.

          CREATE tt-report.
          ASSIGN
           tt-report.term-id = ""
           tt-report.key-01  = "work-cash"
           tt-report.key-02  = STRING(ar-inv.inv-no,"999999")
           tt-report.key-05  = STRING(ar-inv.gross).
        END.

  IF LAST-OF(tr-num) THEN DO:
    FOR EACH tt-report
        WHERE tt-report.term-id EQ ""
          AND tt-report.key-01  EQ "work-line"
        NO-LOCK
        BREAK BY tt-report.key-02:

      ACCUMULATE dec(tt-report.key-05) (TOTAL BY tt-report.key-02).

      IF LAST-OF(tt-report.key-02) THEN DO:
        
         RUN GL_SpCreateGLHist(cocode,
                            tt-report.key-02,
                            "OEINV",
                            "ORDER ENTRY INVOICE LINES",
                            ar-ledger.tr-date,
                            - (ACCUMULATE TOTAL BY tt-report.key-02 dec(tt-report.key-05)),
                            ar-ledger.tr-num,
                            period.pnum,
                            "A",
                            ar-ledger.tr-date,
                            string(tt-report.key-03),
                            "AR").
      END. /* last actnum */
    END. /* each work-line */
                                              /** POST MISC. TO G/L TRANS **/
    FOR EACH tt-report
        WHERE tt-report.term-id EQ ""
          AND tt-report.key-01  EQ "work-misc"
        NO-LOCK
        BREAK BY tt-report.key-02:

      ACCUMULATE dec(tt-report.key-05) (TOTAL BY tt-report.key-02).

      IF LAST-OF(tt-report.key-02) THEN DO:
        
         RUN GL_SpCreateGLHist(cocode,
                            tt-report.key-02,
                            "OEINV",
                            "ORDER ENTRY INVOICE MISC.",
                            ar-ledger.tr-date,
                            - (ACCUMULATE TOTAL BY tt-report.key-02 dec(tt-report.key-05)),
                            ar-ledger.tr-num,
                            period.pnum,
                            "A",
                            ar-ledger.tr-date,
                            string(tt-report.key-03),
                            "AR").
      END. /* last actnum */
    END. /* each work-misc */
                                           /** POST SALES TAX TO G/L TRANS **/
    FOR EACH tt-report
        WHERE tt-report.term-id EQ ""
          AND tt-report.key-01  EQ "work-tax"
        NO-LOCK
        BREAK BY tt-report.key-02:

      ACCUMULATE dec(tt-report.key-05) (TOTAL BY tt-report.key-02).

      IF LAST-OF(tt-report.key-02) THEN DO:
        
         RUN GL_SpCreateGLHist(cocode,
                            tt-report.key-02,
                            "OEINV",
                            "ORDER ENTRY INVOICE TAX",
                            ar-ledger.tr-date,
                            - (ACCUMULATE TOTAL BY tt-report.key-02 dec(tt-report.key-05)),
                            ar-ledger.tr-num,
                            period.pnum,
                            "A",
                            ar-ledger.tr-date,
                            string(tt-report.key-03),
                            "AR").
      END. /* last actnum */
    END. /* each work-tax */

    FOR EACH work-job BREAK BY work-job.actnum:
      
     RUN GL_SpCreateGLHist(cocode,
                        work-job.actnum,
                        "OEINV",
                        (IF work-job.fg THEN "ORDER ENTRY INVOICE FG" ELSE "ORDER ENTRY INVOICE COGS"),
                        ar-ledger.tr-date,
                        (IF work-job.fg THEN - work-job.amt ELSE work-job.amt),
                        ar-ledger.tr-num,
                        period.pnum,
                        "A",
                        ar-ledger.tr-date,
                        "",
                        "AR").   
    END. /* each work-job */

                                          /** POST FREIGHT TO G/L TRANS **/     
     RUN GL_SpCreateGLHist(cocode,
                        v-ar-freight,
                        "OEINV",
                        "ORDER ENTRY INVOICE FREIGHT",
                        ar-ledger.tr-date,
                        v-post-freight,
                        ar-ledger.tr-num,
                        period.pnum,
                        "A",
                        ar-ledger.tr-date,
                        "",
                        "AR").

                                           /** POST DISCOUNT TO G/L TRANS **/     
     RUN GL_SpCreateGLHist(cocode,
                        v-ar-disc,
                        "OEINV",
                        "ORDER ENTRY INVOICE DISCOUNT",
                        ar-ledger.tr-date,
                        v-post-disc,
                        ar-ledger.tr-num,
                        period.pnum,
                        "A",
                        ar-ledger.tr-date,
                        "",
                        "AR").
                                           /** POST CASH TO G/L TRANS **/
    IF v-post-cash NE 0 THEN DO:
      
       RUN GL_SpCreateGLHist(cocode,
                          ar-ctrl.cash-act,
                          "CASHR",
                          "CASH RECEIPT - INVOICE",
                          ar-ledger.tr-date,
                          v-post-cash,
                          ar-ledger.tr-num,
                          period.pnum,
                          "A",
                          ar-ledger.tr-date,
                          "",
                          "AR").
    END.
                                                  /** OFFSET ENTRY TO G/L **/     
     RUN GL_SpCreateGLHist(cocode,
                        cRecAccount,
                        "OEINV",
                        "ORDER ENTRY INVOICE",
                        ar-ledger.tr-date,
                        v-post-total,
                        ar-ledger.tr-num,
                        period.pnum,
                        "A",
                        ar-ledger.tr-date,
                        "",
                        "AR").
  END.
END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
RUN check-date.
  IF v-invalid THEN RETURN NO-APPLY.

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

