&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-invmh.w

  Description: Invoice Highlights

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

{sys/inc/var.i new shared}
{salrep/dashinv.i NEW}

DEF TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD DATE AS DATE
    FIELD row-id AS ROWID
    FIELD qty AS DEC
    FIELD amt       LIKE ar-invl.amt        FORMAT "->>>>>>>9.99"
    FIELD cash-date LIKE ar-inv.inv-date
    FIELD misc AS LOG
    FIELD cost AS DEC
    FIELD msf AS DEC.

assign
 cocode = gcompany
 locode = gloc.

/* gdm - 03090905 */
DEF VAR v-runflg AS LOG INIT NO NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-9 fi_as-of-date fi_company btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fi_as-of-date fi_company 

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

DEFINE VARIABLE fi_as-of-date AS DATE FORMAT "99/99/9999":U 
     LABEL "As Of Date" 
     VIEW-AS FILL-IN 
     SIZE 16.8 BY 1 NO-UNDO.

DEFINE VARIABLE fi_company AS CHARACTER FORMAT "X(3)":U 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71.6 BY 7.86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fi_as-of-date AT ROW 2.81 COL 12.2 COLON-ALIGNED
     fi_company AT ROW 2.81 COL 42.6 COLON-ALIGNED
     btn-ok AT ROW 5.33 COL 14.6
     btn-cancel AT ROW 5.33 COL 30
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 3
     RECT-9 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 72 BY 7.95.


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
         TITLE              = "Invoice Highlights"
         HEIGHT             = 7.95
         WIDTH              = 72
         MAX-HEIGHT         = 7.95
         MAX-WIDTH          = 72
         VIRTUAL-HEIGHT     = 7.95
         VIRTUAL-WIDTH      = 72
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
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Invoice Highlights */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Invoice Highlights */
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
   IF v-runflg THEN DO:

    SESSION:SET-WAIT-STATE("general").

    ASSIGN {&displayed-objects}.

    EMPTY TEMP-TABLE tt-report.
    EMPTY TEMP-TABLE tt-raw-sales.

    IF NOT CAN-FIND(FIRST company WHERE
       company.company EQ fi_company) THEN
       DO:
          MESSAGE "Invalid Company."
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          APPLY "ENTRY:U" TO fi_company IN FRAME {&FRAME-NAME}.
          LEAVE.
       END.

    RUN run-report.

    SESSION:SET-WAIT-STATE("").
   END.
   ELSE
   IF NOT v-runflg THEN DO:

      MESSAGE 
          "Management Reports are available for purchase, please call ASI."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.

      APPLY  "close" TO THIS-PROCEDURE.

   END.

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
    fi_company = cocode
    fi_as-of-date = TODAY.


  RUN enable_UI.

  /* {custom/usrprint.i} */

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
     APPLY "entry" TO fi_as-of-date.
  END.

  /* gdm - 03090905 */
  {salrep/SlsMgmt.i}

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
  DISPLAY fi_as-of-date fi_company 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-9 fi_as-of-date fi_company btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE raw-sales-proc C-Win 
PROCEDURE raw-sales-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*from HY */
   DEF VAR from-date AS DATE NO-UNDO.
   DEF VAR to-date AS DATE NO-UNDO.
   DEF VAR ld-cost AS DEC NO-UNDO.
   DEF VAR date-index AS DATE NO-UNDO.

   EMPTY TEMP-TABLE tt-report.
   EMPTY TEMP-TABLE tt-raw-sales.

   ASSIGN from-date = DATE(1,1,YEAR(fi_as-of-date) - 1)
          to-date = DATE(12,31,YEAR(fi_as-of-date)).

   DO date-index = from-date TO to-date:

      CREATE tt-raw-sales.
      ASSIGN tt-raw-sales.DATE = date-index.
      RELEASE tt-raw-sales.
   END.

   for each cust WHERE
       cust.company eq fi_company
       no-lock:

       /*{sa/sa-sls03.i from-date to-date}*/
       for each ar-inv FIELDS() WHERE
           ar-inv.company  eq fi_company AND
           ar-inv.posted   eq yes AND
           ar-inv.cust-no  eq cust.cust-no AND
           ar-inv.inv-date ge from-date AND
           ar-inv.inv-date le to-date AND
           ar-inv.type    ne "FC" 
           no-lock:

           create tt-report.
           assign
             tt-report.key-09  = cust.cust-no
             tt-report.key-10  = "ar-inv"
             tt-report.rec-id  = recid(ar-inv).
       end.

       for each ar-cash FIELDS(c-no) WHERE 
           ar-cash.company    eq fi_company AND
           ar-cash.cust-no    eq cust.cust-no AND
           ar-cash.check-date ge from-date AND
           ar-cash.check-date le to-date AND
           ar-cash.posted     eq yes
           no-lock,
           EACH ar-cashl FIELDS(company actnum) WHERE
                ar-cashl.c-no    EQ ar-cash.c-no AND
                ar-cashl.posted  EQ YES AND
                ar-cashl.memo    EQ YES AND
                CAN-FIND(FIRST account WHERE
                         account.company EQ ar-cashl.company AND
                         account.actnum  EQ ar-cashl.actnum AND
                         account.type    EQ "R")
           NO-LOCK:

           create tt-report.
           assign
              tt-report.key-09  = cust.cust-no
              tt-report.key-10  = "ar-cashl"
              tt-report.rec-id  = recid(ar-cashl).
       end.
   end.

   FOR each tt-report:

    if tt-report.key-10 eq "ar-inv" then do:
      find ar-inv where recid(ar-inv) eq tt-report.rec-id no-lock.

      FIND FIRST tt-raw-sales WHERE
           tt-raw-sales.DATE EQ ar-inv.inv-date.

      for each ar-invl
          where ar-invl.x-no    eq ar-inv.x-no
          no-lock:

        find first itemfg
            where itemfg.company eq fi_company
              and itemfg.i-no    eq ar-invl.i-no
            no-lock no-error.

        RUN salrep/salecost.p (3, /*Invoice Cost*/
                               ROWID(ar-invl),
                               ar-invl.job-no,
                               ar-invl.job-no2,
                               ar-invl.ship-qty,
                               OUTPUT ld-cost).

        assign
         tt-raw-sales.date-qty = tt-raw-sales.date-qty +
                                 ar-invl.ship-qty
         tt-raw-sales.date-msf = tt-raw-sales.date-msf +
                                 (if ar-invl.amt-msf ne 0 then ar-invl.amt-msf
                                  else
                                  if avail itemfg then
                                    (ar-invl.ship-qty * itemfg.t-sqft / 1000)
                                  else 0)
         tt-raw-sales.date-cost  = tt-raw-sales.date-cost + ld-cost
         tt-raw-sales.date-tons = tt-raw-sales.date-tons +
                                  ((if ar-invl.t-weight ne 0 then ar-invl.t-weight
                                    else
                                    if avail itemfg then
                                      (ar-invl.ship-qty * itemfg.weight-100 / 100)
                                    else 0) / 2000)
        tt-raw-sales.date-amt = tt-raw-sales.date-amt + ar-invl.amt.
      end.
    end.

    else
    if tt-report.key-10 eq "ar-cashl" then do:
      find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock.
      find ar-cash  where ar-cash.c-no    eq ar-cashl.c-no no-lock.

      FIND FIRST tt-raw-sales WHERE
           tt-raw-sales.DATE EQ ar-cash.check-date.

      RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

      IF AVAIL oe-retl THEN DO:
        find first ar-invl
            where ar-invl.company eq fi_company
              and ar-invl.cust-no eq ar-cash.cust-no
              and ar-invl.inv-no  eq ar-cashl.inv-no
              and ar-invl.i-no    eq oe-retl.i-no
              and (ar-invl.billable or not ar-invl.misc)
            no-lock no-error.

        if avail ar-invl then do:

          find first itemfg
              where itemfg.company eq fi_company
                and itemfg.i-no    eq ar-invl.i-no
              no-lock no-error.

          RUN salrep/salecost.p (3, /*Invoice Cost*/
                                 ROWID(ar-invl),
                                 oe-retl.job-no,
                                 oe-retl.job-no2,
                                 oe-retl.tot-qty-return,
                                 OUTPUT ld-cost).

          assign
           tt-raw-sales.date-qty = tt-raw-sales.date-qty -
                                   oe-retl.tot-qty-return
           tt-raw-sales.date-msf = tt-raw-sales.date-msf -
                                  (if avail itemfg then
                                  (oe-retl.tot-qty-return * itemfg.t-sqft / 1000)
                                  else 0)
           tt-raw-sales.date-tons = tt-raw-sales.date-tons +
                                    ((if avail itemfg then
                                     (oe-retl.tot-qty-return * itemfg.weight-100 / 100)
                                     else 0) / 2000)
           tt-raw-sales.date-cost = tt-raw-sales.date-cost + ld-cost.

        end.
      end.

      tt-raw-sales.date-amt = tt-raw-sales.date-amt + (ar-cashl.amt-paid - ar-cashl.amt-disc).
    end.
   end.

    FOR EACH tt-raw-sales:
        tt-raw-sales.date-net-profit = IF tt-raw-sales.date-amt NE 0 THEN
                                          (tt-raw-sales.date-amt - tt-raw-sales.date-cost) /
                                           tt-raw-sales.date-amt
                                       ELSE 0.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
DO WITH FRAME {&FRAME-NAME}:

   RUN raw-sales-proc. /*Raw Sales*/

   RUN salrep\dashinv.p(INPUT fi_company,
                        INPUT fi_as-of-date).
END.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

