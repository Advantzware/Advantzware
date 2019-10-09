&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-salrmh.w

  Description: Sales Rep Highlights

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
{salrep/dashrep.i NEW}

DEF TEMP-TABLE tt-report LIKE report
    FIELD DATE AS DATE
    FIELD row-id AS ROWID
    FIELD qty AS DEC
    FIELD amt       LIKE ar-invl.amt        FORMAT "->>>>>>>9.99"
    FIELD cash-date LIKE ar-inv.inv-date
    FIELD misc AS LOG
    FIELD cost AS DEC
    FIELD msf AS DEC.

def TEMP-TABLE w-data no-undo
  field w-sman-no   AS CHAR
  field w-sqft      LIKE itemfg.t-sqft format "->>>9.999"    extent 3
  field w-amt       like ar-inv.gross  format "->>>,>>9.99"  extent 3
  field w-cost      like ar-inv.t-cost format "->>,>>9.99"   extent 3
  FIELD w-msf       AS DEC EXTENT 3.

DEF BUFFER b-tt-report FOR tt-report.
DEF BUFFER b-ar-invl FOR ar-invl.

DEF VAR v-sman-no AS CHAR NO-UNDO.
DEF VAR fsman AS CHAR NO-UNDO.
DEF VAR tsman AS CHAR INIT "zzz" NO-UNDO.

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

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-9 fi_as-of-date fi_company ~
tg_only-sales btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fi_as-of-date fi_company tg_only-sales 

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

DEFINE VARIABLE tg_only-sales AS LOGICAL INITIAL no 
     LABEL "Only Show Sales Reps with YTD Activity" 
     VIEW-AS TOGGLE-BOX
     SIZE 43 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fi_as-of-date AT ROW 2.81 COL 12.2 COLON-ALIGNED
     fi_company AT ROW 2.81 COL 42.6 COLON-ALIGNED
     tg_only-sales AT ROW 4.1 COL 14 WIDGET-ID 2
     btn-ok AT ROW 5.71 COL 14.6
     btn-cancel AT ROW 5.71 COL 30
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
         TITLE              = "Sales Rep Highlights"
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
   FRAME-NAME                                                           */
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
ON END-ERROR OF C-Win /* Sales Rep Highlights */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Sales Rep Highlights */
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
    EMPTY TEMP-TABLE tt-raw-salesmen.

    IF NOT CAN-FIND(FIRST company WHERE
       company.company EQ fi_company) THEN
       DO:
          MESSAGE "Invalid Company."
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          APPLY "ENTRY:U" TO fi_company IN FRAME {&FRAME-NAME}.
          LEAVE.
       END.

    FOR EACH sman FIELDS(sman sname) WHERE
        sman.company EQ fi_company
        NO-LOCK:

        CREATE tt-raw-salesmen.
        ASSIGN tt-raw-salesmen.sman = sman.sman
               tt-raw-salesmen.sname = sman.sname
               tt-raw-salesmen.DATE = fi_as-of-date.
        RELEASE tt-raw-salesmen.
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
  DISPLAY fi_as-of-date fi_company tg_only-sales 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-9 fi_as-of-date fi_company tg_only-sales btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE raw-salesmen-proc C-Win 
PROCEDURE raw-salesmen-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var v-pct       as   dec format "99.99"                             no-undo.
  def var v-amt       LIKE ar-inv.gross  format "->,>>>,>>9.99"           no-undo.    
  def var v-cost      LIKE itemfg.t-sqft  format "->,>>9.999"             no-undo.
  def var v-sqft      like ar-inv.t-cost format "->,>>>,>>9.99"           no-undo.
  DEF VAR v-index AS INT NO-UNDO.
  DEF VAR ld-inv-pct AS DEC NO-UNDO.
  DEF VAR v-start-of-year AS DATE NO-UNDO.
  DEF VAR v-end-of-year AS DATE NO-UNDO.
  DEF VAR v-this-month AS INT NO-UNDO.

  EMPTY TEMP-TABLE tt-report.
  EMPTY TEMP-TABLE w-data.

  ASSIGN
     v-start-of-year = DATE(1,1,YEAR(fi_as-of-date))
     v-end-of-year   = DATE(12,31,YEAR(fi_as-of-date))
     v-this-month    = MONTH(fi_as-of-date).

  /*from HT*/
  for each ar-inv FIELDS(company cust-no x-no inv-date)
      where ar-inv.company  eq fi_company
        and ar-inv.posted   eq yes
        and ar-inv.inv-date ge v-start-of-year
        and ar-inv.inv-date le v-end-of-year
        and ar-inv.type    ne "FC"
        no-lock,
      first cust FIELDS(sman)
      where cust.company eq ar-inv.company
        and cust.cust-no eq ar-inv.cust-no
      no-lock,

      each ar-invl FIELDS(sman s-pct i-no actnum)
      where ar-invl.x-no eq ar-inv.x-no
        and (ar-invl.billable or not ar-invl.misc)
      no-lock:

      {sa/sa-sman6.i ar-inv.inv-date "ar-invl" }
  end. /*each ar-inv*/

  for each cust FIELDS(cust-no sman)
      where cust.company eq fi_company
      no-lock,
      each ar-cash FIELDS(c-no cust-no check-date)
      where ar-cash.company    eq fi_company
        and ar-cash.cust-no    eq cust.cust-no
        and ar-cash.check-date ge v-start-of-year
        and ar-cash.check-date le v-end-of-year
        and ar-cash.posted     eq yes
      no-lock,

      EACH ar-cashl FIELDS(company actnum inv-no dscr c-no amt-paid amt-disc)
      WHERE ar-cashl.c-no    EQ ar-cash.c-no
        AND ar-cashl.posted  EQ YES
        AND ar-cashl.memo    EQ YES
        AND CAN-FIND(FIRST account
                     WHERE account.company EQ ar-cashl.company
                       AND account.actnum  EQ ar-cashl.actnum
                       AND account.type    EQ "R")
      NO-LOCK:

    RELEASE ar-invl.

    RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

    IF AVAIL oe-retl THEN
    find first ar-invl
        where ar-invl.company eq fi_company
          and ar-invl.cust-no eq ar-cash.cust-no
          and ar-invl.inv-no  eq ar-cashl.inv-no
          and ar-invl.i-no    eq oe-retl.i-no
          and (ar-invl.billable or not ar-invl.misc)
        no-lock no-error.

    IF ar-cashl.inv-no NE 0                                                       AND
           (AVAIL ar-invl                             OR
            (NOT AVAIL reftable AND
             NOT ar-cashl.dscr MATCHES "*oe return*") OR
            SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,5) EQ "items") THEN
        FOR EACH b-ar-invl FIELDS(billable misc sman s-pct i-no actnum)
            WHERE b-ar-invl.company EQ ar-cashl.company
              AND b-ar-invl.cust-no EQ cust.cust-no
              AND b-ar-invl.inv-no  EQ ar-cashl.inv-no
              AND (b-ar-invl.billable OR NOT b-ar-invl.misc)
              AND (NOT AVAIL ar-invl OR ROWID(b-ar-invl) EQ ROWID(ar-invl))
            NO-LOCK:
          {sa/sa-sman6.i ar-cash.check-date "ar-cashl" "b-"}
        end.

        else
        do:
          create tt-report.
          assign
           tt-report.key-02  = cust.sman
           tt-report.rec-id  = recid(ar-cashl)
           tt-report.DATE    = ar-cash.check-date.
        end.
  end. /*each cust*/

  FOR EACH tt-report,
      FIRST tt-raw-salesmen WHERE
            tt-raw-salesmen.sman EQ tt-report.key-02
      break by tt-report.key-02:

      find first w-data
          where w-data.w-sman-no eq tt-report.key-02
          no-lock no-error.

      if not avail w-data then do:
        create w-data.
        w-data.w-sman-no = tt-report.key-02.
      end.

      find ar-invl where recid(ar-invl) eq tt-report.rec-id no-lock no-error.

      if avail ar-invl then do:
         find ar-inv where ar-inv.x-no eq ar-invl.x-no no-lock.

         find first itemfg
              where itemfg.company eq fi_company
              and itemfg.i-no    eq ar-invl.i-no
              no-lock no-error.

         assign
           v-pct  = 1
           v-amt  = ar-invl.amt
           v-cost = ar-invl.t-cost
           v-sqft = if ar-invl.amt-msf ne 0 then ar-invl.amt-msf
                    else
                    if avail itemfg then
                      (itemfg.t-sqft * ar-invl.ship-qty / 1000) else 0.

         if v-amt  eq ? then v-amt  = 0.
         if v-cost eq ? then v-cost = 0.
         if v-sqft eq ? then v-sqft = 0.

         do i = 1 to 3:
           if ar-invl.sman[i] eq tt-report.key-02 then
             assign
              v-pct = ar-invl.s-pct[i] / 100
              i     = 3.
         end.

         if v-pct eq 0 then
         do i = 1 to 3:
           if i eq 1 then j = 0.
           if ar-invl.sman[i] ne "" then j = j + 1.
           if i eq 3 then v-pct = 1 / j.
         end.

         if v-pct le 0 or v-pct eq ? then v-pct = 1.

         IF ar-inv.inv-date EQ tt-raw-salesmen.DATE THEN
            assign
              w-data.w-sqft[1] = w-data.w-sqft[1] + (v-sqft * v-pct)
              w-data.w-amt[1]  = w-data.w-amt[1]  + (v-amt  * v-pct)
              w-data.w-cost[1] = w-data.w-cost[1] + (v-cost * v-pct).

         IF ar-inv.inv-date LE fi_as-of-date THEN
         DO:
            IF MONTH(ar-inv.inv-date) EQ v-this-month THEN
               assign
                 w-data.w-sqft[2] = w-data.w-sqft[2] + (v-sqft * v-pct)
                 w-data.w-amt[2]  = w-data.w-amt[2]  + (v-amt  * v-pct)
                 w-data.w-cost[2] = w-data.w-cost[2] + (v-cost * v-pct).

            assign
              w-data.w-sqft[3] = w-data.w-sqft[3] + (v-sqft * v-pct)
              w-data.w-amt[3]  = w-data.w-amt[3]  + (v-amt  * v-pct)
              w-data.w-cost[3] = w-data.w-cost[3] + (v-cost * v-pct).
         END.

         assign
            v-index = MONTH(ar-inv.inv-date)
            tt-raw-salesmen.amt[v-index] = tt-raw-salesmen.amt[v-index] + (v-amt * v-pct)
            tt-raw-salesmen.msf[v-index] = tt-raw-salesmen.msf[v-index] + (v-sqft * v-pct).

      END.
      ELSE DO:
         find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock no-error.

         if avail ar-cashl then do:
           find ar-cash where ar-cash.c-no eq ar-cashl.c-no no-lock.

           assign
            v-amt  = ar-cashl.amt-paid - ar-cashl.amt-disc
            v-cost = 0
            v-sqft = 0
            v-pct  = 1.

           RELEASE itemfg.
           RELEASE ar-invl.
           RELEASE oe-retl.

           FIND ar-invl WHERE ROWID(ar-invl) EQ tt-report.row-id NO-LOCK NO-ERROR.

           IF NOT AVAIL ar-invl THEN
              RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

           IF AVAIL oe-retl THEN DO:
             find first itemfg
                 where itemfg.company eq fi_company
                   and itemfg.i-no    eq oe-retl.i-no
                 no-lock no-error.

             v-sqft = if avail itemfg then
                        oe-retl.tot-qty-return * itemfg.t-sqft / 1000
                      else 0.

             if v-sqft eq ? then v-sqft = 0.

             RUN salrep/salecost.p(3,
                                   ROWID(ar-invl),
                                   oe-retl.job-no,
                                   oe-retl.job-no,
                                   oe-retl.tot-qty-return,
                                   OUTPUT v-cost).
           END.
           ELSE
           IF AVAIL ar-invl THEN DO:
              ld-inv-pct = 0.
              FOR EACH b-ar-invl FIELDS(amt) WHERE b-ar-invl.x-no EQ ar-invl.x-no NO-LOCK:
                ld-inv-pct = ld-inv-pct + b-ar-invl.amt.
                ACCUMULATE 1 (TOTAL).
              END.
              ld-inv-pct = IF ld-inv-pct EQ 0 THEN
                              (1 / IF (ACCUM TOTAL 1) EQ 0 THEN 1
                                                           ELSE (ACCUM TOTAL 1))
                           ELSE (ar-invl.amt / ld-inv-pct).

              IF ld-inv-pct EQ ? THEN ld-inv-pct = 0.

              v-amt = v-amt * ld-inv-pct.

              if v-sqft eq ? then v-sqft = 0.

              do i = 1 to 3:
                if ar-invl.sman[i] eq tt-report.key-02 then
                  assign
                   v-pct = ar-invl.s-pct[i] / 100
                   i     = 3.
              end.

              if v-pct eq 0 then
              do i = 1 to 3:
                if i eq 1 then j = 0.
                if ar-invl.sman[i] ne "" then j = j + 1.
                if i eq 3 then v-pct = 1 / j.
              end.

              if v-pct le 0 or v-pct eq ? then v-pct = 1.
           end.

           IF ar-cash.check-date EQ tt-raw-salesmen.DATE THEN
            assign
              w-data.w-sqft[1] = w-data.w-sqft[1] - (v-sqft * v-pct)
              w-data.w-amt[1]  = w-data.w-amt[1]  + (v-amt  * v-pct)
              w-data.w-cost[1] = w-data.w-cost[1] - (v-cost * v-pct).

           IF ar-cash.check-date LE fi_as-of-date THEN
           DO:
              IF MONTH(ar-cash.check-date) EQ v-this-month THEN
               assign
                 w-data.w-sqft[2] = w-data.w-sqft[2] - (v-sqft * v-pct)
                 w-data.w-amt[2]  = w-data.w-amt[2]  + (v-amt  * v-pct)
                 w-data.w-cost[2] = w-data.w-cost[2] - (v-cost * v-pct).

               assign
                 w-data.w-sqft[3] = w-data.w-sqft[3] - (v-sqft * v-pct)
                 w-data.w-amt[3]  = w-data.w-amt[3]  + (v-amt  * v-pct)
                 w-data.w-cost[3] = w-data.w-cost[3] - (v-cost * v-pct).
           END.

           assign
            v-index = MONTH(ar-cash.check-date)
            tt-raw-salesmen.amt[v-index] = tt-raw-salesmen.amt[v-index] + (v-amt * v-pct)
            tt-raw-salesmen.msf[v-index] = tt-raw-salesmen.msf[v-index] - (v-sqft  * v-pct).
         END.
      END.

      IF LAST-OF(tt-report.key-02) THEN
      DO:
         ASSIGN
            tt-raw-salesmen.date-msf = tt-raw-salesmen.date-msf + w-data.w-sqft[1]
            tt-raw-salesmen.date-amt = tt-raw-salesmen.date-amt + w-data.w-amt[1]
            tt-raw-salesmen.date-sf = tt-raw-salesmen.date-sf + (w-data.w-sqft[1] * 1000)
            tt-raw-salesmen.date-cost = tt-raw-salesmen.date-cost + w-data.w-cost[1]
            tt-raw-salesmen.mtd-msf = tt-raw-salesmen.mtd-msf + w-data.w-sqft[2] 
            tt-raw-salesmen.mtd-amt = tt-raw-salesmen.mtd-amt + w-data.w-amt[2]
            tt-raw-salesmen.mtd-sf = tt-raw-salesmen.mtd-sf + (w-data.w-sqft[2] * 1000)
            tt-raw-salesmen.mtd-cost = tt-raw-salesmen.mtd-cost + w-data.w-cost[2]
            tt-raw-salesmen.ytd-msf = tt-raw-salesmen.ytd-msf + w-data.w-sqft[3]
            tt-raw-salesmen.ytd-amt = tt-raw-salesmen.ytd-amt + w-data.w-amt[3]
            tt-raw-salesmen.ytd-sf = tt-raw-salesmen.ytd-sf + (w-data.w-sqft[3] * 1000)
            tt-raw-salesmen.ytd-cost = tt-raw-salesmen.ytd-cost + w-data.w-cost[3].

         DELETE w-data.
      END.
  END.

  FOR EACH tt-raw-salesmen:

      ASSIGN
         tt-raw-salesmen.date-profit = IF tt-raw-salesmen.date-amt NE 0 THEN
                                          (tt-raw-salesmen.date-amt - tt-raw-salesmen.date-cost) /
                                          tt-raw-salesmen.date-amt * 100
                                       ELSE 0
         tt-raw-salesmen.mtd-profit = IF tt-raw-salesmen.mtd-amt NE 0 THEN
                                         (tt-raw-salesmen.mtd-amt - tt-raw-salesmen.mtd-cost) /
                                         tt-raw-salesmen.mtd-amt * 100
                                      ELSE 0
         tt-raw-salesmen.ytd-profit = IF tt-raw-salesmen.ytd-amt NE 0 THEN
                                         (tt-raw-salesmen.ytd-amt - tt-raw-salesmen.ytd-cost) /
                                         tt-raw-salesmen.ytd-amt * 100
                                      ELSE 0.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
DO WITH FRAME {&FRAME-NAME}:

   RUN raw-salesmen-proc. /*Raw Salesmen*/ 

   IF tg_only-sales THEN
      FOR EACH tt-raw-salesmen WHERE
          tt-raw-salesmen.ytd-sf EQ 0 AND
          tt-raw-salesmen.ytd-amt EQ 0 AND
          tt-raw-salesmen.ytd-msf EQ 0:

          DELETE tt-raw-salesmen.
      END.

   RUN salrep\dashrephm.p(INPUT fi_company,
                        INPUT fi_as-of-date).
END.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

