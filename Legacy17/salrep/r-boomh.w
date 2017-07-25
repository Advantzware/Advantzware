&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-boomh.w

  Description: Bookings Highlights

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
{salrep/dashbook.i NEW}

DEF TEMP-TABLE tt-report NO-UNDO LIKE report
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
  field w-sqft      LIKE itemfg.t-sqft format "->>>9.999"    extent 4
  field w-amt       like ar-inv.gross  format "->>>,>>9.99"  extent 4
  field w-cost      like ar-inv.t-cost format "->>,>>9.99"   extent 3
  FIELD w-msf       AS DEC EXTENT 3.

def TEMP-TABLE w-ord NO-UNDO
  field cost like oe-ordl.cost
  field price like oe-ordl.price
  field t-price like oe-ordl.t-price format "->>,>>>,>>9"
  field rel-qty like oe-rel.qty
  field rel-date as DATE
  field msf as dec format "->>9.999"
  FIELD tons AS DEC.

DEF BUFFER b-oe-ordl FOR oe-ordl.

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
     btn-ok AT ROW 5.67 COL 14.6
     btn-cancel AT ROW 5.67 COL 30
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
         TITLE              = "Bookings Highlights"
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
ON END-ERROR OF C-Win /* Bookings Highlights */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Bookings Highlights */
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
  DO WITH FRAME {&FRAME-NAME}:

   IF v-runflg THEN DO:
    SESSION:SET-WAIT-STATE("general").

    ASSIGN {&displayed-objects}.

    EMPTY TEMP-TABLE tt-raw-op.
    EMPTY TEMP-TABLE tt-report.
    EMPTY TEMP-TABLE w-data.

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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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


/* security check need {methods/prgsecur.i} in definition section */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

  ASSIGN
    fi_company = cocode
    fi_as-of-date = TODAY.


  RUN enable_UI.

    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
  /* {custom/usrprint.i} */

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
     APPLY "entry" TO fi_as-of-date.
  END.

  /* gdm - 03090905 */
  {salrep/SlsMgmt.i}


    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE raw-op-proc C-Win 
PROCEDURE raw-op-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* OR10 for orders booked, except using order date instead of due date*/
   DEF VAR v-date AS DATE NO-UNDO.
   DEF VAR v-price AS DEC NO-UNDO.
   DEF VAR v-oe-gp AS DEC NO-UNDO.
   DEF VAR v-start-last-year AS DATE NO-UNDO.
   DEF VAR v-end-this-year AS DATE NO-UNDO.

   ASSIGN
      v-start-last-year = DATE(1,1,YEAR(fi_as-of-date) - 1)
      v-end-this-year   = DATE(12,31,YEAR(fi_as-of-date)).

   EMPTY TEMP-TABLE tt-raw-op.

   DO v-date = DATE(1,1,YEAR(fi_as-of-date) - 1) TO
      DATE(12,31,YEAR(fi_as-of-date)):

      CREATE tt-raw-op.
      tt-raw-op.DATE = v-date.
      RELEASE tt-raw-op.
   END.

   FOR each oe-ord FIELDS(company ord-no ord-date TYPE) WHERE
       oe-ord.company  eq fi_company AND
       oe-ord.ord-date ge v-start-last-year AND
       oe-ord.ord-date le v-end-this-year AND
       oe-ord.type     ne "T"
       no-lock,
       each oe-ordl FIELDS(t-price qty company ord-no cost) WHERE
            oe-ordl.company eq oe-ord.company AND
            oe-ordl.ord-no  eq oe-ord.ord-no
            no-lock,
       first itemfg FIELDS(company i-no t-sqft weight-100) WHERE
             itemfg.company eq oe-ord.company AND
             itemfg.i-no    eq oe-ordl.i-no
             no-lock,
       FIRST tt-raw-op WHERE
             tt-raw-op.DATE EQ oe-ord.ord-date:

      ASSIGN
        tt-raw-op.oe-dollars = tt-raw-op.oe-dollars + oe-ordl.t-price
        tt-raw-op.oe-qty = tt-raw-op.oe-qty 
                         + oe-ordl.qty
        tt-raw-op.oe-qty-msf = tt-raw-op.oe-qty-msf 
                             + (itemfg.t-sqft * oe-ordl.qty / 1000)
        tt-raw-op.oe-qty-tons = tt-raw-op.oe-qty-tons
                              +( itemfg.weight-100 * oe-ordl.qty / 100 / 2000)
        v-oe-gp = (IF oe-ordl.t-price NE 0 THEN
                   ((oe-ordl.t-price - (oe-ordl.cost * (oe-ordl.qty / 1000) ) )  
                     / oe-ordl.t-price * 100)
                   ELSE 0)
        v-oe-gp = IF v-oe-gp EQ ? THEN 0 ELSE v-oe-gp
        tt-raw-op.oe-gp = tt-raw-op.oe-gp 
                        + v-oe-gp.

   END.

   RUN raw-op-rel-proc.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE raw-op-rel-proc C-Win 
PROCEDURE raw-op-rel-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*OR2*/
   DEF VAR v-types AS CHAR INIT "PALSBICZ" NO-UNDO.
   DEF VAR v-type AS CHAR NO-UNDO.
   DEF VAR v-start-date AS DATE NO-UNDO.
   DEF VAR v-end-date AS DATE NO-UNDO.
   DEF VAR lv-qty AS DEC NO-UNDO.
   DEF VAR v-qty AS DEC NO-UNDO.
   DEF VAR v-date AS DATE NO-UNDO.
   DEF VAR v-rel-gp AS DEC NO-UNDO.

   ASSIGN
     v-start-date = DATE(1,1,YEAR(fi_as-of-date) - 1)
     v-end-date = DATE(12,31,YEAR(fi_as-of-date)). 

   EMPTY TEMP-TABLE w-ord.
   EMPTY TEMP-TABLE tt-report.

   FOR EACH oe-ordl FIELDS(company opened ord-no LINE i-no)
      WHERE oe-ordl.company EQ fi_company
        AND oe-ordl.opened  EQ YES
        AND NOT CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}
                         USE-INDEX ord-no)
      USE-INDEX opened NO-LOCK,
      FIRST oe-ord FIELDS(company ord-no)
      WHERE oe-ord.company EQ oe-ordl.company
        AND oe-ord.ord-no  EQ oe-ordl.ord-no
      NO-LOCK:

      /* RUN oe/cleanrel.p (ROWID(oe-ordl)). */

      for each oe-rel FIELDS(company cust-no ord-no i-no LINE rel-date) no-lock
        where oe-rel.company   eq oe-ordl.company
          and oe-rel.ord-no    eq oe-ordl.ord-no
          and oe-rel.i-no      eq oe-ordl.i-no
          and oe-rel.line      eq oe-ordl.line
          and oe-rel.rel-date  ge v-start-date
          and oe-rel.rel-date  le v-end-date
        use-index ord-item:

        RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-type).

        if index("AB",v-type) gt 0 then next.

        if index(v-types,v-type) gt 0 then do:
          create tt-report.
          assign
           tt-report.key-06  = v-type
           tt-report.rec-id  = recid(oe-rel).
        end.
      end.

      FOR EACH oe-rell FIELDS(r-no company ord-no i-no LINE b-ord-no po-no
          qty rel-no) NO-LOCK
        WHERE oe-rell.company EQ oe-ordl.company
          AND oe-rell.ord-no  EQ oe-ordl.ord-no
          AND oe-rell.i-no    EQ oe-ordl.i-no
          AND oe-rell.line    EQ oe-ordl.line
          AND ((oe-rell.b-ord-no NE 0 AND INDEX(v-types,"B") GT 0) OR
               (oe-rell.b-ord-no EQ 0 AND INDEX(v-types,"A") GT 0))
        USE-INDEX ord-no,

        FIRST oe-relh fields(cust-no r-no posted deleted rel-date) NO-LOCK
        WHERE oe-relh.r-no     EQ oe-rell.r-no
          AND oe-relh.posted   EQ NO
          AND oe-relh.deleted  EQ NO
          AND oe-relh.rel-date GE v-start-date
          AND oe-relh.rel-date LE v-end-date

        USE-INDEX r-no

      BREAK BY oe-rell.r-no
            BY oe-rell.ord-no
            BY oe-rell.i-no
            BY oe-rell.line
            BY oe-rell.rel-no
            BY oe-rell.b-ord-no
            BY oe-rell.po-no:

       IF FIRST-OF(oe-rell.po-no) THEN lv-qty = 0.

       lv-qty = lv-qty + oe-rell.qty.

       IF LAST-OF(oe-rell.po-no) THEN DO:
         create tt-report.
         assign
          tt-report.key-06  = if oe-rell.b-ord-no eq 0 then "A" else "B"
          tt-report.qty     = lv-qty
          tt-report.rec-id  = recid(oe-rell).
       END.
      END.
   END.

   IF NOT CAN-FIND(FIRST tt-report) THEN DO:
      CREATE tt-report.
   END.

   RELEASE tt-report.

   for each tt-report:

       release oe-rel.
       release oe-rell.
       release oe-relh.
       release oe-ord.
       release oe-ordl.

       find first oe-rel 
           where recid(oe-rel) eq tt-report.rec-id 
           no-lock no-error.

       if avail oe-rel then do:
         FOR EACH oe-rell FIELDS(company ord-no rel-no b-ord-no i-no LINE
             r-no) NO-LOCK
             WHERE oe-rell.company  EQ fi_company
               AND oe-rell.ord-no   EQ oe-rel.ord-no
               AND oe-rell.rel-no   EQ oe-rel.rel-no
               AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
               AND oe-rell.i-no     EQ oe-rel.i-no
               AND oe-rell.line     EQ oe-rel.line
             USE-INDEX ord-no,
             FIRST oe-relh FIELDS(cust-no r-no posted deleted) WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK:

           IF oe-relh.posted EQ NO AND oe-relh.deleted EQ NO THEN
             tt-report.rec-id = recid(oe-rell).
           ELSE RELEASE oe-relh.

           LEAVE.
         END.

         find first oe-ordl
             where oe-ordl.company eq fi_company
               and oe-ordl.ord-no  eq oe-rel.ord-no
               and oe-ordl.i-no    eq oe-rel.i-no
               and oe-ordl.line    eq oe-rel.line
             no-lock.
       end.

       find oe-rell where recid(oe-rell) eq tt-report.rec-id no-lock no-error.
       if avail oe-rell then do:    
         if index("SLI",tt-report.key-06) gt 0 then
           tt-report.key-06 = if oe-rell.b-ord-no eq 0 then "A" else "B" .

         find first oe-relh
             where oe-relh.company eq fi_company
               and oe-relh.r-no    eq oe-rell.r-no
             use-index r-no no-lock.

         find first oe-ordl
             where oe-ordl.company eq fi_company
               and oe-ordl.ord-no  eq oe-rell.ord-no
               and oe-ordl.i-no    eq oe-rell.i-no
               and oe-ordl.line    eq oe-rell.line
             no-lock.
       end.

       find first oe-ord of oe-ordl no-lock no-error.

       if avail oe-ord then
        find first cust
            where cust.company eq fi_company
              and cust.cust-no eq oe-ord.cust-no
            no-lock no-error.

        if avail oe-relh then
          assign
           v-qty     = IF tt-report.qty NE 0 THEN tt-report.qty ELSE oe-rell.qty
           v-date    = oe-relh.rel-date.
        else
        if avail oe-rel then
          assign
           v-qty     = oe-rel.qty 
           v-date    = oe-rel.rel-date.

    if avail oe-ordl then do:
      find first itemfg
          where itemfg.company eq fi_company
            and itemfg.i-no    eq oe-ordl.i-no
          NO-LOCK NO-ERROR.

      IF AVAIL itemfg THEN
      DO:
         create w-ord.
         assign
          w-ord.cost      = oe-ordl.cost
          w-ord.price     = oe-ordl.t-price / oe-ordl.qty
          w-ord.rel-qty   = v-qty
          w-ord.t-price   = w-ord.price * w-ord.rel-qty
          w-ord.rel-date  = v-date
          w-ord.msf       = w-ord.rel-qty * itemfg.t-sqft / 1000
          w-ord.tons      = itemfg.weight-100 * oe-ordl.qty / 100 / 2000.
      END.
    END.
   END. /*each tt-report*/

   FOR EACH w-ord,
       FIRST tt-raw-op WHERE
             tt-raw-op.DATE EQ w-ord.rel-date:

       ASSIGN
        tt-raw-op.rel-dollars = tt-raw-op.rel-dollars + w-ord.t-price
        tt-raw-op.rel-qty = tt-raw-op.rel-qty 
                          + w-ord.rel-qty
        tt-raw-op.rel-qty-msf = tt-raw-op.rel-qty-msf 
                              + w-ord.msf
        tt-raw-op.rel-qty-tons = tt-raw-op.rel-qty-tons
                               + w-ord.tons
        v-rel-gp = (IF w-ord.t-price NE 0 THEN
                   ((w-ord.t-price - (w-ord.cost * (w-ord.rel-qty / 1000) ) )  
                     / w-ord.t-price * 100)
                   ELSE 0)
        v-rel-gp = IF v-rel-gp EQ ? THEN 0 ELSE v-rel-gp
        tt-raw-op.rel-gp = tt-raw-op.rel-gp 
                         + v-rel-gp.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
DO WITH FRAME {&FRAME-NAME}:

   RUN raw-op-proc. /*Raw OP*/
   RUN salrep\dashboard-book.p(INPUT fi_company,
                               INPUT fi_as-of-date).
END.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

