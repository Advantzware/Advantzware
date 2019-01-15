&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
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
{custom/globdefs.i}
{custom/gcompany.i}
{custom/getcmpny.i}
{sys/inc/var.i new shared}

cocode = g_company.

DEF VAR ll-transfer AS LOG NO-UNDO.
def var lv-stat as cha no-undo.
DEF VAR ld-date as DATE NO-UNDO.

DEF BUFFER ref-lot-no FOR reftable.
DEF BUFFER ref-sell-price FOR reftable.

DEFINE TEMP-TABLE tt-oe-rel NO-UNDO LIKE oe-rel
   FIELD tt-recid AS RECID
   FIELD rel-qty AS INT.

DEF TEMP-TABLE tt-report NO-UNDO
    LIKE report
    FIELD phantom AS LOG
    FIELD po-no LIKE oe-rel.po-no
    FIELD qty LIKE oe-rel.qty
    FIELD printed AS LOG
    FIELD s-code AS CHAR
    FIELD lot-no AS CHAR
    FIELD sell-price AS DEC
    FIELD freight-pay AS CHAR
    FIELD fob AS CHAR
    FIELD zero-sprice AS LOG.

DO TRANSACTION:
   {sys/ref/relpost.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-oe-rel FIRST

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 tt-oe-rel.LINE tt-oe-rel.i-no tt-oe-rel.ship-id tt-oe-rel.stat tt-oe-rel.carrier tt-oe-rel.tot-qty tt-oe-rel.rel-qty tt-report.po-no tt-report.lot-no tt-report.key-02   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH tt-oe-rel, ~
      FIRST tt-report WHERE tt-report.rec-id eq tt-oe-rel.tt-recid     BY tt-report.key-01        BY tt-oe-rel.po-no         BY tt-oe-rel.ship-no          BY tt-oe-rel.qty
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH tt-oe-rel, ~
      FIRST tt-report WHERE tt-report.rec-id eq tt-oe-rel.tt-recid     BY tt-report.key-01        BY tt-oe-rel.po-no         BY tt-oe-rel.ship-no          BY tt-oe-rel.qty.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tt-oe-rel tt-report
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tt-oe-rel
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-1 tt-report


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_ord-no btn-pop BROWSE-1 btn-status 
&Scoped-Define DISPLAYED-OBJECTS begin_ord-no 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-pop 
     LABEL "Populate Browser" 
     SIZE 22 BY 1.14.

DEFINE BUTTON btn-status 
     LABEL "Recalculate Status" 
     SIZE 22 BY 1.14.

DEFINE VARIABLE begin_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Order #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      tt-oe-rel, 
      tt-report SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 C-Win _FREEFORM
  QUERY BROWSE-1 DISPLAY
      tt-oe-rel.LINE COLUMN-LABEL "Line"
 tt-oe-rel.i-no FORMAT "X(20)" COLUMN-LABEL "Item #"
 tt-oe-rel.ship-id FORMAT "X(8)" COLUMN-LABEL "Ship To"
 tt-oe-rel.stat FORMAT "X(1)" COLUMN-LABEL "S"
 tt-oe-rel.carrier FORMAT "X(8)" COLUMN-LABEL "Via"
 tt-oe-rel.tot-qty FORMAT "->>,>>>,>>9" COLUMN-LABEL "Sched Qty" 
 tt-oe-rel.rel-qty COLUMN-LABEL "Actual Qty"
 tt-report.po-no COLUMN-LABEL "Customer PO" FORMAT "X(20)"
 tt-report.lot-no COLUMN-LABEL "Customer Lot" FORMAT "X(20)"
 tt-report.key-02 COLUMN-LABEL "Date" FORMAT "99/99/99"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 125 BY 8.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_ord-no AT ROW 1.48 COL 8 COLON-ALIGNED HELP
          "Enter Beginning Run Number"
     btn-pop AT ROW 1.48 COL 29
     BROWSE-1 AT ROW 3.14 COL 4
     btn-status AT ROW 12.19 COL 4 WIDGET-ID 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 130 BY 15.38.


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
         TITLE              = "Fix Release Status"
         HEIGHT             = 15.38
         WIDTH              = 130
         MAX-HEIGHT         = 15.38
         MAX-WIDTH          = 130
         VIRTUAL-HEIGHT     = 15.38
         VIRTUAL-WIDTH      = 130
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
/* BROWSE-TAB BROWSE-1 btn-pop FRAME-A */
ASSIGN 
       begin_ord-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-oe-rel,FIRST tt-report WHERE tt-report.rec-id eq tt-oe-rel.tt-recid
    BY tt-report.key-01
       BY tt-oe-rel.po-no
        BY tt-oe-rel.ship-no
         BY tt-oe-rel.qty
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Fix Release Status */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Fix Release Status */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-no C-Win
ON HELP OF begin_ord-no IN FRAME FRAME-A /* Order # */
DO:
  DEF VAR lv AS CHAR NO-UNDO.
  lv = {&self-name}:SCREEN-VALUE.
  RUN run-no-help (INPUT-OUTPUT lv).
  {&self-name}:SCREEN-VALUE = lv.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-no C-Win
ON LEAVE OF begin_ord-no IN FRAME FRAME-A /* Order # */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-pop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-pop C-Win
ON CHOOSE OF btn-pop IN FRAME FRAME-A /* Populate Browser */
DO:
   DEF VAR lv-stat AS CHAR NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:

      ASSIGN begin_ord-no.

      EMPTY TEMP-TABLE tt-oe-rel.
      EMPTY TEMP-TABLE tt-report.

      FOR EACH oe-ordl WHERE
          oe-ordl.company EQ cocode AND
          oe-ordl.ord-no EQ begin_ord-no
          NO-LOCK:
          RUN build-report-file.
      END.

      FOR EACH oe-rel WHERE
          oe-rel.company EQ cocode AND
          oe-rel.ord-no  EQ begin_ord-no
          NO-LOCK:

          CREATE tt-oe-rel.
          BUFFER-COPY oe-rel TO tt-oe-rel
             ASSIGN tt-oe-rel.tt-recid = RECID(oe-rel).

          RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).

          FIND FIRST tt-report WHERE tt-report.rec-id eq recid(oe-rel) NO-ERROR.

          tt-oe-rel.rel-qty = IF INDEX("SIL",lv-stat) GT 0 THEN 0
                              ELSE IF AVAIL tt-report AND
                              INDEX("AB",lv-stat) GT 0 THEN tt-report.qty
                              ELSE
                              oe-rel.qty.

          RELEASE tt-oe-rel.
      END.

      OPEN QUERY browse-1 FOR EACH tt-oe-rel,FIRST tt-report WHERE tt-report.rec-id eq tt-oe-rel.tt-recid.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-status
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-status C-Win
ON CHOOSE OF btn-status IN FRAME FRAME-A /* Recalculate Status */
DO:
   DISABLE TRIGGERS FOR LOAD OF oe-rel.

   DEF VAR lv-stat AS CHAR NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:

      IF AVAIL tt-oe-rel THEN
      DO:
         FIND oe-rel WHERE
              RECID(oe-rel) EQ tt-oe-rel.tt-recid
              NO-ERROR.

         IF AVAIL tt-oe-rel THEN
         DO:
            RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT oe-rel.stat).
            FIND CURRENT oe-rel NO-LOCK NO-ERROR.
         END.
      END.
   END.

   APPLY "CHOOSE" TO btn-pop IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
{sys/inc/oeinq.i}
{sa/sa-sls01.i}

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/*SESSION:DATA-ENTRY-RETURN = YES.*/

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:

DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.

  WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-report-file C-Win 
PROCEDURE build-report-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR v-nxt-r-no LIKE oe-rel.r-no NO-UNDO.
DEF VAR lv-qty AS INT NO-UNDO.
DEF VAR lv-stat AS CHAR NO-UNDO.
DEF VAR lv-s-code LIKE oe-rell.s-code EXTENT 2 NO-UNDO.

DEF BUFFER b-oe-rell FOR oe-rell.
DEF BUFFER b-oe-rell-exc FOR oe-rell.
DEF BUFFER b-oe-rel  FOR oe-rel.

  ll-transfer = CAN-FIND(FIRST oe-ord
                         WHERE oe-ord.company EQ cocode
                           AND oe-ord.ord-no  EQ begin_ord-no
                           AND oe-ord.type    EQ "T").

  RUN delete-phantoms.

  RUN oe/cleanrel.p (ROWID(oe-ordl)).

  FOR EACH oe-rel NO-LOCK
      WHERE oe-rel.company EQ oe-ordl.company
        AND oe-rel.ord-no  EQ oe-ordl.ord-no
        AND oe-rel.i-no    EQ oe-ordl.i-no
        AND oe-rel.line    EQ oe-ordl.line
      USE-INDEX ord-item
      
      BREAK BY oe-rel.rel-no
            BY oe-rel.b-ord-no
            BY oe-rel.po-no

      TRANSACTION:

    IF LAST-OF(oe-rel.po-no) OR oe-rel.rel-no EQ 0 THEN
      RUN create-report-record (ROWID(oe-rel), NO).
  END.

  FOR EACH oe-boll NO-LOCK
      WHERE oe-boll.company  EQ oe-ordl.company
        AND oe-boll.ord-no   EQ oe-ordl.ord-no
        AND oe-boll.i-no     EQ oe-ordl.i-no
        AND oe-boll.line     EQ oe-ordl.line
      USE-INDEX ord-no,

      FIRST oe-bolh WHERE oe-bolh.b-no EQ oe-boll.b-no NO-LOCK,
      
      FIRST oe-rell NO-LOCK
      WHERE oe-rell.company  EQ oe-boll.company
        AND oe-rell.ord-no   EQ oe-boll.ord-no
        AND oe-rell.line     EQ oe-boll.line
        AND oe-rell.i-no     EQ oe-boll.i-no
        AND oe-rell.r-no     EQ oe-boll.r-no
        AND oe-rell.rel-no   EQ oe-boll.rel-no
        AND oe-rell.b-ord-no EQ oe-boll.b-ord-no
        AND oe-rell.po-no    EQ oe-boll.po-no
      USE-INDEX ord-no,

      FIRST oe-relh NO-LOCK WHERE oe-relh.r-no EQ oe-boll.r-no
      
      BREAK BY oe-boll.r-no
            BY oe-boll.rel-no
            BY oe-boll.b-ord-no
            BY oe-boll.po-no

      TRANSACTION:

    IF FIRST-OF(oe-boll.po-no) THEN lv-qty = 0.

    lv-qty = lv-qty + oe-boll.qty.

    IF LAST-OF(oe-boll.po-no) AND lv-qty NE 0 THEN DO:
      RELEASE oe-rel.
      IF oe-rell.link-no NE 0 THEN
      FIND oe-rel NO-LOCK
          WHERE oe-rel.r-no EQ oe-rell.link-no
          USE-INDEX seq-no NO-ERROR.
      IF NOT AVAIL oe-rel THEN
      FIND FIRST oe-rel NO-LOCK
          WHERE oe-rel.company  EQ oe-rell.company
            AND oe-rel.link-no  EQ oe-rell.r-no
            AND oe-rel.ord-no   EQ oe-rell.ord-no
            AND oe-rel.i-no     EQ oe-rell.i-no
            AND oe-rel.line     EQ oe-rell.line
            AND oe-rel.rel-no   EQ oe-rell.rel-no
            AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
            AND oe-rel.po-no    EQ oe-rell.po-no
          USE-INDEX link NO-ERROR.
      IF NOT AVAIL oe-rel THEN
      FIND FIRST oe-rel NO-LOCK
          WHERE oe-rel.company  EQ oe-rell.company
            AND oe-rel.ord-no   EQ oe-rell.ord-no
            AND oe-rel.i-no     EQ oe-rell.i-no
            AND oe-rel.line     EQ oe-rell.line
            AND oe-rel.rel-no   EQ oe-rell.rel-no
            AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
            AND oe-rel.po-no    EQ oe-rell.po-no
          USE-INDEX ord-item NO-ERROR.

      IF AVAIL oe-rel THEN
      FIND CURRENT oe-rel EXCLUSIVE NO-ERROR NO-WAIT.

      IF AVAIL oe-rel THEN DO:
        FIND CURRENT oe-rel.
        ASSIGN
         oe-rel.link-no  = oe-rell.r-no
         oe-rel.rel-no   = oe-rell.rel-no
         oe-rel.b-ord-no = oe-rell.b-ord-no
         oe-rel.po-no    = oe-rell.po-no
         oe-rel.qty      = lv-qty.

        FOR EACH b-oe-rell NO-LOCK
            WHERE b-oe-rell.company  EQ oe-rel.company
              AND b-oe-rell.r-no     EQ oe-rel.link-no
              AND b-oe-rell.ord-no   EQ oe-rel.ord-no
              AND b-oe-rell.i-no     EQ oe-rel.i-no
              AND b-oe-rell.line     EQ oe-rel.line
              AND b-oe-rell.rel-no   EQ oe-rel.rel-no
              AND b-oe-rell.b-ord-no EQ oe-rel.b-ord-no
              AND b-oe-rell.po-no    EQ oe-rel.po-no
            USE-INDEX r-no:
          FIND b-oe-rell-exc WHERE ROWID(b-oe-rell-exc) EQ ROWID(b-oe-rell)
              EXCLUSIVE NO-ERROR NO-WAIT.
          IF AVAIL b-oe-rell-exc THEN b-oe-rell-exc.link-no = oe-rel.r-no.
        END.
      END.

      ELSE DO:
        /* FIND FIRST oe-rel NO-LOCK USE-INDEX seq-no NO-ERROR. */
        /* v-nxt-r-no = IF AVAIL oe-rel THEN oe-rel.r-no + 1 ELSE 1. */
        RUN oe/getNextRelNo.p (INPUT "oe-rel", OUTPUT v-nxt-r-no). 
        CREATE oe-rel.
        ASSIGN
         oe-rel.company   = oe-relh.company
         oe-rel.r-no      = v-nxt-r-no
         oe-rel.link-no   = oe-rell.r-no
         oe-rel.cust-no   = oe-relh.cust-no
         oe-rel.ord-no    = oe-rell.ord-no
         oe-rel.i-no      = oe-rell.i-no
         oe-rel.line      = oe-rell.line
         oe-rel.rel-no    = oe-rell.rel-no
         oe-rel.b-ord-no  = oe-rell.b-ord-no
         oe-rel.rel-date  = oe-relh.rel-date
         oe-rel.carrier   = oe-relh.carrier
         oe-rel.ship-no   = oe-relh.ship-no
         oe-rel.ship-id   = oe-relh.ship-id
         oe-rel.ship-i[1] = oe-relh.ship-i[1]
         oe-rel.ship-i[2] = oe-relh.ship-i[2]
         oe-rel.ship-i[3] = oe-relh.ship-i[3]
         oe-rel.ship-i[4] = oe-relh.ship-i[4]
         oe-rel.po-no     = oe-boll.po-no
         oe-rel.qty       = lv-qty.
         
        RUN CopyShipNote (oe-relh.rec_key, oe-rel.rec_key).
        RUN oe/custxship.p (oe-rel.company,
                            oe-rel.cust-no,
                            oe-rel.ship-id,
                            BUFFER shipto).

        if avail shipto then
          assign
           oe-rel.ship-addr[1] = shipto.ship-addr[1]
           oe-rel.ship-addr[2] = shipto.ship-addr[2]
           oe-rel.ship-city    = shipto.ship-city
           oe-rel.ship-state   = shipto.ship-state
           oe-rel.ship-zip     = shipto.ship-zip.
        RUN create-report-record (ROWID(oe-rel), NO).
      END.
    END.
  END.

  FOR EACH oe-rell
      WHERE oe-rell.company  EQ cocode
        AND oe-rell.ord-no   EQ oe-ordl.ord-no
        AND oe-rell.i-no     EQ oe-ordl.i-no
        AND oe-rell.line     EQ oe-ordl.line
        AND NOT CAN-FIND(FIRST oe-boll
                         WHERE oe-boll.company  EQ oe-rell.company
                           AND oe-boll.r-no     EQ oe-rell.r-no
                           AND oe-boll.ord-no   EQ oe-rell.ord-no
                           AND oe-boll.i-no     EQ oe-rell.i-no
                           AND oe-boll.line     EQ oe-rell.line
                           AND oe-boll.rel-no   EQ oe-rell.rel-no
                           AND oe-boll.b-ord-no EQ oe-rell.b-ord-no
                           AND oe-boll.po-no    EQ oe-rell.po-no
                         USE-INDEX ord-no)
      USE-INDEX ord-no NO-LOCK,

      FIRST oe-relh NO-LOCK
      WHERE oe-relh.r-no    EQ oe-rell.r-no
        AND (oe-relh.posted EQ NO OR relpost-chr EQ "Nothing")
      
      BREAK BY oe-rell.r-no
            BY oe-rell.rel-no
            BY oe-rell.b-ord-no
            BY oe-rell.po-no

      TRANSACTION:

    IF FIRST-OF(oe-rell.po-no) THEN lv-qty = 0.

    lv-qty = lv-qty + oe-rell.qty.

    IF LAST-OF(oe-rell.po-no) AND lv-qty NE 0 THEN DO:
      RELEASE b-oe-rell.
      IF oe-relh.posted THEN
      FOR EACH b-oe-rell
          WHERE b-oe-rell.company EQ oe-rell.company
            AND b-oe-rell.r-no    EQ oe-rell.r-no
            AND ROWID(b-oe-rell)  NE ROWID(oe-rell)
            AND CAN-FIND(FIRST oe-boll
                         WHERE oe-boll.company  EQ b-oe-rell.company
                           AND oe-boll.ord-no   EQ b-oe-rell.ord-no
                           AND oe-boll.i-no     EQ b-oe-rell.i-no
                           AND oe-boll.line     EQ b-oe-rell.line
                           AND oe-boll.r-no     EQ b-oe-rell.r-no
                           AND oe-boll.rel-no   EQ b-oe-rell.rel-no
                           AND oe-boll.b-ord-no EQ b-oe-rell.b-ord-no
                           AND oe-boll.po-no    EQ b-oe-rell.po-no
                         USE-INDEX ord-no)
          USE-INDEX r-no NO-LOCK:

        LEAVE.
      END.

      IF NOT AVAIL b-oe-rell THEN DO:
        RELEASE oe-rel.
        IF oe-rell.link-no NE 0 AND oe-relh.posted THEN
        FIND oe-rel NO-LOCK
            WHERE oe-rel.r-no EQ oe-rell.link-no
            USE-INDEX seq-no NO-ERROR.
        IF NOT AVAIL oe-rel THEN
        FIND FIRST oe-rel NO-LOCK
            WHERE oe-rel.company  EQ oe-rell.company
              AND oe-rel.link-no  EQ oe-rell.r-no
              AND oe-rel.ord-no   EQ oe-rell.ord-no
              AND oe-rel.i-no     EQ oe-rell.i-no
              AND oe-rel.line     EQ oe-rell.line
              AND oe-rel.rel-no   EQ oe-rell.rel-no
              AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
              AND oe-rel.po-no    EQ oe-rell.po-no
            USE-INDEX link NO-ERROR.
        IF NOT AVAIL oe-rel THEN
        FIND FIRST oe-rel NO-LOCK
            WHERE oe-rel.company  EQ oe-rell.company
              AND oe-rel.ord-no   EQ oe-rell.ord-no
              AND oe-rel.i-no     EQ oe-rell.i-no
              AND oe-rel.line     EQ oe-rell.line
              AND oe-rel.rel-no   EQ oe-rell.rel-no
              AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
              AND oe-rel.po-no    EQ oe-rell.po-no
            USE-INDEX ord-item NO-ERROR.

        IF NOT AVAIL oe-rel THEN DO:
          FIND FIRST oe-rel USE-INDEX seq-no NO-LOCK NO-ERROR.
          v-nxt-r-no = IF AVAIL oe-rel THEN oe-rel.r-no + 1 ELSE 1.

          CREATE oe-rel.
          ASSIGN
           oe-rel.company   = oe-relh.company
           oe-rel.r-no      = v-nxt-r-no
           oe-rel.link-no   = IF oe-relh.posted THEN oe-rell.r-no ELSE 0
           oe-rel.cust-no   = oe-relh.cust-no
           oe-rel.ord-no    = oe-rell.ord-no
           oe-rel.i-no      = oe-rell.i-no
           oe-rel.line      = oe-rell.line
           oe-rel.rel-no    = oe-rell.rel-no
           oe-rel.b-ord-no  = oe-rell.b-ord-no
           oe-rel.rel-date  = oe-relh.rel-date
           oe-rel.carrier   = oe-relh.carrier
           oe-rel.ship-no   = oe-relh.ship-no
           oe-rel.ship-id   = oe-relh.ship-id
           oe-rel.ship-i[1] = oe-relh.ship-i[1]
           oe-rel.ship-i[2] = oe-relh.ship-i[2]
           oe-rel.ship-i[3] = oe-relh.ship-i[3]
           oe-rel.ship-i[4] = oe-relh.ship-i[4]
           oe-rel.po-no     = oe-rell.po-no
           oe-rel.qty       = lv-qty.
           
          RUN CopyShipNote (oe-relh.rec_key, oe-rel.rec_key).
          RUN oe/custxship.p (oe-rel.company,
                              oe-rel.cust-no,
                              oe-rel.ship-id,
                              BUFFER shipto).

          if avail shipto then
            assign
             oe-rel.ship-addr[1] = shipto.ship-addr[1]
             oe-rel.ship-addr[2] = shipto.ship-addr[2]
             oe-rel.ship-city    = shipto.ship-city
             oe-rel.ship-state   = shipto.ship-state
             oe-rel.ship-zip     = shipto.ship-zip.

          RUN create-report-record (ROWID(oe-rel), NO).
        END.

        ELSE DO:
          FIND CURRENT oe-rel EXCLUSIVE NO-ERROR NO-WAIT.

          IF AVAIL oe-rel THEN DO:
            IF oe-relh.posted THEN DO:
              ASSIGN
               oe-rel.link-no  = oe-rell.r-no
               oe-rel.rel-no   = oe-rell.rel-no
               oe-rel.b-ord-no = oe-rell.b-ord-no
               oe-rel.po-no    = oe-rell.po-no
               oe-rel.qty      = lv-qty.

              FOR EACH b-oe-rell NO-LOCK
                  WHERE b-oe-rell.company  EQ oe-rel.company
                    AND b-oe-rell.r-no     EQ oe-rel.link-no
                    AND b-oe-rell.ord-no   EQ oe-rel.ord-no
                    AND b-oe-rell.i-no     EQ oe-rel.i-no
                    AND b-oe-rell.line     EQ oe-rel.line
                    AND b-oe-rell.rel-no   EQ oe-rel.rel-no
                    AND b-oe-rell.b-ord-no EQ oe-rel.b-ord-no
                    AND b-oe-rell.po-no    EQ oe-rel.po-no
                  USE-INDEX r-no:
                FIND b-oe-rell-exc WHERE ROWID(b-oe-rell-exc) EQ ROWID(b-oe-rell)
                    EXCLUSIVE NO-ERROR NO-WAIT.
                IF AVAIL b-oe-rell-exc THEN b-oe-rell-exc.link-no = oe-rel.r-no.
              END.
            END.

            ELSE DO:
              IF oe-rel.link-no NE 0 THEN oe-rel.link-no = 0.

              FIND FIRST tt-report WHERE tt-report.rec-id EQ RECID(oe-rel) NO-ERROR.
              IF AVAIL tt-report THEN tt-report.qty = lv-qty.
            END.
          END.
        END.
      END.
    END.
  END.
 

  FOR EACH oe-rel NO-LOCK
      WHERE oe-rel.company EQ oe-ordl.company
        AND oe-rel.ord-no  EQ oe-ordl.ord-no
        AND oe-rel.i-no    EQ oe-ordl.i-no
        AND oe-rel.line    EQ oe-ordl.line
      USE-INDEX ord-item:

      RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).
      IF INDEX("SIL",lv-stat) GT 0 OR 
         (INDEX("CZ",lv-stat) LE 0 AND oe-rel.qty EQ 0) THEN
      DO:
         REPEAT:
            FIND FIRST b-oe-rel WHERE
                 ROWID(b-oe-rel) EQ ROWID(oe-rel)
                 EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

            IF AVAIL b-oe-rel THEN
            DO:
               b-oe-rel.qty = b-oe-rel.tot-qty.
               FIND CURRENT b-oe-rel NO-LOCK.
               RELEASE b-oe-rel.
               LEAVE.
            END.
         END.
      END.
  END.

  RELEASE oe-rel.
  RELEASE b-oe-rell.
  RELEASE oe-rell.
  RELEASE oe-boll.
  RELEASE tt-report.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-report-record C-Win 
PROCEDURE create-report-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
  DEF INPUT PARAM ip-phantom AS LOG NO-UNDO.

  FIND oe-rel WHERE ROWID(oe-rel) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL oe-rel THEN
  FIND FIRST oe-ord
      WHERE oe-ord.company EQ oe-rel.company 
        AND oe-ord.ord-no  EQ oe-rel.ord-no
      NO-LOCK NO-ERROR.

  IF AVAIL oe-ord THEN DO:
    FIND FIRST tt-report
        WHERE tt-report.rec-id EQ RECID(oe-rel)
        NO-ERROR.

    IF NOT AVAIL tt-report THEN CREATE tt-report.

    {oe/rel-stat.i lv-stat}

    RELEASE inv-line.
    IF lv-stat EQ "Z" AND AVAIL oe-boll THEN
    FIND FIRST inv-line
        WHERE inv-line.company EQ oe-boll.company
          AND inv-line.b-no    EQ oe-boll.b-no
          AND inv-line.ord-no  EQ oe-boll.ord-no
          AND inv-line.i-no    EQ oe-boll.i-no
          AND inv-line.po-no   NE ""
        NO-LOCK NO-ERROR.

    RUN create-report-record-1 (ip-phantom,
                                IF AVAIL oe-relh THEN oe-relh.rel-date
                                                 ELSE oe-rel.rel-date).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-report-record-1 C-Win 
PROCEDURE create-report-record-1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAM ip-phantom AS LOG NO-UNDO.
   DEF INPUT PARAM ip-date AS DATE NO-UNDO.

    ASSIGN
     tt-report.term-id = v-term
     tt-report.rec-id  = RECID(oe-rel)
     ld-date           = ip-date
     tt-report.key-01  = STRING(YEAR(ld-date),"9999") +
                         STRING(MONTH(ld-date),"99")  +
                         STRING(DAY(ld-date),"99")
     tt-report.key-02  = STRING(ld-date,"99999999")
     tt-report.phantom = ip-phantom
     tt-report.po-no   = oe-rel.po-no
     tt-report.qty     = oe-rel.qty
     tt-report.printed = (AVAIL oe-relh AND oe-relh.printed) OR
                         INDEX("PCZ",lv-stat) GT 0.
    tt-report.s-code = IF ll-transfer            THEN "T"
                       ELSE
                       IF oe-ordl.is-a-component AND
                          (oe-rel.s-code = "" OR
                           oe-rel.s-code NE "T")   THEN "S"
                       ELSE
                       IF oe-rel.s-code <> ""      THEN oe-rel.s-code
                       ELSE
                       IF AVAIL oe-rell          THEN oe-rell.s-code
                                                 ELSE "B".

   ASSIGN
          tt-report.lot-no      = oe-rel.lot-no
          tt-report.freight-pay = oe-rel.frt-pay
          tt-report.fob         = oe-rel.fob-code.

    
   ASSIGN 
          tt-report.sell-price  = oe-rel.sell-price
          tt-report.zero-sprice = oe-rel.zeroPrice > 0.
       

    IF oeinq THEN 
      tt-report.key-01 = STRING(9999999999 - INT(tt-report.key-01),"9999999999").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CopyShipNote d-oeitem
PROCEDURE CopyShipNote PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Copies Ship Note from rec_key to rec_key
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcRecKeyFrom AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcRecKeyTo AS CHARACTER NO-UNDO.

DEFINE VARIABLE hNotesProcs AS HANDLE NO-UNDO.

    RUN "sys/NotesProcs.p" PERSISTENT SET hNotesProcs.  

    RUN CopyShipNote IN hNotesProcs (ipcRecKeyFrom, ipcRecKeyTo).

    DELETE OBJECT hNotesProcs.   

END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-phantoms C-Win 
PROCEDURE delete-phantoms :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF BUFFER b-oe-rel FOR oe-rel.
   DEF BUFFER b-tt-report FOR tt-report.

   FOR EACH b-tt-report WHERE b-tt-report.phantom:
       FIND FIRST b-oe-rel WHERE RECID(b-oe-rel) EQ b-tt-report.rec-id NO-ERROR.
       IF AVAIL b-oe-rel THEN DELETE b-oe-rel.
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
  DISPLAY begin_ord-no 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_ord-no btn-pop BROWSE-1 btn-status 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

