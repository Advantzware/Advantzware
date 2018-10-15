&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-dash.w

  Description: Management Dashboard

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
{salrep/dashtt.i NEW}

DEF TEMP-TABLE tt-report NO-UNDO LIKE report 
    FIELD DATE AS DATE
    FIELD row-id AS ROWID
    FIELD qty AS DEC
    FIELD amt       LIKE ar-invl.amt        FORMAT "->>>>>>>9.99"
    FIELD cash-date LIKE ar-inv.inv-date
    FIELD misc AS LOG
    FIELD cost AS DEC
    FIELD msf AS DEC
    FIELD is-duplicate AS LOG.

DEF TEMP-TABLE tt-ap-report NO-UNDO LIKE report
    FIELD check-date        LIKE ap-pay.check-date
    FIELD inv-no            LIKE ap-payl.inv-no
    FIELD amt-paid          LIKE ap-payl.amt-paid.

DEF TEMP-TABLE work-tmp NO-UNDO
   field job like job.job
   field frm like job-mch.frm
   field blank-no like job-mch.blank-no
   FIELD sort-field AS CHAR
   field dept as char format 'xx'
   field m-code like mach.m-code
   field pass like job-mch.pass
   field r-act-hrs as dec format '>>>>9.99'
   field m-act-hrs as dec format '>>>>9.99'
   field dt-chg-hrs as dec format '>>>>9.99'
   field dt-nochg-hrs as dec format '>>>>9.99'
   field qty as dec format '>>>>>>>>9'
   field msf as dec format '>>>>>.999'
   INDEX work-tmp job frm blank-no dept m-code pass sort-field.

def TEMP-TABLE w-data no-undo
  field w-sman-no   AS CHAR
  field w-sqft      LIKE itemfg.t-sqft format "->>>9.999"    extent 3
  field w-amt       like ar-inv.gross  format "->>>,>>9.99"  extent 3
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

def TEMP-TABLE w-ord2 NO-UNDO
  field qty-due as int format "->>>,>>>,>>9"
  field qty like oe-ordl.qty
  field cost like oe-ordl.cost
  field price like oe-ordl.price format ">>,>>9.99"
  field uom like oe-ordl.pr-uom
  field disc like oe-ordl.disc
  field t-price like oe-ordl.t-price format ">>>,>>9.99"
  field rel-date like oe-relh.rel-date
  field rel-stat as char
  field inv-qty like oe-ordl.qty .

DEF BUFFER b-mach FOR mach.
DEF BUFFER xreport FOR tt-report.
DEF BUFFER b-tt-report FOR tt-report.
DEF BUFFER b-ar-invl FOR ar-invl.
DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER b-ar-cashl FOR ar-cashl.
DEF BUFFER xoe-ord FOR oe-ord.
DEF BUFFER b-itemfg FOR itemfg.
DEF BUFFER b-account FOR account.
DEF BUFFER b-company FOR company.

DEF VAR counter AS INT NO-UNDO.
DEF VAR v-sman-no AS CHAR NO-UNDO.
DEF VAR v-prodc AS CHAR NO-UNDO.
DEF VAR cp-part-no AS CHAR NO-UNDO.
DEF VAR cp-rowid AS ROWID NO-UNDO.
DEF VAR fsman AS CHAR NO-UNDO.
DEF VAR tsman AS CHAR INIT "zzz" NO-UNDO.
DEF VAR v-dso AS DEC NO-UNDO.
DEFINE VARIABLE iMach AS INTEGER INIT 100 NO-UNDO .
DEFINE VARIABLE iSales AS INTEGER INIT 4 NO-UNDO .
DEFINE BUFFER bf-user-print FOR user-print .
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
&Scoped-define BROWSE-NAME browse-ar

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES account mach company

/* Definitions for BROWSE browse-ar                                     */
&Scoped-define FIELDS-IN-QUERY-browse-ar account.actnum account.dscr account.TYPE   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browse-ar   
&Scoped-define SELF-NAME browse-ar
&Scoped-define QUERY-STRING-browse-ar FOR EACH account WHERE      account.company = fi_company:SCREEN-VALUE NO-LOCK
&Scoped-define OPEN-QUERY-browse-ar OPEN QUERY {&SELF-NAME} FOR EACH account WHERE      account.company = fi_company:SCREEN-VALUE NO-LOCK.
&Scoped-define TABLES-IN-QUERY-browse-ar account
&Scoped-define FIRST-TABLE-IN-QUERY-browse-ar account


/* Definitions for BROWSE browse-machine                                */
&Scoped-define FIELDS-IN-QUERY-browse-machine mach.m-code mach.m-dscr   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browse-machine   
&Scoped-define SELF-NAME browse-machine
&Scoped-define QUERY-STRING-browse-machine FOR EACH mach WHERE      mach.company = fi_company:SCREEN-VALUE AND      mach.loc = locode      NO-LOCK BY mach.m-code
&Scoped-define OPEN-QUERY-browse-machine OPEN QUERY {&SELF-NAME} FOR EACH mach WHERE      mach.company = fi_company:SCREEN-VALUE AND      mach.loc = locode      NO-LOCK BY mach.m-code.
&Scoped-define TABLES-IN-QUERY-browse-machine mach
&Scoped-define FIRST-TABLE-IN-QUERY-browse-machine mach


/* Definitions for BROWSE browse-sales-forecast                         */
&Scoped-define FIELDS-IN-QUERY-browse-sales-forecast company.company company.NAME   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browse-sales-forecast   
&Scoped-define SELF-NAME browse-sales-forecast
&Scoped-define QUERY-STRING-browse-sales-forecast FOR EACH company NO-LOCK
&Scoped-define OPEN-QUERY-browse-sales-forecast OPEN QUERY {&SELF-NAME} FOR EACH company NO-LOCK.
&Scoped-define TABLES-IN-QUERY-browse-sales-forecast company
&Scoped-define FIRST-TABLE-IN-QUERY-browse-sales-forecast company


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-browse-ar}~
    ~{&OPEN-QUERY-browse-machine}~
    ~{&OPEN-QUERY-browse-sales-forecast}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-9 fi_as-of-date fi_company ~
browse-machine tg_round tg_set-comp browse-sales-forecast browse-ar btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fi_as-of-date fi_company tg_round ~
tg_set-comp 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
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
     SIZE 94 BY 22.14.

DEFINE VARIABLE tg_round AS LOGICAL INITIAL no 
     LABEL "Round dollar amounts?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81 NO-UNDO.

DEFINE VARIABLE tg_set-comp AS LOGICAL INITIAL no 
     LABEL "Set Components Use Header Price?" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY 1.19 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY browse-ar FOR 
      account SCROLLING.

DEFINE QUERY browse-machine FOR 
      mach SCROLLING.

DEFINE QUERY browse-sales-forecast FOR 
      company SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE browse-ar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browse-ar C-Win _FREEFORM
  QUERY browse-ar NO-LOCK DISPLAY
      account.actnum FORMAT "X(25)" COLUMN-LABEL "Account Number"
      account.dscr FORMAT "X(45)" COLUMN-LABEL "Description"
      account.TYPE FORMAT "X(1)" COLUMN-LABEL "Type"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 48 BY 5.62 ROW-HEIGHT-CHARS .71 FIT-LAST-COLUMN.

DEFINE BROWSE browse-machine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browse-machine C-Win _FREEFORM
  QUERY browse-machine NO-LOCK DISPLAY
      mach.m-code FORMAT "X(6)" width 10
mach.m-dscr FORMAT "X(20)" COLUMN-LABEL "Description"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 44.4 BY 7.19 ROW-HEIGHT-CHARS .52 FIT-LAST-COLUMN.

DEFINE BROWSE browse-sales-forecast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browse-sales-forecast C-Win _FREEFORM
  QUERY browse-sales-forecast NO-LOCK DISPLAY
      company.company FORMAT "X(3)" COLUMN-LABEL "Code"
      company.NAME FORMAT "X(30)" COLUMN-LABEL "Description"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 41.4 BY 5.62 ROW-HEIGHT-CHARS .57 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fi_as-of-date AT ROW 2.81 COL 12.2 COLON-ALIGNED
     fi_company AT ROW 2.81 COL 42.6 COLON-ALIGNED
     browse-machine AT ROW 5.62 COL 2.6
     tg_round AT ROW 5.76 COL 49
     tg_set-comp AT ROW 7.43 COL 49 WIDGET-ID 2
     browse-sales-forecast AT ROW 15.1 COL 2.6
     browse-ar AT ROW 15.1 COL 46
     btn-ok AT ROW 21.71 COL 21
     btn-cancel AT ROW 21.71 COL 57
     "Select Up To 99 Machines for Production:" VIEW-AS TEXT
          SIZE 48 BY .62 AT ROW 4.38 COL 2.8
          FONT 6
     "Select Up to 4 Companies" VIEW-AS TEXT
          SIZE 30.4 BY .62 AT ROW 13.38 COL 2.6
          FONT 6
     "Release Sales Forecast:" VIEW-AS TEXT
          SIZE 28 BY .62 AT ROW 6.76 COL 49 WIDGET-ID 4
          FONT 6
     "for Sales Forecast:" VIEW-AS TEXT
          SIZE 24.4 BY .62 AT ROW 14.14 COL 2.6
          FONT 6
     "Select A/R Account(s) for Avg DSO:" VIEW-AS TEXT
          SIZE 42.4 BY .62 AT ROW 13.38 COL 46.2
          FONT 6
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 3
          BGCOLOR 2 
     RECT-9 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 94.4 BY 22.24.


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
         TITLE              = "Management Highlights"
         HEIGHT             = 22.24
         WIDTH              = 94.4
         MAX-HEIGHT         = 22.24
         MAX-WIDTH          = 94.4
         VIRTUAL-HEIGHT     = 22.24
         VIRTUAL-WIDTH      = 94.4
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


/* BROWSE-TAB browse-machine fi_company FRAME-A */
/* BROWSE-TAB browse-sales-forecast tg_set-comp FRAME-A */
/* BROWSE-TAB browse-ar browse-sales-forecast FRAME-A */
ASSIGN 
       fi_company:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browse-ar
/* Query rebuild information for BROWSE browse-ar
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH account WHERE
     account.company = fi_company:SCREEN-VALUE NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE browse-ar */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browse-machine
/* Query rebuild information for BROWSE browse-machine
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH mach WHERE
     mach.company = fi_company:SCREEN-VALUE AND
     mach.loc = locode
     NO-LOCK BY mach.m-code.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE browse-machine */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browse-sales-forecast
/* Query rebuild information for BROWSE browse-sales-forecast
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH company NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE browse-sales-forecast */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Management Highlights */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Management Highlights */
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

    ASSIGN {&displayed-objects} fi_company tg_round.

    EMPTY TEMP-TABLE tt-raw-op.
    EMPTY TEMP-TABLE tt-raw-prod.
    EMPTY TEMP-TABLE work-tmp.
    EMPTY TEMP-TABLE tt-report.
    EMPTY TEMP-TABLE w-data.
    EMPTY TEMP-TABLE tt-sales-prod-cat.
    EMPTY TEMP-TABLE tt-raw-salesmen.
    EMPTY TEMP-TABLE tt-raw-sales.
    EMPTY TEMP-TABLE tt-sales-forecast.
    EMPTY TEMP-TABLE tt-ar-dso.

    IF NOT CAN-FIND(FIRST company WHERE
       company.company EQ fi_company) THEN
       DO:
          MESSAGE "Invalid Company."
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          APPLY "ENTRY:U" TO fi_company IN FRAME {&FRAME-NAME}.
          LEAVE.
       END.

    IF BROWSE browse-machine:NUM-SELECTED-ROWS GT 99 THEN
    DO:
       MESSAGE "More than 99 Machines are Selected."
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY "ENTRY:U" TO BROWSE browse-machine.
       LEAVE.
    END.

    IF BROWSE browse-sales-forecast:NUM-SELECTED-ROWS GT 4 THEN
    DO:
       MESSAGE "More than 4 Companines are Selected."
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY "ENTRY:U" TO BROWSE browse-sales-forecast.
       LEAVE.
    END.

    RUN reftable-proc.

    /*blank prod cat*/
    CREATE tt-sales-prod-cat.
    tt-sales-prod-cat.DATE = fi_as-of-date.
    RELEASE tt-sales-prod-cat.

    FOR EACH fgcat FIELDS(procat) WHERE
        fgcat.company eq fi_company
        NO-LOCK:

        CREATE tt-sales-prod-cat.
        ASSIGN tt-sales-prod-cat.prod-cat = fgcat.procat
               tt-sales-prod-cat.DATE     = fi_as-of-date.
        RELEASE tt-sales-prod-cat.
    END.

    IF NOT CAN-FIND(FIRST tt-sales-prod-cat WHERE
       tt-sales-prod-cat.prod-cat EQ "MEMO") THEN
       DO:
          CREATE tt-sales-prod-cat.
          ASSIGN tt-sales-prod-cat.prod-cat = "MEMO"
                 tt-sales-prod-cat.DATE     = fi_as-of-date.
          RELEASE tt-sales-prod-cat.
       END.

    IF NOT CAN-FIND(FIRST tt-sales-prod-cat WHERE
       tt-sales-prod-cat.prod-cat EQ "MISC") THEN
       DO:
          CREATE tt-sales-prod-cat.
          ASSIGN tt-sales-prod-cat.prod-cat = "MISC"
                 tt-sales-prod-cat.DATE     = fi_as-of-date.
          RELEASE tt-sales-prod-cat.
       END.

    FOR EACH sman WHERE
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

      APPLY "close" TO THIS-PROCEDURE.

   END.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_company
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_company C-Win
ON ENTRY OF fi_company IN FRAME FRAME-A /* Company */
DO:
  SELF:MODIFIED = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_company C-Win
ON LEAVE OF fi_company IN FRAME FRAME-A /* Company */
DO:
   DO WITH FRAME {&FRAME-NAME}:

      IF SELF:MODIFIED THEN
      DO:
         IF BROWSE browse-sales-forecast:NUM-SELECTED-ROWS GT 0 THEN
            IF browse-sales-forecast:DESELECT-ROWS() THEN.

         FOR EACH reftable WHERE
             reftable.reftable EQ "HM1SF" AND
             reftable.company EQ USERID("NOSWEAT")
             NO-LOCK,
             FIRST company WHERE
                   company.company = reftable.loc
                   NO-LOCK:

             REPOSITION browse-sales-forecast TO ROWID ROWID(company).
             IF browse-sales-forecast:SELECT-FOCUSED-ROW() THEN.
         END.

         FIND FIRST company WHERE
              company.company EQ fi_company:SCREEN-VALUE
              NO-LOCK NO-ERROR.

         IF AVAIL company THEN
         DO:
            REPOSITION browse-sales-forecast TO ROWID ROWID(company).
            IF browse-sales-forecast:SELECT-FOCUSED-ROW() THEN.

            RELEASE company.
         END.

         FIND FIRST company NO-LOCK NO-ERROR.

         IF AVAIL company THEN
            REPOSITION browse-sales-forecast TO ROWID ROWID(company).

         ASSIGN fi_company.

         CLOSE QUERY browse-machine.

         OPEN QUERY browse-machine FOR EACH mach WHERE
              mach.company = fi_company AND
              mach.loc = locode
              NO-LOCK BY mach.m-code.

         FOR EACH reftable WHERE
             reftable.reftable EQ "HM1" AND
             reftable.company EQ USERID("NOSWEAT") AND
             reftable.loc = fi_company
             NO-LOCK,
             FIRST mach WHERE
                   mach.company = fi_company AND
                   mach.loc = locode AND
                   mach.m-code = reftable.CODE
                   NO-LOCK:

             REPOSITION browse-machine TO ROWID ROWID(mach).
             IF browse-machine:SELECT-FOCUSED-ROW() THEN.
         END.

         FOR EACH b-mach WHERE
             b-mach.company EQ fi_company:SCREEN-VALUE AND
             b-mach.loc EQ locode
             NO-LOCK
             BY b-mach.m-code:

            LEAVE.
         END.

         IF AVAIL b-mach THEN
            REPOSITION browse-machine TO ROWID ROWID(b-mach).

         CLOSE QUERY browse-ar.

         OPEN QUERY browse-ar FOR EACH account WHERE
              account.company = fi_company no-lock.
         END.

         FOR EACH reftable WHERE
             reftable.reftable EQ "HM1Acct" AND
             reftable.company EQ USERID("NOSWEAT") AND
             reftable.loc = fi_company:SCREEN-VALUE
             NO-LOCK,
             FIRST account WHERE
                   account.company = fi_company:SCREEN-VALUE AND
                   account.actnum = reftable.CODE
                   NO-LOCK:

             REPOSITION browse-ar TO ROWID ROWID(account).
             IF browse-ar:SELECT-FOCUSED-ROW() THEN.
         END.

         FIND FIRST account WHERE
              account.company = fi_company:SCREEN-VALUE
              NO-LOCK NO-ERROR.

         IF AVAIL account THEN
            REPOSITION browse-ar TO ROWID ROWID(account).
      END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME browse-ar
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

  fi_company = cocode.

  RUN enable_UI.

  {methods/nowait.i}
  {custom/usrprint.i}

  DO WITH FRAME {&FRAME-NAME}:

     FIND FIRST company WHERE
          company.company EQ fi_company:SCREEN-VALUE
          NO-LOCK NO-ERROR.

     IF AVAIL company THEN
     DO:
         REPOSITION browse-sales-forecast TO ROWID ROWID(company).
         IF browse-sales-forecast:SELECT-FOCUSED-ROW() THEN.

         RELEASE company.
     END.


      FIND FIRST bf-user-print NO-LOCK
          WHERE (bf-user-print.company  EQ cocode  OR bf-user-print.company EQ "")       
          AND bf-user-print.program-id EQ "HM1SF" 
          AND bf-user-print.user-id EQ USERID(LDBNAME(1)) NO-ERROR.

      IF AVAIL bf-user-print THEN DO:
          DO i = 1 TO iSales:
              FIND FIRST company NO-LOCK
                  WHERE company.company = bf-user-print.field-value[i]  NO-ERROR.

              IF AVAIL company AND bf-user-print.field-value[i] NE ""  THEN
                   REPOSITION browse-sales-forecast TO ROWID ROWID(company).
                   IF browse-sales-forecast:SELECT-FOCUSED-ROW() THEN.
           END.
      END.    /*avail user-print */ 
      RELEASE bf-user-print .

     FIND FIRST company NO-LOCK NO-ERROR.

     IF AVAIL company THEN
        REPOSITION browse-sales-forecast TO ROWID ROWID(company).

     CLOSE QUERY browse-machine.

     OPEN QUERY browse-machine FOR EACH mach WHERE
          mach.company = fi_company:SCREEN-VALUE AND
          mach.loc = locode
          NO-LOCK BY mach.m-code.
     
      FIND FIRST bf-user-print NO-LOCK
          WHERE bf-user-print.company    EQ cocode        
          AND bf-user-print.program-id EQ "HM1" 
          AND bf-user-print.user-id EQ USERID(LDBNAME(1)) NO-ERROR.

      IF AVAIL bf-user-print THEN DO:
          DO i = 1 TO iMach:
              IF bf-user-print.field-value[i] EQ "" THEN LEAVE .
              FIND FIRST mach NO-LOCK
                  WHERE mach.company = fi_company:SCREEN-VALUE AND
                  mach.loc = locode AND
                  mach.m-code = bf-user-print.field-value[i] NO-ERROR.
              IF AVAIL mach AND bf-user-print.field-value[i] NE ""  THEN
                  REPOSITION browse-machine TO ROWID ROWID(mach).
                  IF browse-machine:SELECT-FOCUSED-ROW() THEN.
          END.
       END.    /*avail user-print */ 

     FOR EACH b-mach WHERE
         b-mach.company EQ fi_company:SCREEN-VALUE AND
         b-mach.loc = locode
         NO-LOCK
         BY b-mach.m-code:

         LEAVE.
     END.

     IF AVAIL b-mach THEN
        REPOSITION browse-machine TO ROWID ROWID(b-mach).

     CLOSE QUERY browse-ar.

     OPEN QUERY browse-ar FOR EACH account WHERE
          account.company = fi_company:SCREEN-VALUE NO-LOCK.
     
      FIND FIRST bf-user-print NO-LOCK
          WHERE bf-user-print.company    EQ cocode        
          AND bf-user-print.program-id EQ "HM1Acct" 
          AND bf-user-print.user-id EQ USERID(LDBNAME(1)) NO-ERROR.

      IF AVAIL bf-user-print THEN DO:
          DO i = 1 TO iMach:
              IF bf-user-print.field-value[i] EQ "" THEN LEAVE .
              FIND FIRST account NO-LOCK
                  WHERE account.company EQ fi_company:SCREEN-VALUE 
                  AND account.actnum EQ bf-user-print.field-value[i] NO-ERROR.

              IF AVAIL account AND bf-user-print.field-value[i] NE ""  THEN
                REPOSITION browse-ar TO ROWID ROWID(account).
                IF browse-ar:SELECT-FOCUSED-ROW() THEN.
          END.
  END.    /*avail user-print */ 

     FIND FIRST account WHERE
          account.company = fi_company:SCREEN-VALUE
          NO-LOCK NO-ERROR.

     IF AVAIL account THEN
        REPOSITION browse-ar TO ROWID ROWID(account).

     fi_as-of-date:SCREEN-VALUE = STRING(TODAY).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ap-ar-proc C-Win 
PROCEDURE ap-ar-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*from arrep\r-cashsm.w  include terms discount is yes
     Show only invoices ... 0 days*/

   def var v-sman as   CHAR NO-UNDO.
   DEF VAR i AS INT NO-UNDO.
   def var v-amt  like ar-cashl.amt-paid extent 2 NO-UNDO.
   def var v-dsc  like v-amt NO-UNDO.
   DEF VAR v-pct AS DEC NO-UNDO.
   DEF VAR v-start-of-month AS DATE NO-UNDO.

   EMPTY TEMP-TABLE tt-ar-ap.
   EMPTY TEMP-TABLE tt-report.

   CREATE tt-ar-ap.

   v-start-of-month = DATE(MONTH(fi_as-of-date),1,YEAR(fi_as-of-date)).

   for each cust FIELDS(company cust-no sman) where
       cust.company eq fi_company no-lock:

    for each ar-inv FIELDS(x-no inv-date inv-date) 
        where ar-inv.company  eq fi_company
          and ar-inv.posted   eq yes
          and ar-inv.cust-no  eq cust.cust-no
          and ar-inv.inv-date ge v-start-of-month
          and ar-inv.inv-date le fi_as-of-date
          and ar-inv.terms    eq "CASH"
        no-lock,
        each ar-invl FIELDS(sman s-pct inv-no )
        where ar-invl.x-no eq ar-inv.x-no
        no-lock

        /*transaction*/ :

      do i = 1 to 3:
        v-sman = if ar-invl.sman[i] eq "" and i eq 1 then cust.sman
                 else ar-invl.sman[i].

        if i ne 1 and
           (v-sman eq "" or ar-invl.s-pct[i] eq 0) then next.

        find first tt-report
            where tt-report.key-01  eq v-sman
              and tt-report.key-02  eq string(ar-invl.inv-no,"9999999999")
              and tt-report.rec-id  eq recid(ar-invl)
            no-lock no-error.
        if not avail tt-report then do:
          create tt-report.
          assign
           tt-report.key-01  = v-sman
           tt-report.key-02  = string(ar-invl.inv-no,"9999999999")
           tt-report.key-04  = STRING(ar-inv.inv-date,"99/99/9999")
           tt-report.rec-id  = recid(ar-invl).
        end.
      end.
    end.      

    for each ar-cash FIELDS(c-no cust-no check-date)
        where ar-cash.company    eq fi_company
          and ar-cash.cust-no    eq cust.cust-no
          and ar-cash.check-date ge v-start-of-month
          and ar-cash.check-date le fi_as-of-date
          and ar-cash.posted     eq yes
          and ar-cash.check-no   ne 0
        no-lock,
        each ar-cashl FIELDS(inv-no)
        where ar-cashl.c-no   eq ar-cash.c-no
          and ar-cashl.posted eq yes
          and ar-cashl.memo   eq no
         /* and (v-days         eq 0 or
               (ar-cash.check-date - ar-cashl.inv-date gt v-days and
                ar-cashl.inv-no ne 0)) */
        no-lock

        /*transaction*/ :

      if ar-cashl.inv-no ne 0 then
      for each ar-invl FIELDS(sman s-pct inv-no)
          where ar-invl.company eq fi_company
            and ar-invl.cust-no eq ar-cash.cust-no
            and ar-invl.inv-no  eq ar-cashl.inv-no
          no-lock:

        do i = 1 to 3:
          v-sman = if ar-invl.sman[i] eq "" and i eq 1 then cust.sman
                   else ar-invl.sman[i].

          if i ne 1 and
             (v-sman eq "" or ar-invl.s-pct[i] eq 0) then next.

          find first tt-report
              where tt-report.key-01  eq v-sman
                and tt-report.key-02  eq string(ar-invl.inv-no,"9999999999")
                and tt-report.rec-id  eq recid(ar-cashl)
              no-lock no-error.
          if not avail tt-report then do:
            create tt-report.
            assign
             tt-report.key-01  = v-sman
             tt-report.key-02  = string(ar-invl.inv-no,"9999999999")
             tt-report.key-04  = STRING(ar-cash.check-date,"99/99/9999")    
             tt-report.rec-id  = recid(ar-cashl).
          end.
        end.
      end.

      ELSE do:
        v-sman = cust.sman.

        find first tt-report
            where tt-report.key-01  eq v-sman
              and tt-report.key-02  eq string(ar-cashl.inv-no,"9999999999")
              and tt-report.rec-id  eq recid(ar-cashl)
            no-lock no-error.
        if not avail tt-report then do:
          create tt-report.

          assign
           tt-report.key-01  = v-sman
           tt-report.key-02  = string(ar-cashl.inv-no,"9999999999")
           tt-report.key-04  = STRING(ar-cash.check-date,"99/99/9999")
           tt-report.rec-id  = recid(ar-cashl).
        end.
      end.
    end.
  end.

  for each tt-report
      /*transaction*/ :

    release ar-inv.
    release ar-cash.

    find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock no-error.    

    if avail ar-cashl then do:
      find first ar-cash where ar-cash.c-no eq ar-cashl.c-no no-lock.

      assign
       v-dsc[1] = ar-cashl.amt-disc
       v-amt[1] = ar-cashl.amt-paid + v-dsc[1]
       v-amt[2] = v-amt[1].

      if ar-cashl.inv-no ne 0 then
      for each ar-invl FIELDS(x-no inv-no amt sman s-pct)
          where ar-invl.company eq fi_company
            and ar-invl.cust-no eq ar-cash.cust-no
            and ar-invl.inv-no  eq ar-cashl.inv-no
          no-lock,

          first ar-inv FIELDS(tax-amt f-bill freight) where
                ar-inv.x-no eq ar-invl.x-no no-lock

          break by ar-invl.inv-no:

        if first(ar-invl.inv-no) then
          assign
           v-amt    = 0
           v-amt[1] = ar-inv.tax-amt +
                      (if ar-inv.f-bill then ar-inv.freight else 0).

        v-amt[1] = v-amt[1] + ar-invl.amt.

        if ar-invl.sman[1] ne "" then
        do i = 1 to 3:
          if tt-report.key-01 eq ar-invl.sman[i] then do:
            ASSIGN
             v-amt[2] = v-amt[2] + (ar-invl.amt * ar-invl.s-pct[i] / 100).
            leave.
          end.
        end.
        else
          assign
           v-amt[2] = v-amt[2] + ar-invl.amt.
      end.

      assign
       v-pct    = v-amt[2] / v-amt[1]
       v-amt[1] = (ar-cashl.amt-paid + v-dsc[1]) * v-pct
       v-pct    = v-amt[1] / v-amt[2].

      release ar-inv.
    end.

    else do:
      find ar-invl where recid(ar-invl) eq tt-report.rec-id no-lock.

      assign
       v-amt[1] = ar-invl.amt.
    end.

    if v-amt[1] eq ? then v-amt[1] = 0.

    IF DATE(tt-report.key-04) EQ fi_as-of-date THEN
       tt-ar-ap.date-ar-rec-amt = tt-ar-ap.date-ar-rec-amt + v-amt[1].

    tt-ar-ap.mtd-ar-rec-amt = tt-ar-ap.mtd-ar-rec-amt + v-amt[1].

  END.

  RUN ap-proc.

  RELEASE tt-ar-ap.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ap-proc C-Win 
PROCEDURE ap-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*from VR13*/
  DEF VAR v-gross-amt AS DEC NO-UNDO.
  DEF VAR v-start-of-month AS DATE NO-UNDO.

  v-start-of-month = DATE(MONTH(fi_as-of-date),1,YEAR(fi_as-of-date)).

  EMPTY TEMP-TABLE tt-ap-report.

  FOR EACH vend FIELDS(vend-no) WHERE
      vend.company EQ fi_company
      NO-LOCK,
      EACH ap-pay FIELDS(c-no check-no vend-no check-act check-date check-amt)
    WHERE ap-pay.company    EQ fi_company
      AND ap-pay.vend-no    EQ vend.vend-no
      AND ap-pay.check-date GE v-start-of-month
      AND ap-pay.check-date LE fi_as-of-date
      AND ap-pay.posted     EQ YES
      AND ap-pay.memo       EQ NO
    NO-LOCK USE-INDEX vend-no,
    EACH ap-payl FIELDS(amt-paid amt-disc inv-no amt-paid amt-disc LINE) 
         WHERE ap-payl.c-no EQ ap-pay.c-no NO-LOCK

    BREAK BY ap-pay.check-act
          BY ap-pay.check-no
          BY ap-payl.inv-no
          BY ap-payl.line
          BY ap-payl.amt-paid:

    v-gross-amt = v-gross-amt + (ap-payl.amt-paid + ap-payl.amt-disc).

    IF FIRST-OF(ap-payl.inv-no) THEN DO:
      CREATE tt-ap-report.

      ASSIGN
       tt-ap-report.key-03     = ap-payl.inv-no
       tt-ap-report.check-date = ap-pay.check-date
       tt-ap-report.inv-no     = ap-payl.inv-no
       tt-ap-report.amt-paid   = ap-payl.amt-paid.
    END.

    IF LAST-OF(ap-pay.check-no) THEN DO:
      IF NOT FIRST-OF(ap-pay.check-no) OR v-gross-amt EQ 0 THEN DO:
        CREATE tt-ap-report.

        ASSIGN
         tt-ap-report.key-03     = FILL("z",100) + "TOTAL"
         tt-ap-report.check-date = ap-pay.check-date
         tt-ap-report.inv-no     = IF v-gross-amt EQ 0 THEN "Void" ELSE ""
         tt-ap-report.amt-paid   = ap-pay.check-amt.

        IF tt-ap-report.inv-no EQ "Void" THEN
           tt-ap-report.amt-paid  = tt-ap-report.amt-paid * -1.
      END.

      ASSIGN
       v-gross-amt = 0. 
    END.
  END.

  FOR EACH tt-ap-report:

     IF tt-ap-report.key-03 NE FILL("z",100) + "TOTAL" OR
        tt-ap-report.inv-no EQ "Void" THEN
        DO:
           tt-ar-ap.mtd-ap-paid-amt = tt-ar-ap.mtd-ap-paid-amt + tt-ap-report.amt-paid.

           IF tt-ap-report.check-date EQ fi_as-of-date THEN
              tt-ar-ap.date-ap-paid-amt = tt-ar-ap.date-ap-paid-amt + tt-ap-report.amt-paid. 
        END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE backlog-sales-forecast-proc C-Win 
PROCEDURE backlog-sales-forecast-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*OR 13 Range this month, Include jobs, Exclude set components = no*/
  DEFINE INPUT PARAMETER ip-start-month AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER ip-start-next-month AS DATE NO-UNDO.

  DEF VAR li-qty AS INT EXTENT 2 NO-UNDO.
  def var v-qty as int extent 2.

  ip-start-month = DATE(MONTH(fi_as-of-date),1,YEAR(fi_as-of-date)).

  IF MONTH(fi_as-of-date) NE 12 THEN
     ip-start-next-month = DATE(MONTH(fi_as-of-date) + 1,1,YEAR(fi_as-of-date)).
  ELSE
     ip-start-next-month = DATE(1,1,YEAR(fi_as-of-date) + 1).

  FOR EACH tt-sales-forecast:

    EMPTY TEMP-TABLE tt-report.
    EMPTY TEMP-TABLE w-ord2.

    FOR each xoe-ord FIELDS(cust-no ord-date TYPE company opened ord-no
        last-date) WHERE
        xoe-ord.company  eq tt-sales-forecast.company AND
        xoe-ord.opened   eq YES
        use-index opened no-lock,
        each oe-ordl FIELDS(ship-qty t-ship-qty qty part-no i-no
                            job-no job-no2) WHERE
             oe-ordl.company eq xoe-ord.company AND
             oe-ordl.ord-no  eq xoe-ord.ord-no AND
             oe-ordl.stat    NE "C" AND
             oe-ordl.req-date ge ip-start-month AND
             oe-ordl.req-date LT ip-start-next-month
             no-lock:

        IF oe-ordl.ship-qty EQ oe-ordl.t-ship-qty THEN
           li-qty[2] = oe-ordl.ship-qty.
        ELSE
           RUN oe/ordlsqty.p (ROWID(oe-ordl), OUTPUT li-qty[1], OUTPUT li-qty[2]).

        IF li-qty[2] LT oe-ordl.qty THEN DO:  
           create tt-report.
           assign
            tt-report.key-03  = oe-ordl.i-no
            tt-report.key-04  = string(xoe-ord.ord-no,"9999999999")
            tt-report.rec-id  = recid(oe-ordl).
        END.
    END. /*for each xoe-ord */

    for each job-hdr FIELDS(job job-no job-no2 i-no qty cust-no)
      where job-hdr.company eq tt-sales-forecast.company
        and job-hdr.ord-no  eq 0
      no-lock,
      first job FIELDS(job-no job-no2)
      where job.company    eq tt-sales-forecast.company
        and job.job        eq job-hdr.job
        and job.job-no     eq job-hdr.job-no
        and job.job-no2    eq job-hdr.job-no2
        and job.start-date ge ip-start-month
        and job.start-date LT ip-start-next-month
        and job.opened     EQ YES
      no-lock,
      first itemfg FIELDS(part-no)
      where itemfg.company eq tt-sales-forecast.company
        and itemfg.i-no    eq job-hdr.i-no
      no-lock  

      break by job-hdr.job
            by job-hdr.job-no
            by job-hdr.job-no2
            by job-hdr.i-no:

      v-qty[1] = v-qty[1] + job-hdr.qty.

      if last-of(job-hdr.i-no) then do:
        for each fg-act FIELDS(qty)
            where fg-act.company eq tt-sales-forecast.company
              and fg-act.job     eq job-hdr.job
              and fg-act.job-no  eq job-hdr.job-no
              and fg-act.job-no2 eq job-hdr.job-no2
              and fg-act.i-no    eq job-hdr.i-no
            use-index job-idx no-lock:

          v-qty[2] = v-qty[2] + fg-act.qty.  
        end.

        if v-qty[2] lt v-qty[1] then do:
          create tt-report.
          assign
           tt-report.key-03  = job-hdr.i-no
           tt-report.key-04  = string(v-qty[1] - v-qty[2],"9999999999")
           tt-report.key-05  = job-hdr.cust-no
           tt-report.rec-id  = recid(job).
        end.

        v-qty = 0.
      end. /*last-of (job-hdr.i-no) */
    end. /* each job-hdr */

    for each tt-report:

        {oe/rep/backlog3.i}

         FIND FIRST itemfg WHERE
              itemfg.company = tt-sales-forecast.company AND
              itemfg.i-no   = tt-report.key-03
              NO-LOCK NO-ERROR.

         ASSIGN
           tt-sales-forecast.backlog-amt = tt-sales-forecast.backlog-amt + w-ord2.t-price
           tt-sales-forecast.backlog-qty = tt-sales-forecast.backlog-qty + w-ord2.qty-due
           tt-sales-forecast.backlog-cost = tt-sales-forecast.backlog-cost + w-ord2.cost.

         IF AVAIL itemfg THEN
            tt-sales-forecast.backlog-msf = tt-sales-forecast.backlog-msf + (w-ord2.qty-due * itemfg.t-sqft / 1000).

    END. /* each tt-report */

  END. /* for each tt-sales-forecast*/

  FOR EACH tt-sales-forecast:
      tt-sales-forecast.backlog-profit = ROUND(tt-sales-forecast.backlog-amt -
                                               tt-sales-forecast.backlog-cost,2).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dso-proc C-Win 
PROCEDURE dso-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-total-period-bal AS DEC DECIMALS 2 NO-UNDO.
   DEF VAR v-ninety-days-date AS DATE NO-UNDO.
   DEF VAR v-raw-sales AS DEC DECIMALS 2 NO-UNDO.    
   DEF VAR cAccountList AS CHAR NO-UNDO.
   DEFINE VARIABLE dOpenBal AS DECIMAL     NO-UNDO.

   FIND FIRST period WHERE
        period.company EQ fi_company AND
        period.pst LE fi_as-of-date AND
        period.pend GE fi_as-of-date
        NO-LOCK NO-ERROR.

   v-dso = 0.

   IF AVAIL period THEN
   DO:
      FOR EACH tt-ar-dso,
          FIRST account FIELDS(cyr company actnum) WHERE
                account.company EQ fi_company AND
                account.actnum EQ tt-ar-dso.actnum
                NO-LOCK:
          cAccountList = cAccountList + tt-ar-dso.actnum + ",".
          dOpenBal = 0.
          RUN gl/gl-opend.p (ROWID(account), period.pst, OUTPUT dOpenBal).
/*           v-total-period-bal = v-total-period-bal + account.cyr[period.pnum]. */
          v-total-period-bal = v-total-period-bal + dOpenBal.          
/*           IF period.pstat eq YES THEN */
         FOR each gltrans FIELDS(tr-amt) WHERE
             gltrans.company eq account.company AND
             gltrans.actnum  eq account.actnum AND
             gltrans.period  eq period.pnum AND
             gltrans.tr-date ge period.pst AND
             gltrans.tr-date le period.pend
             no-lock:

             v-total-period-bal = v-total-period-bal + gltrans.tr-amt.
         END.
      END.

      v-ninety-days-date = fi_as-of-date - 89.

      FOR EACH tt-raw-sales 
          WHERE tt-raw-sales.DATE GE v-ninety-days-date
            AND tt-raw-sales.DATE LE fi_as-of-date :

          v-raw-sales = v-raw-sales + ROUND(tt-raw-sales.date-amt,2).
      END.

      ASSIGN
         v-raw-sales = v-raw-sales / 90.0
         v-dso = IF v-raw-sales NE 0 THEN ROUND(v-total-period-bal / v-raw-sales,1)
                 ELSE 0.
   END.

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
  DISPLAY fi_as-of-date fi_company tg_round tg_set-comp 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-9 fi_as-of-date fi_company browse-machine tg_round tg_set-comp 
         browse-sales-forecast browse-ar btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE invoiced-sales-forecast-proc C-Win 
PROCEDURE invoiced-sales-forecast-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*OR6 Order Cost*/
  DEFINE INPUT PARAMETER ip-start-month AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER ip-start-next-month AS DATE NO-UNDO.

  DEF VAR ld-date AS DATE NO-UNDO.
  DEF VAR ld-inv-pct AS DEC NO-UNDO.
  DEF VAR ll-comp AS LOG NO-UNDO.
  DEF VAR v-slsm LIKE ar-invl.sman EXTENT 1 NO-UNDO.
  DEF var v-slsp like ar-invl.s-pct extent 1 NO-UNDO.
  DEF VAR v-qty AS DEC NO-UNDO.
  DEF VAR v-amt AS DEC DECIMALS 2 NO-UNDO.
  DEF VAR ld-csh-pct AS DEC NO-UNDO.
  DEF VAR v-cost AS DEC DECIMALS 2 NO-UNDO.
  DEF VAR v-i-no AS CHAR NO-UNDO.
  DEF VAR v-ord-no AS INT NO-UNDO.
  DEF VAR v-msf AS DEC NO-UNDO.

  ip-start-month = DATE(MONTH(fi_as-of-date),1,YEAR(fi_as-of-date)).

  IF MONTH(fi_as-of-date) NE 12 THEN
     ip-start-next-month = DATE(MONTH(fi_as-of-date) + 1,1,YEAR(fi_as-of-date)).
  ELSE
     ip-start-next-month = DATE(1,1,YEAR(fi_as-of-date) + 1).

  FOR EACH tt-sales-forecast:

      EMPTY TEMP-TABLE tt-report.

      FOR EACH cust FIELDS(cust-no sman) where
          cust.company eq tt-sales-forecast.company
          NO-LOCK:

         FOR EACH ar-inv FIELDS(x-no inv-no inv-date) WHERE
             ar-inv.company  EQ tt-sales-forecast.company AND
             ar-inv.posted   EQ YES AND
             ar-inv.cust-no  eq cust.cust-no AND
             ar-inv.inv-date GE ip-start-month AND
             ar-inv.inv-date LT ip-start-next-month
             NO-LOCK,
             each ar-invl FIELDS(sman s-pct billable misc) WHERE
                  ar-invl.x-no eq ar-inv.x-no AND
                 (ar-invl.billable or not ar-invl.misc)
             no-lock:

             RUN oe/invlcomp.p (ROWID(ar-invl), OUTPUT ll-comp).
             IF ll-comp THEN NEXT.

             do i = 1 to 3:
               v-slsm[1] = if ar-invl.sman[i] eq "" and i eq 1 then
                             cust.sman else ar-invl.sman[i].

               if (i ne 1 and
                   (v-slsm[1] eq "" or ar-invl.s-pct[i] eq 0)) then next.

               create tt-report.
               assign
                tt-report.key-01  = v-slsm[1]
                tt-report.key-02  = cust.cust-no
                tt-report.key-10  = "ar-invl"
                tt-report.rec-id  = recid(ar-invl)
                tt-report.row-id  = ROWID(ar-invl).
               IF i GT 1 THEN DO:
                   tt-report.is-duplicate = YES.
               END.

             end.
         END. /*each ar-inv*/

         FOR EACH ar-cash FIELDS(c-no)
             WHERE ar-cash.company    EQ tt-sales-forecast.company
               AND ar-cash.cust-no    EQ cust.cust-no
               AND ar-cash.check-date GE ip-start-month
               AND ar-cash.check-date LT ip-start-next-month
               AND ar-cash.posted     EQ YES
             NO-LOCK,
             EACH ar-cashl FIELDS(company actnum inv-no dscr)
             WHERE ar-cashl.c-no    EQ ar-cash.c-no
               AND ar-cashl.posted  EQ YES
               AND ar-cashl.memo    EQ YES
               AND CAN-FIND(FIRST account
                            WHERE account.company EQ ar-cashl.company
                              AND account.actnum  EQ ar-cashl.actnum
                              AND account.type    EQ "R")
             NO-LOCK:

           RELEASE tt-report.
           RELEASE ar-invl.

           RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

           if avail oe-retl then 
           find first ar-invl
               where ar-invl.company eq ar-cashl.company
                 and ar-invl.cust-no eq cust.cust-no
                 and ar-invl.inv-no  eq ar-cashl.inv-no
                 and ar-invl.i-no    eq oe-retl.i-no
                 and (ar-invl.billable or not ar-invl.misc)
               no-lock no-error.

           IF ar-cashl.inv-no NE 0                                                       AND
              (AVAIL ar-invl                             OR
               (NOT AVAIL reftable AND
                NOT ar-cashl.dscr MATCHES "*oe return*") OR
               SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,5) EQ "items") THEN
           FOR EACH b-ar-invl FIELDS(billable misc sman s-pct inv-no)
               WHERE b-ar-invl.company EQ ar-cashl.company
                 AND b-ar-invl.cust-no EQ cust.cust-no
                 AND b-ar-invl.inv-no  EQ ar-cashl.inv-no
               NO-LOCK:

             IF NOT ((b-ar-invl.billable OR NOT b-ar-invl.misc)
                AND (NOT AVAIL ar-invl OR ROWID(b-ar-invl) EQ ROWID(ar-invl))) THEN
                NEXT.

             IF AVAIL ar-invl THEN DO:
               RUN oe/invlcomp.p (ROWID(b-ar-invl), OUTPUT ll-comp).
               IF ll-comp THEN NEXT.
             END.

             DO i = 1 TO 3:
               v-slsm[1] = IF b-ar-invl.sman[i] EQ "" AND i EQ 1 THEN
                             cust.sman ELSE b-ar-invl.sman[i].

               IF (i NE 1 AND
                  (v-slsm[1] EQ "" OR b-ar-invl.s-pct[i] EQ 0)) THEN NEXT.

               CREATE tt-report.
               ASSIGN
                tt-report.key-01 = v-slsm[1]
                tt-report.row-id = ROWID(b-ar-invl)
                tt-report.key-02 = cust.cust-no
                tt-report.key-10 = "ar-cashl"
                tt-report.rec-id = RECID(ar-cashl).
               IF i GT 1 THEN  do:
                   tt-report.is-duplicate = YES.
               END.

             END.
           END.

           ELSE DO:

             create tt-report.
             assign
                tt-report.key-01 = cust.sman.

             if avail tt-report then
               assign
                tt-report.key-02  = cust.cust-no
                tt-report.key-10  = "ar-cashl"
                tt-report.rec-id  = recid(ar-cashl).
           END.
         end. /*each ar-cash*/
      END. /*each cust*/ 

      input-work:
      for each tt-report WHERE ,
          first cust FIELDS(cust-no) WHERE
                cust.company eq tt-sales-forecast.company AND
                cust.cust-no eq tt-report.key-02
                no-lock:

          release ar-invl.
          release ar-cashl.

          ASSIGN
            v-cost = 0
            v-amt = 0
            v-qty = 0
            v-msf = 0
            v-i-no = ""
            v-ord-no = 0.

          if tt-report.key-10 eq "ar-invl" then
             find ar-invl where recid(ar-invl) eq tt-report.rec-id no-lock no-error.

          if avail ar-invl then do:

             RELEASE itemfg.

             FIND FIRST itemfg WHERE
                  itemfg.company EQ tt-sales-forecast.company AND
                  itemfg.i-no    EQ ar-invl.i-no
                  NO-LOCK NO-ERROR.

             do i = 1 to 3:
               if ar-invl.sman[i] eq tt-report.key-01 or
                  ar-invl.sman[1] eq "" then leave.
               if i eq 3 then next input-work.
             end.

             ASSIGN
             v-slsp[1] = if ar-invl.sman[i] eq ""              or
                            (ar-invl.s-pct[i] eq 0 and i eq 1) then 100
                         else ar-invl.s-pct[i]
             v-i-no    = ar-invl.i-no
             v-ord-no  = ar-invl.ord-no
             v-qty = (if ar-invl.inv-qty ne 0 then ar-invl.inv-qty
                      else ar-invl.qty) * v-slsp[1] / 100
             v-msf = if ar-invl.amt-msf ne 0 then ar-invl.amt-msf
                     else
                       if avail itemfg then
                          (itemfg.t-sqft * ar-invl.qty / 1000) else 0
             v-amt = ar-invl.amt * v-slsp[1] / 100
             v-cost = ar-invl.t-cost * v-slsp[1] / 100.

          end. /*if avail ar-invl*/
         else
         if tt-report.key-10 eq "ar-cashl" then
            find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock no-error.

         if avail ar-cashl then do:
           RELEASE oe-retl.
           RELEASE ar-invl.

           FIND ar-invl WHERE ROWID(ar-invl) EQ tt-report.row-id NO-LOCK NO-ERROR.

           RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

           if avail oe-retl and not avail ar-invl then 
           find first ar-invl
               where ar-invl.company eq tt-sales-forecast.company
                 and ar-invl.cust-no eq cust.cust-no
                 and ar-invl.inv-no  eq ar-cashl.inv-no
                 and ar-invl.i-no    eq oe-retl.i-no
               no-lock no-error.

           IF AVAIL ar-invl THEN DO:
             DO i = 1 to 3:
               IF ar-invl.sman[i] EQ tt-report.key-01 OR
                  ar-invl.sman[1] EQ ""               THEN LEAVE.
               IF i EQ 3 THEN NEXT input-work.
             END.

             RELEASE itemfg.

             FIND FIRST itemfg NO-LOCK
                 WHERE itemfg.company EQ tt-sales-forecast.company
                   AND itemfg.i-no    EQ ar-invl.i-no
                 NO-ERROR.

             ASSIGN
              v-slsp[1]   = if ar-invl.sman[i] eq ""              or
                             (ar-invl.s-pct[i] eq 0 and i eq 1) then 100
                            else ar-invl.s-pct[i]
              v-amt = (ar-cashl.amt-paid - ar-cashl.amt-disc) *
                      (v-slsp[1] / 100)
              v-ord-no    = ar-invl.ord-no
              v-i-no      = ar-invl.i-no.

             IF AVAIL oe-retl THEN
               ASSIGN
                v-qty = oe-retl.tot-qty-return * -1
                v-msf = if avail itemfg then
                          oe-retl.tot-qty-return * itemfg.t-sqft / 1000
                        else 0
                v-cost = (oe-retl.cost * (oe-retl.tot-qty-return / 1000) *
                                         (v-slsp[1] / 100) * -1).

             ELSE DO:
               ld-inv-pct = 0.
               FOR EACH b-ar-invl fields(amt) WHERE b-ar-invl.x-no EQ ar-invl.x-no NO-LOCK:
                 ld-inv-pct = ld-inv-pct + b-ar-invl.amt.
                 ACCUMULATE 1 (TOTAL).
               END.
               ld-inv-pct = IF ld-inv-pct EQ 0 THEN
                               (1 / IF (ACCUM TOTAL 1) EQ 0 THEN 1
                                                            ELSE (ACCUM TOTAL 1))
                            ELSE (ar-invl.amt / ld-inv-pct).

               IF ld-inv-pct EQ ? THEN ld-inv-pct = 0.

               ld-csh-pct = 0.
               FOR EACH b-ar-cashl FIELDS(amt-paid amt-disc)
                   WHERE b-ar-cashl.c-no   EQ ar-cashl.c-no
                     AND b-ar-cashl.inv-no EQ ar-cashl.inv-no
                   NO-LOCK:
                 ld-csh-pct = ld-csh-pct + (b-ar-cashl.amt-paid - b-ar-cashl.amt-disc).
               END.
               ld-csh-pct = (ar-cashl.amt-paid - ar-cashl.amt-disc) / ld-csh-pct.

               IF ld-csh-pct EQ ? THEN ld-csh-pct = 0.

               ASSIGN
                v-amt  = v-amt * ld-inv-pct
                v-cost = ar-invl.t-cost * (v-slsp[1] / 100) * -1 * ld-csh-pct.
             END.
           END.
           ELSE
              ASSIGN
                 v-qty = 0
                 v-msf = 0
                 v-amt = ar-cashl.amt-paid - ar-cashl.amt-disc
                 v-cost = 0.

         END. /*avail ar-cashl*/

      IF v-i-no NE "" AND v-ord-no NE 0 THEN
      DO:
          FIND FIRST oe-ordl WHERE
               oe-ordl.company eq tt-sales-forecast.company AND
               oe-ordl.ord-no  eq v-ord-no AND
               oe-ordl.i-no    eq v-i-no
               NO-LOCK NO-ERROR.

          IF AVAIL oe-ordl THEN
             v-cost = oe-ordl.cost * v-qty / 1000.
      END.

      if v-cost eq ? then v-cost = 0.

      {sys/inc/roundup.i v-qty}

      ASSIGN
         tt-sales-forecast.invoiced-amt = tt-sales-forecast.invoiced-amt + v-amt
         tt-sales-forecast.invoiced-qty = tt-sales-forecast.invoiced-qty + v-qty
         tt-sales-forecast.invoiced-msf = tt-sales-forecast.invoiced-msf + v-msf
         tt-sales-forecast.invoiced-cost = tt-sales-forecast.invoiced-cost + v-cost.

      END. /*each tt-report */
  END. /* each tt-sales-forecast */

  FOR EACH tt-sales-forecast:
      tt-sales-forecast.invoiced-profit = ROUND(tt-sales-forecast.invoiced-amt -
                                                tt-sales-forecast.invoiced-cost,2).

  END.
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
       no-lock
       USE-INDEX ordate,
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
         RELEASE w-ord.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE raw-prod-cat-proc C-Win 
PROCEDURE raw-prod-cat-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var v-amt       LIKE ar-inv.gross  format "->,>>>,>>9.99"           no-undo.    
  def var v-cost      LIKE itemfg.t-sqft  format "->,>>9.999"             no-undo.
  def var v-sqft      like ar-inv.t-cost format "->,>>>,>>9.99"           no-undo.
  DEF VAR v-msf       as   dec NO-UNDO.
  DEF VAR v-start-of-year AS DATE NO-UNDO.

  /*similar to HF report*/

  EMPTY TEMP-TABLE tt-report.
  EMPTY TEMP-TABLE w-data.

  v-start-of-year = DATE(1,1,YEAR(fi_as-of-date)).

  for each cust FIELDS(company cust-no) where cust.company eq fi_company no-lock:
    for each ar-inv FIELDS(company posted cust-no inv-date x-no)
      where ar-inv.company  eq fi_company
        and ar-inv.posted   eq YES
        AND ar-inv.cust-no  EQ cust.cust-no
        and ar-inv.inv-date ge v-start-of-year
        and ar-inv.inv-date le fi_as-of-date
        and ar-inv.type    ne "FC"
        no-lock,
      each ar-invl FIELDS(x-no billable misc i-no actnum)
      where ar-invl.x-no eq ar-inv.x-no
        and (ar-invl.billable or not ar-invl.misc)
      no-lock:

      create tt-report.

        assign
         tt-report.rec-id  = recid(ar-invl)
         tt-report.key-01  = "MISC".

        if not ar-invl.misc then do:

          RELEASE itemfg.

          IF ar-invl.i-no NE "" THEN
          find first itemfg
              where itemfg.company eq fi_company
                and itemfg.i-no    eq ar-invl.i-no
              no-lock no-error.

          if avail itemfg then tt-report.key-01 = itemfg.procat.

          else do:
            find first fgcat
                where fgcat.company eq fi_company
                  and fgcat.glacc   eq ar-invl.actnum
                no-lock no-error.
            if avail fgcat then tt-report.key-01 = fgcat.procat.
          end.
        end.

        tt-report.key-02 = /*if v-misc and tt-report.key-01 eq "MISC"
                           then ar-invl.actnum else*/ tt-report.key-01.
    end. /*each ar-inv*/

    for each ar-cash FIELDS(company cust-no check-date posted c-no)
      where ar-cash.company    eq fi_company
        and ar-cash.cust-no    eq cust.cust-no
        and ar-cash.check-date ge v-start-of-year
        and ar-cash.check-date le fi_as-of-date
        and ar-cash.posted     eq yes
      no-lock,

      EACH ar-cashl FIELDS(c-no posted memo company actnum dscr)
      WHERE ar-cashl.c-no    EQ ar-cash.c-no
        AND ar-cashl.posted  EQ YES
        AND ar-cashl.memo    EQ YES
        AND CAN-FIND(FIRST account
                     WHERE account.company EQ ar-cashl.company
                       AND account.actnum  EQ ar-cashl.actnum
                       AND account.type    EQ "R")
      NO-LOCK:

    create tt-report.

        assign
         tt-report.key-01  = "MEMO"
         tt-report.key-02  = ar-cashl.actnum
         tt-report.rec-id  = recid(ar-cashl).

        RELEASE itemfg.

    RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

    IF AVAIL reftable                      OR
       ar-cashl.dscr MATCHES "*OE RETURN*" THEN DO:

       RELEASE itemfg.

       if avail oe-retl AND oe-retl.i-no NE "" then
       find first itemfg
           where itemfg.company eq fi_company
             and itemfg.i-no    eq oe-retl.i-no
           no-lock no-error.

       tt-report.key-01 = if avail itemfg then itemfg.procat else "MISC".
    end.

    tt-report.key-02 = /*if v-misc and tt-report.key-01 eq "MISC"
                       then ar-cashl.actnum else*/ tt-report.key-01.
    END.

  end. /*each cust*/

  FOR EACH tt-report,
      FIRST tt-sales-prod-cat WHERE
            tt-sales-prod-cat.prod-cat EQ tt-report.key-02
      break by tt-report.key-01
            by tt-report.key-02:

      find first w-data no-error.

      if first-of(tt-report.key-02) THEN CREATE w-data.

      find ar-invl where recid(ar-invl) eq tt-report.rec-id no-lock no-error.

      if avail ar-invl then do:
         find ar-inv where ar-inv.x-no eq ar-invl.x-no no-lock.

         RELEASE itemfg.

         IF ar-invl.i-no NE "" THEN
         find first itemfg
              where itemfg.company eq fi_company
              and itemfg.i-no    eq ar-invl.i-no
              no-lock no-error.

         assign
           v-amt  = ar-invl.amt
           v-cost = ar-invl.t-cost
           v-msf = if ar-invl.amt-msf ne 0 then ar-invl.amt-msf
                    else
                    if avail itemfg then
                      (itemfg.t-sqft * ar-invl.ship-qty / 1000) else 0
           v-sqft  = v-msf * 1000.

         if v-amt  eq ? then v-amt  = 0.
         if v-cost eq ? then v-cost = 0.
         if v-sqft eq ? then v-sqft = 0.
         if v-msf eq ? then v-msf = 0.

         IF ar-inv.inv-date EQ tt-sales-prod-cat.DATE THEN
            assign
              w-data.w-sqft[1] = w-data.w-sqft[1] + v-sqft
              w-data.w-amt[1]  = w-data.w-amt[1]  + v-amt  
              w-data.w-cost[1] = w-data.w-cost[1] + v-cost 
              w-data.w-msf[1]  = w-data.w-msf[1]  + v-msf.

         IF ar-inv.inv-date LE fi_as-of-date THEN
         DO:
            IF MONTH(ar-inv.inv-date) EQ MONTH(fi_as-of-date) THEN
               assign
                 w-data.w-sqft[2] = w-data.w-sqft[2] + v-sqft
                 w-data.w-amt[2]  = w-data.w-amt[2]  + v-amt 
                 w-data.w-cost[2] = w-data.w-cost[2] + v-cost
                 w-data.w-msf[2]  = w-data.w-msf[2]  + v-msf.

            assign
              w-data.w-sqft[3] = w-data.w-sqft[3] + v-sqft
              w-data.w-amt[3]  = w-data.w-amt[3]  + v-amt 
              w-data.w-cost[3] = w-data.w-cost[3] + v-cost
              w-data.w-msf[3]  = w-data.w-msf[3]  + v-msf.
         END.
      END.
      ELSE DO:
         find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock no-error.

         if avail ar-cashl then do:
           find ar-cash where ar-cash.c-no eq ar-cashl.c-no no-lock.

           assign
            v-amt  = ar-cashl.amt-paid - ar-cashl.amt-disc
            v-msf  = 0
            v-cost = 0
            v-sqft = 0.

           RELEASE itemfg.
           RELEASE ar-invl.

           RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

           IF AVAIL oe-retl THEN DO:

             IF oe-retl.i-no NE "" THEN
             find first itemfg
                 where itemfg.company eq fi_company
                   and itemfg.i-no    eq oe-retl.i-no
                 no-lock no-error.

             ASSIGN
               v-msf = if avail itemfg then
                          oe-retl.tot-qty-return * itemfg.t-sqft / 1000
                        else 0
               v-sqft  = v-msf * 1000.

             if v-sqft eq ? then v-sqft = 0.
             if v-msf eq ? then v-msf = 0.

             find first ar-invl
                 where ar-invl.company eq fi_company
                   and ar-invl.cust-no eq ar-cash.cust-no
                   and ar-invl.inv-no  eq ar-cashl.inv-no
                   and ar-invl.i-no    eq oe-retl.i-no
                 no-lock no-error.

             if avail ar-invl THEN
               RUN salrep/salecost.p (3,
                                      ROWID(ar-invl),
                                      oe-retl.job-no,
                                      oe-retl.job-no,
                                      oe-retl.tot-qty-return,
                                      OUTPUT v-cost).
           end.

           IF ar-cash.check-date EQ tt-sales-prod-cat.DATE THEN
            assign
              w-data.w-sqft[1] = w-data.w-sqft[1] - v-sqft
              w-data.w-amt[1]  = w-data.w-amt[1]  + v-amt 
              w-data.w-cost[1] = w-data.w-cost[1] - v-cost
              w-data.w-msf[1]  = w-data.w-msf[1]  - v-msf.

           IF ar-cash.check-date LE fi_as-of-date THEN
           DO:
              IF MONTH(ar-cash.check-date) EQ MONTH(fi_as-of-date) THEN
               ASSIGN
                 w-data.w-sqft[2] = w-data.w-sqft[2] - v-sqft
                 w-data.w-amt[2]  = w-data.w-amt[2]  + v-amt 
                 w-data.w-cost[2] = w-data.w-cost[2] - v-cost
                 w-data.w-msf[2]  = w-data.w-msf[2]  - v-msf.

              assign
                 w-data.w-sqft[3] = w-data.w-sqft[3] - v-sqft 
                 w-data.w-amt[3]  = w-data.w-amt[3]  + v-amt
                 w-data.w-cost[3] = w-data.w-cost[3] - v-cost
                 w-data.w-msf[3]  = w-data.w-msf[3]  - v-msf.
           END.
         END.
      END.

      IF LAST-OF(tt-report.key-02) THEN
      DO:
         ASSIGN
            tt-sales-prod-cat.date-sf = tt-sales-prod-cat.date-sf + w-data.w-sqft[1]
            tt-sales-prod-cat.date-amt = tt-sales-prod-cat.date-amt + w-data.w-amt[1]
            tt-sales-prod-cat.date-msf = tt-sales-prod-cat.date-msf + w-data.w-msf[1]
            tt-sales-prod-cat.date-cost = tt-sales-prod-cat.date-cost + w-data.w-cost[1]
            tt-sales-prod-cat.mtd-sf = tt-sales-prod-cat.mtd-sf + w-data.w-sqft[2] 
            tt-sales-prod-cat.mtd-amt = tt-sales-prod-cat.mtd-amt + w-data.w-amt[2]
            tt-sales-prod-cat.mtd-msf = tt-sales-prod-cat.mtd-msf + w-data.w-msf[2]
            tt-sales-prod-cat.mtd-cost = tt-sales-prod-cat.mtd-cost + w-data.w-cost[2]
            tt-sales-prod-cat.ytd-sf = tt-sales-prod-cat.ytd-sf + w-data.w-sqft[3]
            tt-sales-prod-cat.ytd-amt = tt-sales-prod-cat.ytd-amt + w-data.w-amt[3]
            tt-sales-prod-cat.ytd-msf = tt-sales-prod-cat.ytd-msf +  w-data.w-msf[3]
            tt-sales-prod-cat.ytd-cost = tt-sales-prod-cat.ytd-cost + w-data.w-cost[3].

         DELETE w-data.
      END.
  END.

  FOR EACH tt-sales-prod-cat:
      ASSIGN
        tt-sales-prod-cat.date-profit = IF tt-sales-prod-cat.date-amt NE 0 THEN
                                           (tt-sales-prod-cat.date-amt - tt-sales-prod-cat.date-cost) /
                                           tt-sales-prod-cat.date-amt * 100
                                        ELSE 0
        tt-sales-prod-cat.mtd-profit = IF tt-sales-prod-cat.mtd-amt NE 0 THEN
                                          (tt-sales-prod-cat.mtd-amt - tt-sales-prod-cat.mtd-cost) /
                                          tt-sales-prod-cat.mtd-amt * 100
                                       ELSE 0
        tt-sales-prod-cat.ytd-profit = IF tt-sales-prod-cat.ytd-amt NE 0 THEN
                                          (tt-sales-prod-cat.ytd-amt - tt-sales-prod-cat.ytd-cost) /
                                          tt-sales-prod-cat.ytd-amt * 100
                                       ELSE 0.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE raw-prod-proc C-Win 
PROCEDURE raw-prod-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*DR1*/
   def var v-on as int no-undo.
   def var v-out as int no-undo.
   DEF VAR v-up AS INT NO-UNDO.
   DEF VAR std-hrs-var AS DEC NO-UNDO.
   DEF VAR tot-hrs-var AS DEC NO-UNDO.
   DEF VAR v-start-of-year AS DATE NO-UNDO.
   DEF VAR v-month-as-of-date AS INT NO-UNDO.

   EMPTY TEMP-TABLE tt-report.

   ASSIGN
      v-start-of-year = DATE(1,1,YEAR(fi_as-of-date))
      v-month-as-of-date = MONTH(fi_as-of-date).

   FOR EACH tt-raw-prod,
       EACH mch-act FIELDS(company m-code op-date job frm blank-no dept pass
            CODE qty op-date hours) WHERE
            mch-act.company EQ fi_company AND
            mch-act.m-code  EQ tt-raw-prod.m-code AND
            mch-act.op-date GE v-start-of-year AND
            mch-act.op-date LE fi_as-of-date
            NO-LOCK:

       find first work-tmp where
            work-tmp.job = mch-act.job and
            work-tmp.frm = mch-act.frm and
            work-tmp.blank-no = mch-act.blank-no and
            work-tmp.dept = mch-act.dept and
            work-tmp.m-code = mch-act.m-code and
            work-tmp.pass = mch-act.pass
            no-error.

       find first b-mach WHERE
            b-mach.company eq fi_company AND
            b-mach.loc EQ locode AND
            b-mach.m-code  eq mch-act.m-code
            no-lock no-error.

       IF NOT AVAIL work-tmp THEN
       DO:
          CREATE work-tmp.
          ASSIGN
             work-tmp.job      = mch-act.job
             work-tmp.frm      = mch-act.frm
             work-tmp.blank-no = mch-act.blank-no
             work-tmp.dept     = IF mch-act.dept NE "" THEN mch-act.dept
                                 ELSE IF AVAIL b-mach THEN b-mach.dept[1] ELSE ""
             work-tmp.m-code   = mch-act.m-code
             work-tmp.pass     = mch-act.pass.
       END.

       FIND job-code WHERE
            job-code.code EQ mch-act.code
            NO-LOCK NO-ERROR.

       IF AVAIL job-code THEN
       DO:
           IF job-code.cat eq "RUN" THEN DO:

              work-tmp.qty = work-tmp.qty
                           + IF mch-act.qty EQ ? THEN 0 ELSE mch-act.qty.

              IF work-tmp.qty EQ ? THEN work-tmp.qty = 0.

              IF mch-act.op-date EQ tt-raw-prod.DATE THEN
                 ASSIGN
                    tt-raw-prod.date-qty = tt-raw-prod.date-qty
                                         + (IF mch-act.qty EQ ? THEN 0 ELSE mch-act.qty)
                    tt-raw-prod.date-run-hrs = tt-raw-prod.date-run-hrs
                                             + mch-act.hours.

              IF mch-act.op-date LE fi_as-of-date THEN
              DO:
                 IF MONTH(mch-act.op-date) EQ v-month-as-of-date THEN
                    ASSIGN
                       tt-raw-prod.mtd-qty = tt-raw-prod.mtd-qty
                                           + (IF mch-act.qty EQ ? THEN 0 ELSE mch-act.qty)
                       tt-raw-prod.mtd-run-hrs = tt-raw-prod.mtd-run-hrs
                                               + mch-act.hours.

                 ASSIGN
                    tt-raw-prod.ytd-qty = tt-raw-prod.ytd-qty
                                        + (IF mch-act.qty EQ ? THEN 0 ELSE mch-act.qty)
                    tt-raw-prod.ytd-run-hrs = tt-raw-prod.ytd-run-hrs
                                            + mch-act.hours.
              END.

              for each job-hdr FIELDS(company job frm i-no est-no frm blank-no n-on) WHERE
                  job-hdr.company   eq fi_company AND
                  job-hdr.job       eq work-tmp.job AND
                  job-hdr.frm       eq work-tmp.frm
                  no-lock,
                  first itemfg FIELDS(company i-no t-sqft) WHERE
                        itemfg.company eq fi_company AND
                        itemfg.i-no    eq job-hdr.i-no
                        no-lock:

                 IF NOT (job-hdr.blank-no eq mch-act.blank-no or mch-act.blank-no eq 0) THEN
                    NEXT.

                 assign
                    v-on  = 1
                    v-out = 1.

                 if avail b-mach and index("APB",b-mach.p-type) le 0 then do:

                    IF job-hdr.blank-no eq 0 THEN
                       find first eb WHERE
                            eb.company   eq job-hdr.company AND
                            eb.est-no    EQ job-hdr.est-no AND
                            eb.form-no   eq job-hdr.frm
                            no-lock no-error.
                    ELSE
                       find first eb WHERE
                            eb.company   eq job-hdr.company AND
                            eb.est-no    EQ job-hdr.est-no AND
                            eb.form-no   eq job-hdr.frm AND
                            eb.blank-no eq job-hdr.blank-no
                            no-lock no-error.

                     if avail eb then v-up = eb.num-up.

                     if job-hdr.n-on ne 0 then v-up = job-hdr.n-on.

                     find first ef
                         where ef.company eq job-hdr.company
                           and ef.est-no  EQ job-hdr.est-no
                           and ef.form-no eq job-hdr.frm
                         no-lock no-error.

                     IF AVAIL ef THEN RUN est/ef-#out.p (ROWID(ef), OUTPUT v-out).

                     v-on = v-up * v-out.

                     IF mch-act.blank-no NE 0 THEN
                        find first est-op
                         where est-op.company eq job-hdr.company
                           AND est-op.est-no  EQ job-hdr.est-no
                           and est-op.s-num   eq mch-act.frm
                           and est-op.b-num  eq mch-act.blank-no
                           and est-op.m-code  eq mch-act.m-code
                           and est-op.op-pass eq mch-act.pass
                           and est-op.dept    eq mch-act.dept
                           and est-op.line    lt 500
                         no-lock no-error.
                     ELSE
                        find first est-op
                         where est-op.company eq job-hdr.company
                           AND est-op.est-no  EQ job-hdr.est-no
                           and est-op.s-num   eq mch-act.frm
                           and est-op.m-code  eq mch-act.m-code
                           and est-op.op-pass eq mch-act.pass
                           and est-op.dept    eq mch-act.dept
                           and est-op.line    lt 500
                         no-lock no-error.

                     if not avail est-op then
                     DO:
                        IF mch-act.blank-no NE 0 THEN
                           find first est-op
                               where est-op.company eq job-hdr.company
                                 AND est-op.est-no  EQ job-hdr.est-no
                                 and est-op.s-num   eq mch-act.frm
                                 and (est-op.b-num  eq mch-act.blank-no or
                                      mch-act.blank-no eq 0)
                                 and est-op.op-pass eq mch-act.pass
                                 and est-op.dept    eq mch-act.dept
                                 and est-op.line    lt 500
                           no-lock no-error.
                        ELSE
                           find first est-op
                            where est-op.company eq job-hdr.company
                              AND est-op.est-no  EQ job-hdr.est-no
                              and est-op.s-num   eq mch-act.frm
                              and est-op.op-pass eq mch-act.pass
                              and est-op.dept    eq mch-act.dept
                              and est-op.line    lt 500
                           no-lock no-error.
                     END.

                     if avail est-op then
                       run sys/inc/numout.p (recid(est-op), output v-out).

                     else v-out = 1.

                     v-on = v-on / v-out.
                 end.

                 IF mch-act.op-date EQ tt-raw-prod.DATE THEN
                    tt-raw-prod.date-qty-msf = tt-raw-prod.date-qty-msf
                                             + (mch-act.qty * itemfg.t-sqft * v-on / 1000).

                 IF mch-act.op-date LE fi_as-of-date THEN
                 DO:
                    IF MONTH(mch-act.op-date) EQ v-month-as-of-date THEN
                       tt-raw-prod.mtd-qty-msf = tt-raw-prod.mtd-qty-msf
                                               + (mch-act.qty * itemfg.t-sqft * v-on / 1000).

                    tt-raw-prod.ytd-qty-msf = tt-raw-prod.ytd-qty-msf
                                            + (mch-act.qty * itemfg.t-sqft * v-on / 1000).
                 END.
              END.
           END.
           ELSE
              IF job-code.cat EQ "MR" THEN
              DO:
                 IF mch-act.op-date EQ tt-raw-prod.DATE THEN
                    tt-raw-prod.date-mr-hrs = tt-raw-prod.date-mr-hrs
                                            + mch-act.hours.

                 IF mch-act.op-date LE fi_as-of-date THEN
                 DO:
                    IF MONTH(mch-act.op-date) EQ v-month-as-of-date THEN
                       tt-raw-prod.mtd-mr-hrs = tt-raw-prod.mtd-mr-hrs
                                              + mch-act.hours.

                    tt-raw-prod.ytd-mr-hrs = tt-raw-prod.ytd-mr-hrs
                                           + mch-act.hours.
                 END.
              END.
           ELSE
              IF job-code.cat EQ "DT" THEN
              DO:
                 IF mch-act.op-date EQ tt-raw-prod.DATE THEN
                    tt-raw-prod.date-dt-charge = tt-raw-prod.date-dt-charge
                                               + mch-act.hours.

                 IF mch-act.op-date LE fi_as-of-date THEN
                 DO:
                    IF MONTH(mch-act.op-date) EQ v-month-as-of-date THEN
                       tt-raw-prod.mtd-dt-charge = tt-raw-prod.mtd-dt-charge
                                                 + mch-act.hours.


                    tt-raw-prod.ytd-dt-charge = tt-raw-prod.ytd-dt-charge
                                              + mch-act.hours.
                 END.
              END.
           ELSE
           DO:
              IF mch-act.op-date EQ tt-raw-prod.DATE THEN
                 tt-raw-prod.date-dt-nc = tt-raw-prod.date-dt-nc
                                        + mch-act.hours.

              IF mch-act.op-date LE fi_as-of-date THEN
              DO:
                 IF MONTH(mch-act.op-date) EQ v-month-as-of-date THEN
                    tt-raw-prod.mtd-dt-nc = tt-raw-prod.mtd-dt-nc
                                          + mch-act.hours.

                 tt-raw-prod.ytd-dt-nc = tt-raw-prod.ytd-dt-nc
                                       + mch-act.hours.
              END.
           END.
       END. /*avail job-code */

   END. /*each tt-raw-prod*/

   FOR EACH work-tmp,
       FIRST tt-raw-prod WHERE
             tt-raw-prod.m-code EQ work-tmp.m-code:

       IF work-tmp.blank-no NE 0 THEN
          find first job-mch where
               job-mch.company  = fi_company and
               job-mch.job      eq work-tmp.job and
               job-mch.frm      = work-tmp.frm and
               job-mch.blank-no = work-tmp.blank-no and
               job-mch.m-code   = work-tmp.m-code and
               job-mch.pass     = work-tmp.pass
               no-lock no-error.
       ELSE
          find first job-mch where
               job-mch.company  = fi_company and
               job-mch.job      eq work-tmp.job and
               job-mch.frm      = work-tmp.frm and
               job-mch.m-code   = work-tmp.m-code and
               job-mch.pass     = work-tmp.pass
               no-lock no-error.

       if not avail job-mch then
       DO:
          IF work-tmp.blank-no NE 0 THEN
             find first job-mch where
                  job-mch.company eq fi_company and
                  job-mch.job      eq work-tmp.job and
                  job-mch.frm      eq work-tmp.frm and
                  job-mch.blank-no = work-tmp.blank-no and
                  job-mch.m-code   eq work-tmp.m-code
                  no-lock no-error.
          ELSE
             find first job-mch where
                  job-mch.company eq fi_company and
                  job-mch.job      eq work-tmp.job and
                  job-mch.frm      eq work-tmp.frm and
                  job-mch.m-code   eq work-tmp.m-code
                  no-lock no-error.
       END.

       if not avail job-mch then
       find first job-mch where job-mch.company eq fi_company and
                                job-mch.job     eq work-tmp.job and
                                job-mch.frm     eq work-tmp.frm and
                                job-mch.m-code  eq work-tmp.m-code and
                                job-mch.speed   ne 0
                                no-lock no-error.
       if not avail job-mch then
       find first job-mch where job-mch.company eq fi_company and
                                job-mch.job     eq work-tmp.job and
                                job-mch.frm     eq work-tmp.frm and
                                job-mch.m-code  eq work-tmp.m-code
                                no-lock no-error.

       if available job-mch then
       DO:
          std-hrs-var = (IF work-tmp.qty NE 0 AND job-mch.speed NE 0 THEN
                            work-tmp.qty / job-mch.speed ELSE job-mch.run-hr)
                      + job-mch.mr-hr.

          IF job-mch.start-date EQ tt-raw-prod.DATE THEN
             tt-raw-prod.date-std-hrs = tt-raw-prod.date-std-hrs
                                      + std-hrs-var.

          IF job-mch.start-date LE fi_as-of-date THEN
          DO:
             IF MONTH(job-mch.start-date) EQ v-month-as-of-date THEN
                tt-raw-prod.mtd-std-hrs = tt-raw-prod.mtd-std-hrs
                                        + std-hrs-var.

             tt-raw-prod.ytd-std-hrs = tt-raw-prod.ytd-std-hrs
                                     + std-hrs-var.
          END.
       end.

   END. /*each work-tmp*/

   FOR EACH tt-raw-prod:

       ASSIGN
       tt-raw-prod.date-eff = (IF (tt-raw-prod.date-run-hrs +
                               tt-raw-prod.date-mr-hrs +
                               tt-raw-prod.date-dt-charge) NE 0 THEN
                               (tt-raw-prod.date-std-hrs / (tt-raw-prod.date-run-hrs +
                               tt-raw-prod.date-mr-hrs + tt-raw-prod.date-dt-charge) * 100)
                               ELSE 0)
       tt-raw-prod.date-util = (IF (tt-raw-prod.date-run-hrs +
                                tt-raw-prod.date-mr-hrs +
                                tt-raw-prod.date-dt-charge +
                                tt-raw-prod.date-dt-nc) NE 0 THEN
                                (tt-raw-prod.date-std-hrs / (tt-raw-prod.date-run-hrs +
                                tt-raw-prod.date-mr-hrs + tt-raw-prod.date-dt-charge +
                                tt-raw-prod.date-dt-nc) * 100)
                                ELSE 0)
       tt-raw-prod.date-dt-perc = (IF (tt-raw-prod.date-run-hrs +
                                tt-raw-prod.date-mr-hrs +
                                tt-raw-prod.date-dt-charge +
                                tt-raw-prod.date-dt-nc) NE 0 THEN
                                (tt-raw-prod.date-dt-nc / (tt-raw-prod.date-run-hrs +
                                tt-raw-prod.date-mr-hrs + tt-raw-prod.date-dt-charge +
                                tt-raw-prod.date-dt-nc) * 100)
                                ELSE 0)

       tt-raw-prod.mtd-eff = (IF (tt-raw-prod.mtd-run-hrs +
                              tt-raw-prod.mtd-mr-hrs +
                              tt-raw-prod.mtd-dt-charge) NE 0 THEN
                              (tt-raw-prod.mtd-std-hrs / (tt-raw-prod.mtd-run-hrs +
                              tt-raw-prod.mtd-mr-hrs + tt-raw-prod.mtd-dt-charge) * 100)
                              ELSE 0)
       tt-raw-prod.mtd-util = (IF (tt-raw-prod.mtd-run-hrs +
                              tt-raw-prod.mtd-mr-hrs +
                              tt-raw-prod.mtd-dt-charge + tt-raw-prod.mtd-dt-nc) NE 0 THEN
                              (tt-raw-prod.mtd-std-hrs / (tt-raw-prod.mtd-run-hrs +
                              tt-raw-prod.mtd-mr-hrs + tt-raw-prod.mtd-dt-charge +
                              tt-raw-prod.mtd-dt-nc) * 100)
                              ELSE 0)
       tt-raw-prod.mtd-dt-perc = (IF (tt-raw-prod.mtd-run-hrs +
                                 tt-raw-prod.mtd-mr-hrs +
                                 tt-raw-prod.mtd-dt-charge +
                                 tt-raw-prod.mtd-dt-nc) NE 0 THEN
                                 (tt-raw-prod.mtd-dt-nc / (tt-raw-prod.mtd-run-hrs +
                                 tt-raw-prod.mtd-mr-hrs + tt-raw-prod.mtd-dt-charge +
                                 tt-raw-prod.mtd-dt-nc) * 100)
                                 ELSE 0)
       tt-raw-prod.ytd-eff = (IF (tt-raw-prod.ytd-run-hrs +
                              tt-raw-prod.ytd-mr-hrs +
                              tt-raw-prod.ytd-dt-charge) NE 0 THEN
                              (tt-raw-prod.ytd-std-hrs / (tt-raw-prod.ytd-run-hrs +
                              tt-raw-prod.ytd-mr-hrs + tt-raw-prod.ytd-dt-charge) * 100)
                              ELSE 0)
       tt-raw-prod.ytd-util = (IF (tt-raw-prod.ytd-run-hrs +
                              tt-raw-prod.ytd-mr-hrs +
                              tt-raw-prod.ytd-dt-charge +
                              tt-raw-prod.ytd-dt-nc) NE 0 THEN
                              (tt-raw-prod.ytd-std-hrs / (tt-raw-prod.ytd-run-hrs +
                              tt-raw-prod.ytd-mr-hrs + tt-raw-prod.ytd-dt-charge +
                              tt-raw-prod.ytd-dt-nc) * 100)
                              ELSE 0)
       tt-raw-prod.ytd-dt-perc = (IF (tt-raw-prod.ytd-run-hrs +
                                 tt-raw-prod.ytd-mr-hrs +
                                 tt-raw-prod.ytd-dt-charge +
                                 tt-raw-prod.ytd-dt-nc) NE 0 THEN
                                 (tt-raw-prod.ytd-dt-nc / (tt-raw-prod.ytd-run-hrs +
                                 tt-raw-prod.ytd-mr-hrs + tt-raw-prod.ytd-dt-charge +
                                 tt-raw-prod.ytd-dt-nc) * 100)
                                 ELSE 0).
   END.


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

       for each ar-cash FIELDS(c-no ) WHERE 
           ar-cash.company    eq fi_company AND
           ar-cash.cust-no    eq cust.cust-no AND
           ar-cash.check-date ge from-date AND
           ar-cash.check-date le to-date AND
           ar-cash.posted     eq yes
           USE-INDEX ar-cash
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
  DEF VAR ld-inv-pct AS DEC NO-UNDO.
  DEF VAR v-start-of-year AS DATE NO-UNDO.
  DEF VAR v-end-of-year AS DATE NO-UNDO.
  DEF VAR v-this-month AS INT NO-UNDO.
  DEF VAR v-index AS INT NO-UNDO.

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
            NO-LOCK:

            IF NOT (b-ar-invl.billable OR NOT b-ar-invl.misc) THEN NEXT.
            IF NOT (NOT AVAIL ar-invl OR ROWID(b-ar-invl) EQ ROWID(ar-invl)) THEN NEXT.

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
            DO:
               assign
                 w-data.w-sqft[2] = w-data.w-sqft[2] + (v-sqft * v-pct)
                 w-data.w-amt[2]  = w-data.w-amt[2]  + (v-amt  * v-pct)
                 w-data.w-cost[2] = w-data.w-cost[2] + (v-cost * v-pct).
            END.

            assign
              w-data.w-sqft[3] = w-data.w-sqft[3] + (v-sqft * v-pct)
              w-data.w-amt[3]  = w-data.w-amt[3]  + (v-amt  * v-pct)
              w-data.w-cost[3] = w-data.w-cost[3] + (v-cost * v-pct).
         END.

         assign
            v-index = MONTH(ar-inv.inv-date)
            tt-raw-salesmen.amt[v-index] = tt-raw-salesmen.amt[v-index] + (v-amt * v-pct)
            tt-raw-salesmen.msf[v-index] = tt-raw-salesmen.msf[v-index] + (v-sqft  * v-pct).
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
            tt-raw-salesmen.amt[v-index] = tt-raw-salesmen.amt[v-index] + (v-amt  * v-pct)
            tt-raw-salesmen.msf[v-index] = tt-raw-salesmen.msf[v-index] - (v-sqft * v-pct).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reftable-proc C-Win 
PROCEDURE reftable-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   do counter = 1 to BROWSE browse-machine:NUM-SELECTED-ROWS:
      BROWSE browse-machine:FETCH-SELECTED-ROW(counter).

      CREATE tt-raw-prod.
      ASSIGN tt-raw-prod.m-code = mach.m-code
             tt-raw-prod.DATE   = fi_as-of-date.
      RELEASE tt-raw-prod.
   end.

   DO counter = 1 to BROWSE browse-sales-forecast:NUM-SELECTED-ROWS:
      BROWSE browse-sales-forecast:FETCH-SELECTED-ROW(counter).

      CREATE tt-sales-forecast.
      tt-sales-forecast.company = company.company.
      RELEASE tt-sales-forecast.
   end.

   do counter = 1 to BROWSE browse-ar:NUM-SELECTED-ROWS:
      BROWSE browse-ar:FETCH-SELECTED-ROW(counter).

      CREATE tt-ar-dso.
      tt-ar-dso.actnum = account.actnum.
      RELEASE tt-ar-dso.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE released-sales-forecast-proc C-Win 
PROCEDURE released-sales-forecast-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*OR12 Invoice*/

  DEFINE INPUT PARAMETER ip-start-next-month AS DATE NO-UNDO.

  DEF VAR v-type AS CHAR NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR v-qty like oe-rel.qty NO-UNDO.
  DEF VAR ld AS DEC NO-UNDO.
  DEF VAR v-price like oe-ordl.price NO-UNDO.
  DEF VAR v-value-head AS LOG NO-UNDO.

  FOR EACH tt-sales-forecast:

    EMPTY TEMP-TABLE tt-report.

    FOR EACH oe-ordl FIELDS(company ord-no i-no LINE s-man)
        WHERE oe-ordl.company EQ tt-sales-forecast.company
          AND oe-ordl.opened  EQ YES
          AND NOT CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl})
        USE-INDEX opened NO-LOCK:

        /* RUN oe/cleanrel.p (ROWID(oe-ordl)).*/

        for each oe-rel FIELDS(i-no cust-no)
            where oe-rel.company   eq tt-sales-forecast.company
              and oe-rel.ord-no    eq oe-ordl.ord-no
              and oe-rel.i-no      eq oe-ordl.i-no
              and oe-rel.line      eq oe-ordl.line
              and oe-rel.rel-date  ge 01/01/0001
              and oe-rel.rel-date  LT ip-start-next-month
            no-lock:

            RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-type).

            IF v-type EQ "Z" THEN
               DO li = 1 TO EXTENT(oe-ordl.s-man):
                  IF oe-ordl.s-man[li] NE "" THEN DO:
                     create tt-report.
                     assign
                      tt-report.rec-id  = recid(oe-rel)
                      tt-report.key-04 = STRING(oe-ordl.ord-no).
                     IF li GT 1 THEN
                         tt-report.is-duplicate = YES.
                  END.
               END.
        END. /* each oe-rel*/
    END. /*each oe-ordl*/

    FOR EACH tt-report WHERE tt-report.is-duplicate = NO
        BREAK BY tt-report.key-04:

        release oe-rel.
        release oe-rell.

        find first oe-rel WHERE
             recid(oe-rel) eq tt-report.rec-id 
             no-lock no-error.

        if avail oe-rel then do:
           FOR EACH oe-rell FIELDS(r-no)
               WHERE oe-rell.company  EQ tt-sales-forecast.company
                 AND oe-rell.ord-no   EQ oe-rel.ord-no
                 AND oe-rell.rel-no   EQ oe-rel.rel-no
                 AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
                 AND oe-rell.i-no     EQ oe-rel.i-no
                 AND oe-rell.line     EQ oe-rel.line
                 AND CAN-FIND(FIRST oe-relh
                              WHERE oe-relh.r-no    EQ oe-rell.r-no
                                AND oe-relh.posted  EQ NO
                                AND oe-relh.deleted EQ NO)
               USE-INDEX ord-no NO-LOCK:
             tt-report.rec-id = RECID(oe-rell).
             LEAVE.
           END.

           find first oe-ordl
               where oe-ordl.company eq tt-sales-forecast.company
                 and oe-ordl.ord-no  eq oe-rel.ord-no
                 and oe-ordl.i-no    eq oe-rel.i-no
                 and oe-ordl.line    eq oe-rel.line
               no-lock.
        end. /* avail oe-rel*/

        find first oe-rell
            where recid(oe-rell) eq tt-report.rec-id
            no-lock no-error.

        if avail oe-rell THEN
        DO:
           find first oe-ordl WHERE
                oe-ordl.company eq tt-sales-forecast.company AND
                oe-ordl.ord-no  eq oe-rell.ord-no AND
                oe-ordl.i-no    eq oe-rell.i-no AND
                oe-ordl.line    eq oe-rell.line
                no-lock.

           v-qty = oe-rell.qty.
        END.

        else
           v-qty = oe-rel.qty.

        RELEASE b-oe-ordl.
        IF oe-ordl.is-a-component THEN
        FIND FIRST b-oe-ordl
            WHERE b-oe-ordl.company EQ oe-ordl.company
              AND b-oe-ordl.ord-no  EQ oe-ordl.ord-no
              AND b-oe-ordl.line    EQ oe-ordl.set-hdr-line
              AND b-oe-ordl.is-a-component EQ NO
            NO-LOCK NO-ERROR.

        RELEASE b-itemfg.

        v-value-head = NO.

        IF AVAIL b-oe-ordl AND tg_set-comp EQ YES THEN
        DO:
           v-value-head = YES.

           IF FIRST-OF(tt-report.key-04) THEN
              v-price = b-oe-ordl.t-price.
           ELSE
              v-price = 0.
        END.
        ELSE
        DO:
           IF AVAIL b-oe-ordl THEN
           FIND FIRST b-itemfg
                WHERE b-itemfg.company EQ b-oe-ordl.company
                  AND b-itemfg.i-no    EQ b-oe-ordl.i-no
                NO-LOCK NO-ERROR.

           RELEASE itemfg.
           IF AVAIL b-itemfg THEN
           FIND FIRST itemfg
               WHERE itemfg.company EQ oe-ordl.company
                 AND itemfg.i-no    EQ oe-ordl.i-no
               NO-LOCK NO-ERROR.

           IF AVAIL itemfg THEN DO:
             IF itemfg.std-tot-cost NE 0 THEN
                ld = (itemfg.std-tot-cost * oe-ordl.qty) /
                     (b-itemfg.std-tot-cost * b-oe-ordl.qty).
             ELSE
                ld = (itemfg.weight-100 * oe-ordl.qty) /
                     (b-itemfg.weight-100 * b-oe-ordl.qty).

             v-price = b-oe-ordl.t-price * ld / b-oe-ordl.qty.
           END.
           ELSE v-price = oe-ordl.t-price / oe-ordl.qty.
        END.

        IF v-price EQ ? THEN v-price = 0.

        IF v-value-head EQ NO THEN
           tt-sales-forecast.released-amt = tt-sales-forecast.released-amt + (v-price * v-qty).
        ELSE
           tt-sales-forecast.released-amt = tt-sales-forecast.released-amt + v-price.

        ASSIGN
          tt-sales-forecast.released-qty = tt-sales-forecast.released-qty + v-qty
          tt-sales-forecast.released-cost = tt-sales-forecast.released-cost + ((v-qty / 1000) * oe-ordl.cost).

        RELEASE itemfg.

        FIND FIRST itemfg WHERE
             itemfg.company EQ oe-ordl.company AND
             itemfg.i-no    EQ oe-ordl.i-no
             NO-LOCK NO-ERROR.

        IF AVAIL itemfg THEN
           tt-sales-forecast.released-msf = tt-sales-forecast.released-msf + (v-qty * itemfg.t-sqft / 1000).

    END. /* each tt-report*/

  END. /*each tt-sales-forecast*/

  FOR EACH tt-sales-forecast:
      tt-sales-forecast.released-profit = ROUND(tt-sales-forecast.released-amt -
                                                tt-sales-forecast.released-cost,2).

  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
DO WITH FRAME {&FRAME-NAME}:

   RUN raw-op-proc. /*Raw OP*/
   RUN raw-prod-proc. /*Raw Production*/
   RUN raw-sales-proc. /*Raw Sales*/
   RUN raw-prod-cat-proc. /*Raw Sales PC*/
   RUN raw-salesmen-proc. /*Raw Salesmen*/
   RUN ap-ar-proc. /*AP/AR*/
   RUN sales-forecast-proc. 
   RUN dso-proc.
   RUN salrep\dashboard.p(INPUT fi_company,
                          INPUT fi_as-of-date,
                          INPUT v-dso,
                          INPUT tg_round).
   RUN saveparameters.
   RUN custom\usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
END.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sales-forecast-proc C-Win 
PROCEDURE sales-forecast-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR start-month AS DATE NO-UNDO.
  DEF VAR start-next-month AS DATE NO-UNDO.

  start-month = DATE(MONTH(fi_as-of-date),1,YEAR(fi_as-of-date)).

  IF MONTH(fi_as-of-date) NE 12 THEN
     start-next-month = DATE(MONTH(fi_as-of-date) + 1,1,YEAR(fi_as-of-date)).
  ELSE
     start-next-month = DATE(1,1,YEAR(fi_as-of-date) + 1).

  RUN invoiced-sales-forecast-proc(INPUT start-month, INPUT start-next-month).
  RUN backlog-sales-forecast-proc(INPUT start-month, INPUT start-next-month).
  RUN released-sales-forecast-proc(INPUT start-next-month).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveparameters C-Win 
PROCEDURE saveparameters :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-user AS CHAR NO-UNDO.

   v-user = USERID("NOSWEAT").

   DO TRANSACTION:
      ASSIGN i = 0 .
      FOR EACH tt-raw-prod:
          
          FIND FIRST user-print EXCLUSIVE-LOCK
              WHERE user-print.company    EQ cocode        
              AND user-print.program-id EQ "HM1" 
              AND user-print.user-id EQ USERID(LDBNAME(1)) NO-ERROR.
          IF NOT AVAIL user-print THEN DO:
              CREATE user-print .
              ASSIGN
                  user-print.company     = cocode            
                  user-print.program-id  = "HM1"     
                  user-print.user-id     = USERID(LDBNAME(1)) .
          END.
          i = i + 1 .
          ASSIGN user-print.field-value[i] =  tt-raw-prod.m-code  .


      END.  /* FOR EACH tt-raw-prod */

      
      i = 0 .
      FOR EACH tt-ar-dso:
          
          FIND FIRST user-print EXCLUSIVE-LOCK
              WHERE user-print.company    EQ cocode        
              AND user-print.program-id EQ "HM1Acct" 
              AND user-print.user-id EQ USERID(LDBNAME(1)) NO-ERROR.
          IF NOT AVAIL user-print THEN DO:
              CREATE user-print .
              ASSIGN
                  user-print.company     = cocode            
                  user-print.program-id  = "HM1Acct"     
                  user-print.user-id     = USERID(LDBNAME(1)) .
          END.
          i = i + 1 .
          ASSIGN user-print.field-value[i] =  tt-ar-dso.actnum  .

      END.  /* FOR EACH tt-ar-dso */

      
      i = 0.
      FOR EACH tt-sales-forecast:
        
          FIND FIRST user-print EXCLUSIVE-LOCK
              WHERE user-print.company    EQ cocode        
              AND user-print.program-id EQ "HM1SF" 
              AND user-print.user-id EQ USERID(LDBNAME(1)) NO-ERROR.
          IF NOT AVAIL user-print THEN DO:
              CREATE user-print .
              ASSIGN
                  user-print.company     = cocode            
                  user-print.program-id  = "HM1SF"     
                  user-print.user-id     = USERID(LDBNAME(1)) .
          END.
          i = i + 1 .
          ASSIGN user-print.field-value[i] =  tt-sales-forecast.company  .

      END.  /* tt-sales-forecast */

      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

