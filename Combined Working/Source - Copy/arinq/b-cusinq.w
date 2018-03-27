&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
/* Procedure Description
"ASI SmartNavBrowser Object Template with Wizard.

Use this template to create a new SmartNavBrowser object with the assistance of the SmartBrowser Wizard. When completed, this object can then be drawn onto any 'smart' container such as a SmartWindow, SmartDialog or SmartFrame."
*/
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  arinq\b-cusinq.w

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

&SCOPED-DEFINE yellowColumnsName b-cusinq
&SCOPED-DEFINE winReSize
&SCOPED-DEFINE sizeOption HEIGHT
&SCOPED-DEFINE browseOnly
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

DEF SHARED VAR g_company AS cha NO-UNDO.
DEF SHARED VAR g_loc AS cha NO-UNDO.
DEF SHARED VAR g_period AS INT NO-UNDO.
{methods/defines/hndldefs.i}               

{sys/inc/VAR.i NEW SHARED}

DEF TEMP-TABLE tt-arinq NO-UNDO
  FIELD ref-num AS CHAR FORM "x(15)" LABEL "Ck/Cr/Dr/Po#"
  FIELD inv-no LIKE ar-inv.inv-no LABEL "Invoice#"
  FIELD tr-date AS DATE FORM "99/99/9999" LABEL "Date"
  FIELD tr-dscr LIKE gltrans.tr-dscr LABEL "Description"
  FIELD tr-damt LIKE gltrans.tr-amt LABEL "Debits"
  FIELD tr-camt LIKE gltrans.tr-amt LABEL "Credits"
  FIELD ageapp AS CHAR FORM "x(5)" LABEL "Age App"
  FIELD tr-from AS CHAR LABEL "Inquiry From"
  FIELD balance AS dec FORM "->>>,>>>,>>9.99" LABEL "Balance"
  FIELD applied AS LOG
  FIELD seq AS INT
  FIELD printed LIKE ar-inv.printed
  FIELD posted LIKE ar-inv.posted
  INDEX seq seq
  INDEX applied IS PRIMARY applied seq
  INDEX ref-num ref-num seq
  INDEX inv-no inv-no seq
  INDEX tr-date tr-date seq
  INDEX tr-dscr tr-dscr seq
  INDEX tr-damt tr-damt seq
  INDEX tr-camt tr-camt seq
  INDEX ageapp ageapp seq
  INDEX tr-from tr-from seq
  INDEX balance balance seq.

ASSIGN cocode = g_company
       locode = g_loc.

DEF VAR lv-first AS LOG NO-UNDO.
DEF VAR lv-sort-by AS cha NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.
DEF VAR v-format AS cha NO-UNDO.
DEF NEW SHARED VAR uperiod AS INT NO-UNDO.  /* for gl-open.p */
DEF VAR v-gltrans-desc AS CHAR NO-UNDO.

&SCOPED-DEFINE SORTBY-ASC ASCENDING
&SCOPED-DEFINE SORTBY-DES DESCENDING

DEF VAR lv-save-char AS CHAR INIT "" NO-UNDO.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "ARINQ"
    no-lock no-error.
if not avail sys-ctrl then do on error undo, retry transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "ARINQ"
   sys-ctrl.descrip  = "AR Customer Activity Inquiry format"
   sys-ctrl.char-fld = "ASI".
  message "Enter" sys-ctrl.descrip " (ASI/Fibre)"
          update sys-ctrl.char-fld.
  if lookup(sys-ctrl.char-fld,"ASI,Fibre") eq 0 then undo, retry.
end.
v-format = sys-ctrl.char-fld.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target,Navigation-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Browser-Table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-arinq

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table tt-arinq.ref-num tt-arinq.inv-no tt-arinq.tr-date tt-arinq.tr-dscr tt-arinq.ageapp tt-arinq.tr-damt tt-arinq.tr-camt display-balance () @ tt-arinq.balance tt-arinq.balance display-balance () @ tt-arinq.balance   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table tt-arinq.ref-num ~
tt-arinq.inv-no ~
tt-arinq.tr-date ~
tt-arinq.tr-dscr ~
tt-arinq.ageapp ~
tt-arinq.tr-damt ~
tt-arinq.tr-camt ~
tt-arinq.balance   
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table tt-arinq
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table tt-arinq
&Scoped-define SELF-NAME Browser-Table
&Scoped-define QUERY-STRING-Browser-Table FOR EACH tt-arinq ~{&SORTBY-PHRASE} BY tt-arinq.seq
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY {&SELF-NAME} FOR EACH tt-arinq ~{&SORTBY-PHRASE} BY tt-arinq.seq.
&Scoped-define TABLES-IN-QUERY-Browser-Table tt-arinq
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table tt-arinq


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi_cust fi_finv fi_tinv fi_fchk fi_tchk ~
fi_days tb_open btn_go btn_print Browser-Table 
&Scoped-Define DISPLAYED-OBJECTS fi_cust fi_name fi_finv fi_tinv fi_fchk ~
fi_tchk fi_days tb_open fi_sortby 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-balance B-table-Win 
FUNCTION display-balance RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_go 
     LABEL "&Go" 
     SIZE 12 BY 1.

DEFINE BUTTON btn_print 
     LABEL "&Print" 
     SIZE 12 BY 1.

DEFINE VARIABLE fi_cust AS CHARACTER FORMAT "X(8)":U 
     LABEL "Customer#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_days AS INTEGER FORMAT ">,>>9":U INITIAL 9999 
     LABEL "Up to" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_fchk AS INTEGER FORMAT ">>>>>>>>>>":U INITIAL 0 
     LABEL "Beginning Check#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_finv AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     LABEL "Beginning Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_name AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sorted By" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE fi_tchk AS INTEGER FORMAT ">>>>>>>>>>":U INITIAL 2147483647 
     LABEL "Ending Check#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_tinv AS INTEGER FORMAT ">>>>>>>>" INITIAL 99999999 
     LABEL "Ending Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 148 BY 3.62.

DEFINE VARIABLE tb_open AS LOGICAL INITIAL yes 
     LABEL "Open Invoices Only?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      tt-arinq SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _FREEFORM
  QUERY Browser-Table NO-LOCK DISPLAY
      tt-arinq.ref-num FORM "X(12)" LABEL-BGCOLOR 14
     tt-arinq.inv-no LABEL-BGCOLOR 14
     tt-arinq.tr-date LABEL-BGCOLOR 14
     tt-arinq.tr-dscr FORM "X(11)" LABEL-BGCOLOR 14
     tt-arinq.ageapp LABEL-BGCOLOR 14
     tt-arinq.tr-damt LABEL-BGCOLOR 14
     tt-arinq.tr-camt LABEL-BGCOLOR 14
     display-balance () @ tt-arinq.balance
     tt-arinq.balance LABEL-BGCOLOR 14
     display-balance () @ tt-arinq.balance
     ENABLE
       tt-arinq.ref-num
       tt-arinq.inv-no
       tt-arinq.tr-date
       tt-arinq.tr-dscr
       tt-arinq.ageapp
       tt-arinq.tr-damt
       tt-arinq.tr-camt
       tt-arinq.balance
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 148 BY 16.38
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi_cust AT ROW 1.24 COL 15 COLON-ALIGNED
     fi_name AT ROW 1.24 COL 37 COLON-ALIGNED HELP
          "Enter Finished Goods Name used for Alpha Numeric Searches." NO-LABEL
     fi_finv AT ROW 2.43 COL 20 COLON-ALIGNED
     fi_tinv AT ROW 2.43 COL 54 COLON-ALIGNED
     fi_fchk AT ROW 3.48 COL 20 COLON-ALIGNED
     fi_tchk AT ROW 3.48 COL 54 COLON-ALIGNED
     fi_days AT ROW 1.24 COL 99 COLON-ALIGNED
     tb_open AT ROW 2.43 COL 73
     btn_go AT ROW 1.24 COL 122
     btn_print AT ROW 1.24 COL 136
     Browser-Table AT ROW 4.62 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     fi_sortby AT ROW 2.43 COL 108 COLON-ALIGNED
     "Days Old" VIEW-AS TEXT
          SIZE 10 BY 1 AT ROW 1.24 COL 111
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 
         DEFAULT-BUTTON btn_go.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
   Other Settings: PERSISTENT-ONLY
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 20
         WIDTH              = 148.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/navbrows.i}
{custom/yellowColumns.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
/* BROWSE-TAB Browser-Table btn_print F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR FILL-IN fi_name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-arinq ~{&SORTBY-PHRASE} BY tt-arinq.seq.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,"
     _Query            is NOT OPENED
*/  /* BROWSE Browser-Table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME Browser-Table
&Scoped-define SELF-NAME Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON START-SEARCH OF Browser-Table IN FRAME F-Main
DO:
  RUN startSearch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  IF AVAILABLE tt-arinq THEN
  RUN custom/setUserPrint.p (g_company,'ar-inv_.',
                             'begin_inv,end_inv,begin_cust,end_cust,tb_reprint,tb_posted',
                             STRING(tt-arinq.inv-no) + ',' + STRING(tt-arinq.inv-no) + ',' +
                             fi_cust:SCREEN-VALUE + ',' + fi_cust:SCREEN-VALUE + ',' +
                             STRING(tt-arinq.printed) + ',' + STRING(tt-arinq.posted)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Go */
DO:
  RUN valid-cust NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&DISPLAYED-OBJECTS}.
  END.

  RUN dispatch ("open-query").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_print B-table-Win
ON CHOOSE OF btn_print IN FRAME F-Main /* Print */
DO:
{sys/form/r-topw.f}

DEF VAR lv-cust  LIKE cust.cust-no          NO-UNDO.
DEF VAR lv-name  LIKE cust.NAME             NO-UNDO.
DEF VAR lv-finv  AS   INT FORMAT ">>>>>>>>" NO-UNDO.
DEF VAR lv-tinv  LIKE lv-finv               NO-UNDO.
DEF VAR li-days  AS   INT                   NO-UNDO.
DEF VAR ll-open  AS   LOG                   NO-UNDO.

FORM SKIP(1)
     lv-cust COLON 30 LABEL "Customer#"
     lv-name          NO-LABEL
     lv-finv COLON 30 LABEL "Beginning Inv#"
     lv-tinv COLON 30 LABEL "Ending Inv#"
     li-days COLON 30 LABEL "Up to How Many Days Old?"
     ll-open COLON 30 LABEL "Open Invoices Only?"
     SKIP(1)

      WITH STREAM-IO WIDTH 80 FRAME ar-inq SIDE-LABELS NO-UNDERLINE PAGE-TOP
           TITLE "  C U S T O M E R   A C T I V I T Y  ".

format tt-arinq.ref-num format "X(12)"          column-label "Ck/Cr/Dr#"
       tt-arinq.inv-no  format "zzzzzz"         column-label "Inv. #"
       tt-arinq.tr-date format "99/99/99"       column-label "Date"
       tt-arinq.tr-dscr format "X(11)"          column-label "Descr."
       tt-arinq.ageapp                          column-label "Appl"
       tt-arinq.tr-damt format "->,>>>,>>>.99"  column-label "Debits"
       tt-arinq.tr-camt format "->,>>>,>>>.99"  column-label "Credits"
       tt-arinq.balance format "->,>>>,>>>.99"  column-label "Balance"

    with no-box no-attr-space frame a2-1 row 8 12 down stream-io width 132 centered.

format tt-arinq.ref-num format "X(12)"          column-label "Ck/Cr/Dr#/PO"
       tt-arinq.inv-no  format "zzzzzz"         column-label "Inv. #"
       tt-arinq.tr-date format "99/99/99"       column-label "Date"
       tt-arinq.tr-dscr format "X(11)"          column-label "Descr."
       tt-arinq.ageapp                          column-label "Age App"
       tt-arinq.tr-damt format "->,>>>,>>>.99"  column-label "Debits"
       tt-arinq.tr-camt format "->,>>>,>>>.99"  column-label "Credits"
       tt-arinq.balance format "->,>>>,>>>.99"  column-label "Balance"

    with no-box no-attr-space frame a2-2 row 8 12 down stream-io width 132 centered.


  FIND FIRST tt-arinq NO-ERROR.

  IF AVAIL tt-arinq THEN DO WITH FRAME ar-inq:
    SESSION:SET-WAIT-STATE ("general").

    {sys/inc/print1.i}
    {sys/inc/outprint.i 56}

    VIEW FRAME r-top.

    DISPLAY fi_cust @ lv-cust
            fi_name @ lv-name
            fi_finv @ lv-finv
            fi_tinv @ lv-tinv
            fi_days @ li-days
            tb_open @ ll-open.

    FOR EACH tt-arinq BY tt-arinq.seq:
      IF v-format EQ "ASI" THEN DO WITH FRAME a2-1:
        DISPLAY tt-arinq.ref-num
                tt-arinq.inv-no
                tt-arinq.tr-date
                tt-arinq.tr-dscr
                tt-arinq.ageapp
                tt-arinq.tr-damt
                tt-arinq.tr-camt
                tt-arinq.balance.
         DOWN.
      END.
      ELSE DO WITH FRAME a2-2:
        DISPLAY tt-arinq.ref-num
                tt-arinq.inv-no
                tt-arinq.tr-date
                tt-arinq.tr-dscr
                tt-arinq.ageapp
                tt-arinq.tr-damt
                tt-arinq.tr-camt
                tt-arinq.balance.
        DOWN.
      END.
    END.

    OUTPUT CLOSE.

    SESSION:SET-WAIT-STATE ("").

    RUN scr-rpt.w (list-name,TRIM(FRAME ar-inq:TITLE),11,"P"). /* open file-name, title */
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cust B-table-Win
ON ENTRY OF fi_cust IN FRAME F-Main /* Customer# */
DO:
  RUN new-cust.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cust B-table-Win
ON LEAVE OF fi_cust IN FRAME F-Main /* Customer# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-cust NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cust B-table-Win
ON VALUE-CHANGED OF fi_cust IN FRAME F-Main /* Customer# */
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1 + IF LASTKEY EQ 32 THEN 1 ELSE 0. /* added by script _caps.p */

  RUN new-cust.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_fchk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_fchk B-table-Win
ON LEAVE OF fi_fchk IN FRAME F-Main /* Beginning Check# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_tchk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_tchk B-table-Win
ON LEAVE OF fi_tchk IN FRAME F-Main /* Ending Check# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_open
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_open B-table-Win
ON RETURN OF tb_open IN FRAME F-Main /* Open Invoices Only? */
DO:
  APPLY "tab" TO {&self-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
SESSION:DATA-ENTRY-RETURN = YES.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-tempfile B-table-Win 
PROCEDURE create-tempfile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR xsum AS DEC NO-UNDO.
  def var v-open       as   log format "Y/N" init "N".
  def var t-check-no  as   char   format "x(8)".
  def var x-check-no  as   char   format "x(10)".
  def var t-credits   as   dec    format ">,>>>,>>>.99"  column-label "Credits".
  def var t-debits    as   dec    format ">,>>>,>>>.99"  column-label "Debits".
  def var t-balance   as   dec    format ">>,>>>,>>>.99" column-label "Balance".
  def var v-tot-due   as   dec    format "->,>>>,>>9.99".
  def var v-pay-stat1 as   log    format "Y/N".
  def var v-pay-stat2 as   char   format "x(4)".
  DEF VAR li-seq AS INT NO-UNDO.
  DEF VAR lv-cust-no LIKE ar-cashl.cust-no NO-UNDO.
  DEF VAR ll-valid AS LOG NO-UNDO.
  DEF VAR li-fchk LIKE ar-cash.check-no NO-UNDO.
  DEF VAR li-tchk LIKE ar-cash.check-no NO-UNDO.


  SESSION:SET-WAIT-STATE ("general").

  FOR EACH tt-arinq:
    DELETE tt-arinq.
  END.

  ASSIGN
   v-open  = tb_open
   li-seq  = 0
   xsum    = 0
   li-fchk = fi_fchk
   li-tchk = fi_tchk.

  IF NOT CAN-FIND(FIRST ar-cash
                  WHERE ar-cash.company  EQ cocode
                    AND ar-cash.memo     EQ YES
                    AND ar-cash.check-no LT li-fchk) AND
     NOT CAN-FIND(FIRST ar-cash
                  WHERE ar-cash.company  EQ cocode
                    AND ar-cash.memo     EQ NO
                    AND ar-cash.check-no LT li-fchk) THEN li-fchk = 0.

  IF NOT CAN-FIND(FIRST ar-cash
                  WHERE ar-cash.company  EQ cocode
                    AND ar-cash.memo     EQ YES
                    AND ar-cash.check-no GT li-fchk) AND
     NOT CAN-FIND(FIRST ar-cash
                  WHERE ar-cash.company  EQ cocode
                    AND ar-cash.memo     EQ NO
                    AND ar-cash.check-no GT li-fchk) THEN li-fchk = 2147483647.

  for each ar-inv no-lock
      where ar-inv.company  eq cocode
        and ar-inv.cust-no  eq fi_cust /*or fi_cust eq "")*/
        and ar-inv.inv-no   ge fi_finv
        and ar-inv.inv-no   le fi_tinv
        and ar-inv.posted   eq yes
        and ar-inv.terms    ne "CASH"
        and ar-inv.inv-date ge (today - fi_days)        
      USE-INDEX ar-inv
      BY ar-inv.inv-date
      BY ar-inv.inv-no:

    if v-format eq "ASI" then do:
      {ar/ar-iact1.i 1}
    end.    
    else do:
      {ar/ar-iact1.i 2}
    end.
  end. /* for each ar-inv record */

  /* display unapplied cr/db memos/payments */
  lv-cust-no = fi_cust.

  DO WHILE TRUE:
    IF lv-cust-no NE "" THEN
    FOR EACH ar-cashl NO-LOCK
        WHERE ar-cashl.company EQ cocode
          AND ar-cashl.posted  EQ YES
          AND ar-cashl.cust-no EQ lv-cust-no
          AND ar-cashl.inv-no  EQ 0,      
        FIRST ar-cash NO-LOCK
        WHERE ar-cash.c-no     EQ ar-cashl.c-no
          AND ar-cash.check-no GE li-fchk
          AND ar-cash.check-no LE li-tchk
        BY ar-cash.check-date
        BY ar-cash.c-no:

      IF v-format eq "ASI" THEN DO:
        {ar/ar-iact2.i 1}
      END.    
      ELSE DO:
        {ar/ar-iact2.i 2}
      END.  
    END. /* for each ar-cash record */

    RELEASE ar-cashl.

    IF fi_cust EQ "" THEN
    FIND FIRST ar-cashl
        WHERE ar-cashl.company EQ cocode
          AND ar-cashl.posted  EQ YES
          AND ar-cashl.company GT lv-cust-no
        NO-LOCK NO-ERROR.
    IF NOT AVAIL ar-cashl THEN LEAVE.

    lv-cust-no = ar-cashl.cust-no.
  END.
  APPLY 'VALUE-CHANGED':U TO BROWSE {&BROWSE-NAME}.

  SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Disable-Navigation B-table-Win 
PROCEDURE Disable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Enable-Navigation B-table-Win 
PROCEDURE Enable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    APPLY "entry" TO fi_cust.
    tt-arinq.ageapp:LABEL IN BROWSE {&browse-name} =
                                IF v-format EQ "ASI" THEN "Appl" ELSE "Age App".

    ASSIGN 
     tt-arinq.ref-num:READ-ONLY IN BROWSE {&browse-name} = YES
     tt-arinq.inv-no:READ-ONLY IN BROWSE {&browse-name} = YES
     tt-arinq.tr-date:READ-ONLY IN BROWSE {&browse-name} = YES
     tt-arinq.tr-dscr:READ-ONLY IN BROWSE {&browse-name} = YES
     tt-arinq.ageapp:READ-ONLY IN BROWSE {&browse-name} = YES
     tt-arinq.tr-damt:READ-ONLY IN BROWSE {&browse-name} = YES
     tt-arinq.tr-camt:READ-ONLY IN BROWSE {&browse-name} = YES 
     tt-arinq.balance:READ-ONLY IN BROWSE {&browse-name} = YES.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF fi_cust NE "" THEN RUN create-tempfile.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-cust B-table-Win 
PROCEDURE new-cust :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&frame-name}:
    FIND cust
        WHERE cust.company EQ cocode
          AND cust.cust-no EQ fi_cust:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAIL cust THEN
      ASSIGN
       fi_cust:SCREEN-VALUE = cust.cust-no
       fi_name:SCREEN-VALUE = cust.name. 
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-arinq"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-focus B-table-Win 
PROCEDURE set-focus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{methods/setfocus.i {&BROWSE-NAME}}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-xl B-table-Win 
PROCEDURE export-xl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR first-cust AS CHAR NO-UNDO.
DEF VAR last-cust AS CHAR NO-UNDO.

/*GET FIRST Browser-Table .
ASSIGN first-cust = cust.cust-no .
GET LAST Browser-Table .
ASSIGN last-cust = cust.cust-no . */

/*RUN fg/phon-exp.w (first-cust ,last-cust).*/

RUN arinq/csnq-exp.w ("", "").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust B-table-Win 
PROCEDURE valid-cust :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    RUN new-cust.

    IF NOT CAN-FIND(FIRST cust WHERE cust.company EQ cocode
                                 AND cust.cust-no EQ fi_cust:SCREEN-VALUE)
    THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fi_cust.
      RETURN ERROR.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-balance B-table-Win 
FUNCTION display-balance RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR lv-label-3 AS CHAR NO-UNDO.
  DEF VAR lv-label-8 AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     lv-label-3 = BROWSE {&BROWSE-NAME}:GET-BROWSE-COLUMN(3):LABEL
     lv-label-8 = BROWSE {&BROWSE-NAME}:GET-BROWSE-COLUMN(8):LABEL.

    RETURN (IF fi_sortby:SCREEN-VALUE EQ ""             OR
               fi_sortby:SCREEN-VALUE BEGINS lv-label-3 OR
               fi_sortby:SCREEN-VALUE BEGINS lv-label-8 THEN tt-arinq.balance 
            ELSE 0).   /* Function return value. */
  END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

