&SCOPED-DEFINE SVB SCREEN-VALUE IN BROWSE {&browse-name}
&SCOPED-DEFINE FVB FORMAT IN BROWSE {&browse-name}
&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&SCOPED-DEFINE WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: browsers\probe.w

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

&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE voverall AS DECIMAL FORMAT ">>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE vtot-msf AS DECIMAL FORMAT ">>>>9.99" NO-UNDO.
DEFINE VARIABLE dMatPctSellPrice LIKE probe.net-profit. 
 
{jcrep/r-ticket.i "new shared"}
{cecrep/jobtick.i "new shared"}

{cec/print4.i "new shared" "new shared"}
{cec/print42.i "new shared"}

{sys/inc/var.i "new shared"}

{custom/globdefs.i}

DEFINE BUFFER probe-ref FOR reftable.

DEFINE NEW SHARED VARIABLE k_frac AS DECIMAL INITIAL "6.25" NO-UNDO.
DEFINE NEW SHARED VARIABLE day_str AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE NEW SHARED VARIABLE tim_str AS CHARACTER FORMAT "x(8)" NO-UNDO.
DEFINE NEW SHARED VARIABLE maxpage AS INTEGER FORMAT ">9" NO-UNDO.
DEFINE NEW SHARED VARIABLE tmp-dir AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE col-norm AS CHARACTER INITIAL "White/Blue" NO-UNDO. 
DEFINE NEW SHARED VARIABLE qty AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE v-shared-rel AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE v-do-gsa LIKE do-gsa NO-UNDO.
DEFINE NEW SHARED BUFFER xop FOR est-op.


DEFINE NEW SHARED VARIABLE v-qtty LIKE qtty NO-UNDO.
DEFINE NEW SHARED VARIABLE v-drop-rc AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE v-update-qty-gsa AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE ld-gsa-brd AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE ld-gsa-mat AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE ld-gsa-lab AS DECIMAL NO-UNDO.

DEFINE VARIABLE v AS INTEGER NO-UNDO.
DEFINE VARIABLE vn-out LIKE ef.n-out-l INITIAL 1 NO-UNDO.
DEFINE VARIABLE v-outf AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-on-f AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-on-l AS DECIMAL NO-UNDO.
DEFINE VARIABLE sh-tmp LIKE sh-len NO-UNDO.
DEFINE VARIABLE v-widp AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-brd-only LIKE sys-ctrl.log-fld INITIAL NO NO-UNDO.
DEFINE VARIABLE v-brd-cost AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-module AS CHARACTER FORMAT "x(60)" NO-UNDO.
DEFINE NEW SHARED VARIABLE v-prep-mat LIKE tprep-mat NO-UNDO.  /* for probemk cost */
DEFINE NEW SHARED VARIABLE v-prep-lab LIKE tprep-lab NO-UNDO.
DEFINE VARIABLE v-bqty AS INTEGER NO-UNDO.
DEFINE VARIABLE v-gsa AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE ls-outfile AS CHARACTER NO-UNDO.
DEFINE VARIABLE ls-probetime AS CHARACTER NO-UNDO.  /* time display */
DEFINE VARIABLE v-tmp-int AS INTEGER NO-UNDO.
DEFINE VARIABLE v-can-update AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-orig-gp AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-orig-cm-pct AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-ceSellPrice AS CHARACTER NO-UNDO.

DEFINE NEW SHARED WORKFILE w-form
    FIELD form-no LIKE ef.form-no
    FIELD min-msf AS   LOGICAL INITIAL NO.

DEFINE TEMP-TABLE w-probeit LIKE probeit
    FIELD mat-cost   LIKE probe.mat-cost
    FIELD lab-cost   LIKE probe.lab-cost
    FIELD vo-cost    LIKE probe.vo-cost
    FIELD fo-cost    LIKE probe.fo-cost
    FIELD probe-date LIKE probe.probe-date.

DEFINE TEMP-TABLE q-sort NO-UNDO FIELD qty AS DECIMAL FIELD rel AS INTEGER.
DEFINE TEMP-TABLE q-sort1 NO-UNDO FIELD qty AS DECIMAL FIELD rel AS INTEGER.
DEFINE TEMP-TABLE q-sort2 NO-UNDO FIELD qty AS DECIMAL FIELD rel AS INTEGER.

DEFINE NEW SHARED TEMP-TABLE tt-qtty FIELD qtty LIKE qtty
                                  FIELD rel LIKE rels
                                  FIELD lRunShip LIKE lRunShips.

DEFINE TEMP-TABLE tt-bqty NO-UNDO FIELD tt-bqty AS INTEGER FIELD tt-brel AS INTEGER.

DEFINE TEMP-TABLE tt-probeit LIKE probeit
                          FIELD row-id AS ROWID.
DEFINE NEW SHARED TEMP-TABLE tt-est-op LIKE est-op.    
&SCOPED-DEFINE where-probeit WHERE probeit.company EQ probe.company ~
                               AND probeit.est-no  EQ probe.est-no  ~
                               AND probeit.line    EQ probe.line

DEFINE VARIABLE lv-override AS LOGICAL NO-UNDO. /* probe override or new creatation */
DEFINE VARIABLE module AS CHARACTER FORMAT "x(60)" NO-UNDO.
DEFINE VARIABLE lv-changed AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-fullc AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-gprof AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-nprof AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-price AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-brd-% AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-brdcm AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-brdc$ AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-comm AS CHARACTER NO-UNDO.
DEFINE VARIABLE hold-value AS CHARACTER NO-UNDO.
DEFINE VARIABLE ll-use-margin AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-prt-note AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-prt-box AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-from-dept AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-to-dept AS CHARACTER NO-UNDO.
DEFINE VARIABLE ll-no-valid AS LOGICAL NO-UNDO.
DEFINE VARIABLE lv-col-no AS INTEGER NO-UNDO.
DEFINE VARIABLE lv-int AS INTEGER NO-UNDO.
DEFINE VARIABLE lv-valid-profit AS CHARACTER NO-UNDO
    INITIAL "market-price,gross-profit,net-profit".

DEFINE NEW SHARED VARIABLE lv-cebrowse-dir AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE cCeBrowseBaseDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE cerunc-dec AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-dir AS CHARACTER FORMAT "X(80)" NO-UNDO.
DEFINE VARIABLE v-cestcalc AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-sort-by AS CHAR INIT "probe-date" NO-UNDO.
DEFINE VARIABLE lv-sort-by-lab AS CHAR INIT "Probe Date" NO-UNDO.
DEFINE VARIABLE ll-sort-asc AS LOG NO-UNDO.
DEFINE VARIABLE v-col-move AS LOGICAL INITIAL TRUE NO-UNDO.
DEFINE VARIABLE v-called-setCellColumns AS LOGICAL NO-UNDO.

{custom/xprint.i}

ASSIGN
 cocode = g_company
 locode = g_loc.
 
DEFINE VARIABLE retcode AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE lBussFormModle AS LOGICAL NO-UNDO.

 RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR. 

FIND FIRST sys-ctrl NO-LOCK WHERE
    sys-ctrl.company EQ cocode AND
    sys-ctrl.name    EQ "CESTCALC"
     NO-ERROR.

IF NOT AVAILABLE sys-ctrl THEN DO TRANSACTION:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = cocode
   sys-ctrl.name    = "CESTCALC"
   sys-ctrl.descrip = "Corrugated Estimate Calc"
   sys-ctrl.log-fld = NO
   sys-ctrl.char-fld = ""
   sys-ctrl.int-fld = 0.
END.
  v-cestcalc = sys-ctrl.char-fld.


RUN est/EstimateProcs.p (cocode, OUTPUT cCEBrowseBaseDir, OUTPUT tmp-dir ).

lv-cebrowse-dir = tmp-dir.


FIND FIRST users NO-LOCK WHERE
     users.user_id EQ USERID("ASI")
      NO-ERROR.

IF AVAILABLE users AND users.user_program[2] NE "" THEN
   v-dir = users.user_program[2] + "\".
ELSE
   v-dir = "c:\tmp\".

FIND FIRST company NO-LOCK WHERE company.company EQ cocode NO-ERROR.
FIND FIRST loc NO-LOCK WHERE loc.loc EQ locode NO-ERROR.

ASSIGN
   module = IF AVAILABLE company THEN company.NAME ELSE cocode
   module = module + " - " + IF AVAILABLE loc THEN loc.dscr ELSE locode.

DO TRANSACTION:
  {sys/inc/cerun.i C}
  
  ASSIGN
   do-speed  = sys-ctrl.log-fld
   vmclean   = sys-ctrl.char-fld NE ""
   vsuthrlnd = LOOKUP(sys-ctrl.char-fld,"Suthrlnd,Clevelnd,Brick") NE 0
   cerunc-dec = sys-ctrl.dec-fld.

  {sys/inc/cerun.i F}
  {sys/inc/cewhatif.i}
  {sys/inc/ceprint.i}
  {sys/inc/cepdies.i}
END.

{sys/inc/ceprepprice.i}
{sys/inc/ceprep.i}
{sys/inc/f16to32.i}

 &SCOPED-DEFINE for-each1    ~
    FOR EACH probe WHERE probe.company = eb.company and ~
    ASI.probe.est-no = eb.est-no ~
    AND probe.probe-date ne ? NO-LOCK

&SCOPED-DEFINE sortby-log                                                                                   ~
    IF lv-sort-by EQ "est-qty"         THEN STRING(9999999999.99 + probe.est-qty,"-9999999999.99")         ELSE ~
    IF lv-sort-by EQ "fact-cost"       THEN STRING(probe.fact-cost,"-9999999999.99999")                    ELSE ~
    IF lv-sort-by EQ "full-cost"       THEN STRING(probe.full-cost,"-9999999999.99999")                    ELSE ~
    IF lv-sort-by EQ "gross-profit"    THEN string(probe.gross-profit)                                     ELSE ~
    IF lv-sort-by EQ "grossProfitPctTemp"         THEN string(probe.grossProfitPctTemp)                                       ELSE ~
    IF lv-sort-by EQ "comm"            THEN string(probe.comm)                                             ELSE ~
    IF lv-sort-by EQ "net-profit"      THEN string(probe.net-profit)                                       ELSE ~
    IF lv-sort-by EQ "sell-price"      THEN string(probe.sell-price,"-9999999999.99")                      ELSE ~
    IF lv-sort-by EQ "gsh-qty"         THEN string(9999999999.99 + probe.gsh-qty,"-9999999999.99")         ELSE ~
    IF lv-sort-by EQ "do-quote"        THEN string(probe.do-quote)                                         ELSE ~
    IF lv-sort-by EQ "probe-date"      THEN STRING(INT(probe.probe-date),"9999999999")                     ELSE ~
    IF lv-sort-by EQ "boardCostPerM"   THEN string(probe.boardCostPerM)                                        ELSE ~
    IF lv-sort-by EQ "boardCostPct"    THEN string(probe.boardCostPct)                                        ELSE ~
    IF lv-sort-by EQ "boardContributionPerM"          THEN string(probe.boardContributionPerM)                                        ELSE ~
    IF lv-sort-by EQ "boardContributionTotal"          THEN string(probe.boardContributionTotal)                                        ELSE ~
    IF lv-sort-by EQ "probe-user"      THEN string(probe.probe-user)                                       ELSE ~
    IF lv-sort-by EQ "vtot-msf"        THEN string(vtot-msf())                                             ELSE ~
    IF lv-sort-by EQ "ls-probetime"    THEN string(cvt-time(probe.probe-time))                             ELSE ~
    IF lv-sort-by EQ "line"            THEN string(probe.LINE)                                             ELSE ~
    IF lv-sort-by EQ "spare-dec-1"      THEN string(probe.spare-dec-1)                                     ELSE ~
    IF lv-sort-by EQ "dMatPctSellPrice"    THEN string(fDirectMatPctSellPrice(1))                          ELSE ~
        STRING(INT(probe.probe-date),"9999999999")


  &SCOPED-DEFINE sortby BY probe.probe-date BY probe.est-qty 

&SCOPED-DEFINE sortby-phrase-asc  ~
    BY ({&sortby-log})            ~
    {&sortby}

&SCOPED-DEFINE sortby-phrase-desc ~
    BY ({&sortby-log}) DESC       ~
    {&sortby}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&SCOPED-DEFINE PROCEDURE-TYPE SmartBrowser
&SCOPED-DEFINE DB-AWARE NO

&SCOPED-DEFINE ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&SCOPED-DEFINE FRAME-NAME F-Main
&SCOPED-DEFINE BROWSE-NAME br_table
&SCOPED-DEFINE SVB SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
&SCOPED-DEFINE FVB FORMAT IN BROWSE {&BROWSE-NAME}

/* External Tables                                                      */
&SCOPED-DEFINE EXTERNAL-TABLES est ef eb
&SCOPED-DEFINE FIRST-EXTERNAL-TABLE est


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR est, ef, eb.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&SCOPED-DEFINE INTERNAL-TABLES probe

/* Define KEY-PHRASE in case it is used by any query. */
&SCOPED-DEFINE KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table probe.est-qty probe.fact-cost ~
probe.full-cost display-gp (1) @ probe.gross-profit ~
display-gp (1) @ probe.gross-profit probe.gross-profit probe.grossProfitPctTemp ~
probe.comm probe.net-profit probe.sell-price probe.gsh-qty probe.do-quote ~
voverall(1) @ voverall probe.probe-date probe.boardCostPerM probe.boardCostPct ~
probe.boardContributionPerM probe.boardContributionTotal probe.probe-user vtot-msf() @ vtot-msf ~
cvt-time(probe.probe-time) @ ls-probetime probe.line probe.spare-dec-1 ~
fDirectMatPctSellPrice(1) @ dMatPctSellPrice 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table probe.full-cost ~
probe.gross-profit probe.grossProfitPctTemp probe.net-profit ~
probe.sell-price probe.do-quote probe.boardCostPct probe.boardContributionPerM ~
probe.boardContributionTotal 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table probe reftable
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table probe
&Scoped-define QUERY-STRING-br_table FOR EACH probe WHERE probe.company = eb.company and ~
ASI.probe.est-no = eb.est-no ~
      AND probe.probe-date ne ? NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH probe WHERE probe.company = eb.company and ~
ASI.probe.est-no = eb.est-no ~
      AND probe.probe-date ne ? NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table probe
&Scoped-define FIRST-TABLE-IN-QUERY-br_table probe


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS  br_table 
&Scoped-Define DISPLAYED-OBJECTS fi_sort-by FI_moveCol

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
company||y|ASI.probe.company
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "company"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&SCOPED-DEFINE OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD checkNCBrd B-table-Win 
FUNCTION checkNCBrd RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD cvt-time B-table-Win 
FUNCTION cvt-time RETURNS CHARACTER
  ( INPUT ip-time AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-gp B-table-Win 
FUNCTION display-gp RETURNS DECIMAL
  ( INPUT ip-type AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fDirectMatPctSellPrice B-table-Win 
FUNCTION fDirectMatPctSellPrice RETURNS DECIMAL
  ( INPUT ip-type AS INTEGER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SatisfiedPDies B-table-Win 
FUNCTION SatisfiedPDies RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD voverall B-table-Win 
FUNCTION voverall RETURNS DECIMAL
  ( INPUT ip-type AS INTEGER )   FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD vtot-msf B-table-Win 
FUNCTION vtot-msf RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

DEFINE VARIABLE FI_moveCol AS CHARACTER FORMAT "X(4)":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE fi_sort-by AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      probe 
       SCROLLING.


/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      probe.est-qty FORMAT ">>>>>>>9":U COLUMN-FONT 0
      probe.fact-cost COLUMN-LABEL "Tot.Fact!Cost" FORMAT ">>,>>>,>>9.99":U
            WIDTH 19 COLUMN-FONT 0
      probe.full-cost FORMAT ">>,>>>,>>9.99":U WIDTH 19 COLUMN-FONT 0
      display-gp (1) @ probe.gross-profit
      display-gp (1) @ probe.gross-profit
      probe.gross-profit COLUMN-LABEL "Gross%" FORMAT "->>9.99":U
            COLUMN-FONT 0
      probe.grossProfitPctTemp COLUMN-LABEL "CM%" FORMAT "->>,>>9.99":U
            WIDTH 9.6
      probe.comm FORMAT "->>,>>9.99<<<":U
      probe.net-profit COLUMN-LABEL "Net%" FORMAT "->>9.99":U COLUMN-FONT 0
      probe.sell-price FORMAT ">>,>>>,>>9.99":U WIDTH 19 COLUMN-FONT 0
      probe.gsh-qty COLUMN-LABEL "Total!Sheets" FORMAT ">>>>>>9":U
            COLUMN-FONT 0
      probe.do-quote COLUMN-LABEL "Q" FORMAT "Y/N":U COLUMN-FONT 0
      voverall(1) @ voverall COLUMN-LABEL "Price!/BSF" WIDTH 19
            COLUMN-FONT 0
      probe.probe-date FORMAT "99/99/9999":U
      probe.boardCostPerM COLUMN-LABEL "Board/M" FORMAT "->,>>>,>>9.99":U
            WIDTH 17
      probe.boardCostPct COLUMN-LABEL "Board%" FORMAT "->>9.99":U
      probe.boardContributionPerM COLUMN-LABEL "Board!Contrib/M" FORMAT "->,>>>,>>9.99":U
            WIDTH 17
      probe.boardContributionTotal COLUMN-LABEL "Board!Contrib$" FORMAT "->>>,>>>,>>9.99":U
            WIDTH 19
      probe.probe-user COLUMN-LABEL "Probe By" FORMAT "X(8)":U
      vtot-msf() @ vtot-msf COLUMN-LABEL "Total!MSF" COLUMN-FONT 0
      cvt-time(probe.probe-time) @ ls-probetime COLUMN-LABEL "Time" FORMAT "x(8)":U
      probe.line FORMAT ">>9":U
      probe.boardCostTotal FORMAT "->>,>>>,>>9.99":U 
      probe.spare-dec-1 COLUMN-LABEL "Direct!Material" FORMAT "->>>,>>9.99":U
            WIDTH 15
      fDirectMatPctSellPrice(1) @ dMatPctSellPrice COLUMN-LABEL "Dir. Mat%"
  ENABLE
      probe.full-cost
      probe.gross-profit
      probe.grossProfitPctTemp
      probe.net-profit
      probe.sell-price
      probe.do-quote
      probe.boardContributionPerM
      probe.boardContributionTotal
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 143 BY 13.1
         FONT 0
         TITLE "Estimate  Analysis Per Thousand".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     
     br_table AT ROW 1 COL 1
     fi_sort-by AT ROW 14.20 COL 73 COLON-ALIGNED NO-LABEL
     "Browser Col. Mode:" VIEW-AS TEXT
          SIZE 22.6 BY .62 AT ROW 14.40 COL 107 WIDGET-ID 6
          FONT 6
     "Sort By:" VIEW-AS TEXT
          SIZE 9.4 BY 1 AT ROW 14.20 COL 65
          FONT 6
     FI_moveCol AT ROW 14.20 COL 128 COLON-ALIGNED NO-LABEL WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 0.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: ASI.est,ASI.ef,ASI.eb
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
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
         HEIGHT             = 13.14
         WIDTH              = 143.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       probe.comm:VISIBLE IN BROWSE br_table = FALSE
       probe.sell-price:AUTO-RESIZE IN BROWSE br_table = TRUE.


/* SETTINGS FOR FILL-IN FI_moveCol IN FRAME F-Main
   NO-ENABLE                                                            */

/* SETTINGS FOR FILL-IN fi_sort-by IN FRAME F-Main
   NO-ENABLE                                                            */


/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "ASI.probe WHERE ASI.eb <external> ..."
     _Options          = "NO-LOCK KEY-PHRASE"
     _TblOptList       = ", FIRST"
     _OrdList          = "ASI.probe.company|yes,ASI.probe.est-no|yes,ASI.probe.probe-date|yes,ASI.probe.est-qty|yes"
     _JoinCode[1]      = "probe.company = eb.company and
ASI.probe.est-no = ASI.eb.est-no"
     _Where[1]         = "ASI.probe.probe-date ne ?"
     _FldNameList[1]   > ASI.probe.est-qty
"probe.est-qty" ? ? "integer" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.probe.fact-cost
"probe.fact-cost" "Tot.Fact!Cost" ">>,>>>,>>9.99" "decimal" ? ? 0 ? ? ? no ? no no "19" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.probe.full-cost
"probe.full-cost" ? ">>,>>>,>>9.99" "decimal" ? ? 0 ? ? ? yes ? no no "19" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"display-gp (1) @ probe.gross-profit" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"display-gp (1) @ probe.gross-profit" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.probe.gross-profit
"probe.gross-profit" "Gross%" "->>9.99" "decimal" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.probe.grossProfitPctTemp
"probe.grossProfitPctTemp" "CM%" ? "decimal" ? ? ? ? ? ? yes ? no no "9.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.probe.comm
"probe.comm" ? ? "decimal" ? ? ? ? ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.probe.net-profit
"probe.net-profit" "Net%" "->>9.99" "decimal" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.probe.sell-price
"probe.sell-price" ? ">>,>>>,>>9.99" "decimal" ? ? 0 ? ? ? yes ? no no "19" yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.probe.gsh-qty
"probe.gsh-qty" "Total!Sheets" ">>>>>>9" "integer" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.probe.do-quote
"probe.do-quote" "Q" ? "logical" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"voverall(1) @ voverall" "Price!/BSF" ? ? ? ? 0 ? ? ? no ? no no "19" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   = ASI.probe.probe-date
     _FldNameList[15]   > ASI.probe.boardCostPerM
"probe.boardCostPerM" "Board/M" "->,>>>,>>9.99" "decimal" ? ? ? ? ? ? no ? no no "17" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.probe.boardCostPct
"probe.boardCostPct" "Board%" "->>9.99" "decimal" ? ? ? ? ? ? ? ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > ASI.probe.boardContributionPerM
"probe.boardContributionPerM" "Board!Contrib/M" "->,>>>,>>9.99" "decimal" ? ? ? ? ? ? yes ? no no "17" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ASI.probe.boardContributionTotal
"probe.boardContributionTotal" "Board!Contrib$" "->>>,>>>,>>9.99" "decimal" ? ? ? ? ? ? yes ? no no "19" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > ASI.probe.probe-user
"probe.probe-user" "Probe By" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"vtot-msf() @ vtot-msf" "Total!MSF" ? ? ? ? 0 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > "_<CALC>"
"cvt-time(probe.probe-time) @ ls-probetime" "Time" "x(8)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > ASI.probe.line
"probe.line" ? ">>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > ASI.probe.spare-dec-1
"probe.spare-dec-1" "Direct!Material" "->>>,>>9.99" "decimal" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > "_<CALC>"
"fDirectMatPctSellPrice(1) @ dMatPctSellPrice" "Dir. Mat%" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON DEFAULT-ACTION OF br_table IN FRAME F-Main /* Estimate  Analysis Per Thousand */
DO:
   IF v-can-update EQ NO THEN
      LEAVE.

   DEFINE VARIABLE phandle AS WIDGET-HANDLE NO-UNDO.
   DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.   
   RUN get-link-handle IN adm-broker-hdl
      (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
   phandle = WIDGET-HANDLE(char-hdl).
   
   RUN new-state IN phandle ('update-begin':U).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON RETURN OF br_table IN FRAME F-Main /* Estimate  Analysis Per Thousand */
ANYWHERE
DO:
   RUN GET-ATTRIBUTE ("FIELDS-ENABLED":U).
   IF RETURN-VALUE EQ "YES" THEN DO:  /* update mode */
      APPLY "tab" TO SELF.
      RETURN NO-APPLY.
   END.
   ELSE DO:
       APPLY "default-action" TO BROWSE {&browse-name}.
       RETURN NO-APPLY. 
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main /* Estimate  Analysis Per Thousand */
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main /* Estimate  Analysis Per Thousand */
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   /*{src/adm/template/brsleave.i} */
     {est/brsleave.i}   /* same but update will be LIKE add */
     
     IF KEYFUNCTION(LASTKEY) EQ "return" THEN REPOSITION {&browse-name} FORWARD 0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON START-SEARCH OF br_table IN FRAME F-Main
DO:
  DEF VAR lh-column AS HANDLE NO-UNDO.
  DEF VAR lv-column-nam AS CHAR NO-UNDO.
  DEF VAR lv-column-lab AS CHAR NO-UNDO.

  
  ASSIGN
   lh-column     = {&BROWSE-NAME}:CURRENT-COLUMN 
   lv-column-nam = lh-column:NAME
   lv-column-lab = lh-column:LABEL.

  IF lv-sort-by EQ lv-column-nam THEN ll-sort-asc = NOT ll-sort-asc.

  ELSE
    ASSIGN
     lv-sort-by     = lv-column-nam
     lv-sort-by-lab = lv-column-lab .

  APPLY 'END-SEARCH' TO {&BROWSE-NAME}.

  /*APPLY "choose" TO btn_go.*/
  RUN resort-query .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main /* Estimate  Analysis Per Thousand */
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME probe.full-cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL probe.full-cost br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF probe.full-cost IN BROWSE br_table /* Full!Cost */
DO:
  APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME probe.gross-profit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL probe.gross-profit br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF probe.gross-profit IN BROWSE br_table /* Gross% */
DO:

  v-orig-gp = probe.gross-profit:{&SVB}.
  IF ll-use-margin OR v-ceSellPrice EQ "F" THEN DO:
    APPLY "tab" TO probe.gross-profit IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL probe.gross-profit br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF probe.gross-profit IN BROWSE br_table /* Gross% */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-profit (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  IF v-orig-gp <> probe.gross-profit:{&SVB} 
      OR probe.grossProfitPctTemp:{&SVB} = "" THEN
    ASSIGN probe.grossProfitPctTemp:{&SVB} 
              = probe.gross-profit:{&SVB}.
    RUN calc-fields NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL probe.gross-profit br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF probe.gross-profit IN BROWSE br_table /* Gross% */
DO:
  lv-changed = "G".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME probe.grossProfitPctTemp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL probe.grossProfitPctTemp br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF probe.grossProfitPctTemp IN BROWSE br_table /* CM% */
DO:
    IF v-ceSellPrice NE "F" THEN DO:
      APPLY "tab" TO probe.grossProfitPctTemp IN BROWSE {&browse-name}.
      RETURN NO-APPLY.
    END.
    ELSE
        v-orig-cm-pct = probe.grossProfitPctTemp:{&SVB}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL probe.grossProfitPctTemp br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF probe.grossProfitPctTemp IN BROWSE br_table /* CM% */
DO:
    /*
  IF v-orig-cm-pct NE probe.grossProfitPctTemp:{&SVB} THEN
    probe.gross-profit:{&SVB} = 
      probe.grossProfitPctTemp:{&SVB}.
      */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL probe.grossProfitPctTemp br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF probe.grossProfitPctTemp IN BROWSE br_table /* CM% */
DO:
  IF v-ceSellPrice EQ "F" THEN
    lv-changed = "G".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME probe.net-profit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL probe.net-profit br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF probe.net-profit IN BROWSE br_table /* Net% */
DO:
  IF ll-use-margin THEN DO:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL probe.net-profit br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF probe.net-profit IN BROWSE br_table /* Net% */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-profit (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN calc-fields NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL probe.net-profit br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF probe.net-profit IN BROWSE br_table /* Net% */
DO:
  lv-changed = "N".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME probe.sell-price
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL probe.sell-price br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF probe.sell-price IN BROWSE br_table /* Selling!Price */
DO:
  DEFINE VARIABLE ld AS DECIMAL NO-UNDO.


  IF cewhatif-cha EQ "PerMSF" THEN DO:
    ASSIGN
     hold-value = {&self-name}:{&SVB}
     ld         = DECIMAL(hold-value).

    per-msf:
    DO ON ERROR UNDO, RETRY:
      RUN est/d-whatif.w (ROWID(probe), INPUT-OUTPUT ld) NO-ERROR.

      IF NOT ERROR-STATUS:ERROR THEN DO:
        IF ROUND(ld,2) NE ROUND(DECIMAL(hold-value),2) THEN DO:
          {&self-name}:{&SVB} = STRING(ld).
          RUN new-sell-price.
          RUN calc-fields NO-ERROR.
          IF ERROR-STATUS:ERROR THEN UNDO per-msf, RETRY per-msf.
        END.
      END.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL probe.sell-price br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF probe.sell-price IN BROWSE br_table /* Selling!Price */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN calc-fields NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL probe.sell-price br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF probe.sell-price IN BROWSE br_table /* Selling!Price */
DO:
  RUN new-sell-price.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME probe.boardCostPct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL probe.boardCostPct br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF probe.boardCostPct IN BROWSE br_table /* Board% */
DO:
  IF LASTKEY NE -1 THEN DO:
    
    RUN calc-fields NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL probe.boardCostPct br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF probe.boardCostPct IN BROWSE br_table /* Board% */
DO:
  lv-changed = "B".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME probe.boardContributionPerM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL probe.boardContributionPerM br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF probe.boardContributionPerM IN BROWSE br_table /* Board!Contrib/M */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN calc-fields NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL probe.boardContributionPerM br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF probe.boardContributionPerM IN BROWSE br_table /* Board!Contrib/M */
DO:
  lv-changed = "BCM".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME probe.boardContributionTotal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL probe.boardContributionTotal br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF probe.boardContributionTotal IN BROWSE br_table /* Board!Contrib$ */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN calc-fields NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL probe.boardContributionTotal br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF probe.boardContributionTotal IN BROWSE br_table /* Board!Contrib$ */
DO:
  lv-changed = "BC$".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br_table
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}

&SCOPED-DEFINE cellColumnDat probe

{methods/browsers/setCellColumns.i}

  FI_moveCol = "Sort".
  DISPLAY FI_moveCol WITH FRAME {&FRAME-NAME}.

&IF DEFINED(UIB_IS_RUNNING) NE 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

DEFINE VARIABLE lv-col-hand AS HANDLE.
FIND FIRST ce-ctrl {sys/look/ce-ctrlw.i} NO-LOCK.
IF AVAILABLE ce-ctrl THEN
  v-ceSellPrice = ce-ctrl.sell-by.
IF v-ceSellPrice NE "F" THEN DO lv-int = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME} 
    WITH FRAME f-main:

  lv-col-hand = BROWSE br_table:GET-BROWSE-COLUMN(lv-int).

  IF  lv-col-hand:LABEL EQ "cm%" THEN DO:
      lv-col-hand:VISIBLE = NO.

  END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win  adm/support/_adm-opn.p
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Opens different cases of the query based on attributes
               such as the 'Key-Name', or 'SortBy-Case'
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

  {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "est"}
  {src/adm/template/row-list.i "ef"}
  {src/adm/template/row-list.i "eb"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "est"}
  {src/adm/template/row-find.i "ef"}
  {src/adm/template/row-find.i "eb"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-fields B-table-Win 
PROCEDURE calc-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
  DEFINE VARIABLE ld-marg% AS DECIMAL NO-UNDO.
  DEFINE VARIABLE ld-commc AS DECIMAL NO-UNDO.
  DEFINE VARIABLE ld-factc AS DECIMAL NO-UNDO.
  DEFINE VARIABLE ld-fullc AS DECIMAL NO-UNDO.
  DEFINE VARIABLE ld-price AS DECIMAL NO-UNDO.
  DEFINE VARIABLE ld-brd-m AS DECIMAL NO-UNDO.
  DEFINE VARIABLE ld-brd-% AS DECIMAL NO-UNDO.
  DEFINE VARIABLE ld-brdcm AS DECIMAL NO-UNDO.
  DEFINE VARIABLE ld-brdc$ AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lv-changed2 LIKE lv-changed NO-UNDO.
  DEFINE VARIABLE v-tmp-set-markup LIKE probe.set-chg NO-UNDO.
  DEFINE VARIABLE v-tmp-value LIKE probe.set-chg NO-UNDO.
  DEFINE VARIABLE lv-orig-changed AS CHARACTER NO-UNDO.
  DEFINE VARIABLE v-freight AS DECIMAL NO-UNDO.

  {cec/combasis.i}

  {sys/inc/ceround.i}
  v-freight = 0.
  FOR EACH est-summ NO-LOCK
      WHERE est-summ.company EQ probe.company
        AND est-summ.est-no  EQ probe.est-no
        AND est-summ.e-num   EQ probe.line
        AND SUBSTR(est-summ.summ-tot,31) BEGINS "Freight"
      USE-INDEX est-qty :
      v-freight = v-freight + est-summ.per-m.
  END.
  FIND FIRST ce-ctrl {sys/look/ce-ctrlw.i} NO-LOCK.

  IF lv-changed NE "" THEN
  DO WITH FRAME {&FRAME-NAME}:

    IF est.est-type EQ 6 AND probe.set-chg NE 0 AND vmclean2 THEN
       v-tmp-set-markup = probe.set-chg.
       
    ASSIGN
        probe.boardCostPerM:{&SVB} = STRING(
                                        DEC(probe.boardCostTotal:{&SVB}) / 
                                        (DEC(probe.est-qty:{&SVB}) / 1000), 
                                            probe.boardCostPerM:{&FVB}
                                            )
        probe.boardCostPct:{&SVB} = STRING(
                                        DEC(probe.boardCostPerM:{&SVB}) / 
                                        (DEC(probe.sell-price:{&SVB}) * 100), 
                                            probe.boardCostPct:{&FVB}
                                            )
        probe.boardContributionPerM:{&SVB} = STRING(
                                                DEC(probe.sell-price:{&SVB}) - 
                                                DEC(probe.boardCostPerM:{&SVB}), 
                                                    probe.boardContributionPerM:{&FVB}
                                                    )
        probe.boardContributionTotal:{&SVB} = STRING(
                                                DEC(probe.boardContributionPerM:{&SVB}) * 
                                                (DEC(probe.est-qty:{&SVB}) / 1000), 
                                                    probe.boardContributionTotal:{&FVB}
                                                    )
        .

    ASSIGN
     lv-orig-changed = lv-changed
     v-com = probe.comm
     lv-changed2 = lv-changed
     ld-price    = DECIMAL(lv-price)
     ld-factc    = DECIMAL(probe.fact-cost:{&SVB})
     ld-commc    = (ld-price - (IF v-basis EQ "G" THEN ld-factc ELSE 0)) *
                   (v-com / 100)   
     ld-fullc    = DECIMAL(probe.full-cost:{&SVB}) -
                   ld-commc
     ld-brd-m    = DECIMAL(probe.boardCostPerM:{&SVB})
     ld-brd-%    = DECIMAL(probe.boardCostPct:{&SVB})
     ld-brdcm    = DECIMAL(probe.boardContributionPerM:{&SVB})
     ld-brdc$    = DECIMAL(probe.boardContributionTotal:{&SVB}).

    IF lv-changed EQ "S" THEN DO:
      ASSIGN
       ld-price = DECIMAL(probe.sell-price:{&SVB})
       ld-commc = (ld-price - (IF v-basis EQ "G" THEN ld-factc ELSE 0)) *
                  (v-com / 100).
        IF ld-price EQ 0 THEN DO:
            RETURN.
        END.

    END.
    ELSE DO:
      IF lv-changed EQ "BC$" THEN
        ld-price = (ld-brdc$ / (probe.est-qty / 1000)) + ld-brd-m.

      ELSE
      IF lv-changed EQ "BCM" THEN
        ld-price = ld-brdcm + ld-brd-m.

      ELSE
      IF lv-changed EQ "B" THEN
        ld-price = ld-brd-m / (ld-brd-% / 100).

      ELSE DO:
        IF lv-changed EQ "G" THEN DO:
          IF ce-ctrl.sell-by EQ "F" THEN DO:
          
            IF  DECIMAL(probe.grossProfitPctTemp:{&SVB}) > 0
               THEN
              v-pct = DECIMAL(probe.grossProfitPctTemp:{&SVB}).
            ELSE IF v-pct EQ 0 THEN
              v-pct = DECIMAL(probe.gross-profit:{&SVB}).
            IF DECIMAL(probe.grossProfitPctTemp:{&SVB}) = 0 THEN
                probe.grossProfitPctTemp:{&SVB} = STRING(v-pct).
          END.
          ELSE
            v-pct = DECIMAL(probe.gross-profit:{&SVB}).

          IF ce-ctrl.sell-by EQ "S" THEN lv-changed2 = "S".
          IF ce-ctrl.sell-by EQ "F" THEN lv-changed2 = "F".
        END.
      
        ELSE v-pct = DECIMAL(probe.net-profit:{&SVB}).

        IF ll-use-margin THEN
          ASSIGN
           v-com       = 0
           lv-changed2 = "N"
           v-pct       = IF lv-changed EQ "M" THEN ld-marg% ELSE (v-pct + v-com) .
        RUN custom/sellpric.p ("",
                               lv-changed2,
                               v-basis,
                               ld-factc,
                               IF lv-changed2 NE "F" THEN ld-fullc - ld-factc ELSE v-freight,
                               v-com,
                               v-pct + v-tmp-set-markup,
                               OUTPUT ld-price,
                               OUTPUT ld-commc).
      END.

      IF v-round NE "PENNY" THEN DO:
        IF v-round EQ "DOLLAR" THEN DO:
          {sys/inc/roundup.i ld-price}
        END.
        lv-changed = "".
      END.
    END.

    ld-marg% = ROUND((ld-price - ld-fullc) / ld-price * 100,2).

    IF est.est-type EQ 6 AND probe.set-chg NE 0 AND vmclean2 THEN
       ld-marg% = ld-marg% - v-tmp-set-markup.
    
    IF ll-use-margin THEN DO:  /* Get Commission% */
      RUN est/getsmanmtrx.p (ROWID(est), "C",
                             INPUT-OUTPUT v-com,
                             INPUT-OUTPUT ld-marg%).

      ld-commc = ld-price * v-com / 100.
    END.

    IF est.est-type EQ 6 AND probe.set-chg NE 0 AND vmclean2 AND
       NOT(lv-changed NE "M" AND lv-orig-changed NE "M") THEN
       ld-marg% = ld-marg% + v-tmp-set-markup.

    ASSIGN
     ld-brd-% = ld-brd-m / ld-price * 100
     ld-brdcm = ld-price - ld-brd-m
     ld-brdc$ = ld-brdcm * (probe.est-qty / 1000)
     ld-fullc = ld-fullc + ld-commc.

    probe.full-cost:{&SVB} =
       STRING(ld-fullc,probe.full-cost:{&FVB}) NO-ERROR.

    IF lv-changed NE "BC$" AND NOT ERROR-STATUS:ERROR THEN
      probe.boardContributionTotal:{&SVB} =
          STRING(ld-brdc$,probe.boardContributionTotal:{&FVB}) NO-ERROR.

    IF lv-changed NE "BCM" AND NOT ERROR-STATUS:ERROR THEN
      probe.boardContributionPerM:{&SVB} =
          STRING(ld-brdcm,probe.boardContributionPerM:{&FVB}) NO-ERROR.

    IF lv-changed NE "B" AND NOT ERROR-STATUS:ERROR THEN
      probe.boardCostPct:{&SVB} =
          STRING(ld-brd-%,probe.boardCostPct:{&FVB}) NO-ERROR.

    
    IF lv-changed NE "S" AND NOT ERROR-STATUS:ERROR THEN
      probe.sell-price:{&SVB} =
          STRING(ld-price,probe.sell-price:{&FVB}) NO-ERROR.
        
    IF lv-changed NE "N" AND NOT ERROR-STATUS:ERROR THEN
    DO:
      IF probe.set-chg NE 0 AND est.est-type EQ 6 AND vmclean2 THEN
         probe.net-profit:{&SVB} =
             STRING((1 - (ld-fullc / ld-price)) * 100 - probe.set-chg) NO-ERROR. 
      ELSE
         probe.net-profit:{&SVB} =
             STRING((1 - (ld-fullc / ld-price)) * 100) NO-ERROR.
    END.

    IF lv-changed NE "G" AND NOT ERROR-STATUS:ERROR THEN
      probe.gross-profit:{&SVB} =
          STRING((1 - (ld-factc / ld-price)) * 100) NO-ERROR.

    ASSIGN
       probe.comm:{&SVB} =
                  STRING(v-com,probe.comm:{&FVB})
       lv-changed = lv-changed2.

    IF ERROR-STATUS:ERROR                                                    OR
       TRIM(probe.full-cost:{&SVB})    EQ "?" OR
       TRIM(probe.gross-profit:{&SVB}) EQ "?" OR
       TRIM(probe.net-profit:{&SVB})   EQ "?" OR
       TRIM(probe.sell-price:{&SVB})   EQ "?" OR
       TRIM(probe.boardCostPct:{&SVB})    EQ "?" OR
       TRIM(probe.boardContributionPerM:{&SVB})    EQ "?" OR
       TRIM(probe.boardContributionTotal:{&SVB})    EQ "?" OR
       ld-price GT 99999999.99                                              THEN DO:

      MESSAGE "Value(s) invalid, please try again" VIEW-AS ALERT-BOX ERROR.

      ASSIGN
       probe.full-cost:{&SVB}    = lv-fullc
       probe.net-profit:{&SVB}   = lv-nprof
       probe.gross-profit:{&SVB} = lv-gprof
       probe.sell-price:{&SVB}   = lv-price
       probe.boardCostPct:{&SVB}    = lv-brd-%
       probe.boardContributionPerM:{&SVB}    = lv-brdcm
       probe.boardContributionTotal:{&SVB}    = lv-brdc$
       probe.comm:{&SVB} = lv-comm.

      IF lv-changed EQ "BC$" THEN
        APPLY "entry" TO probe.boardContributionTotal IN BROWSE {&browse-name}.
      ELSE
      IF lv-changed EQ "BCM" THEN
        APPLY "entry" TO probe.boardContributionPerM IN BROWSE {&browse-name}.
      ELSE
      IF lv-changed EQ "B" THEN
        APPLY "entry" TO probe.boardCostPct IN BROWSE {&browse-name}.
      ELSE
      IF lv-changed EQ "S" THEN
        APPLY "entry" TO probe.sell-price IN BROWSE {&browse-name}.
      ELSE
      IF lv-changed EQ "N" THEN
        APPLY "entry" TO probe.net-profit IN BROWSE {&browse-name}.
      ELSE
        APPLY "entry" TO probe.gross-profit IN BROWSE {&browse-name}.

      lv-changed = "".

      RETURN ERROR.
    END.

    DO WITH FRAME {&FRAME-NAME}:
      voverall:{&SVB} = STRING(voverall (0)).
      dMatPctSellPrice:{&SVB} = STRING(fDirectMatPctSellPrice(2)).
      IF lv-changed2 NE "S" THEN 
        probe.gross-profit:{&SVB} = STRING(display-gp (0)).
    END.
    RUN save-fields.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-for-combo B-table-Win 
PROCEDURE check-for-combo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER op-combo AS LOGICAL NO-UNDO.
  

  op-combo = AVAILABLE est AND est.est-type EQ 8.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-mclean B-table-Win 
PROCEDURE create-mclean :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE li AS INTEGER NO-UNDO.
  DEFINE VARIABLE lv-summ-tot LIKE est-summ.summ-tot NO-UNDO.
  
  
  FOR EACH mclean:
    DELETE mclean.
  END.

  FOR EACH probe NO-LOCK
      WHERE probe.company    EQ est.company
        AND probe.est-no     EQ est.est-no
        AND probe.probe-date NE ?:

    FIND FIRST est-summ NO-LOCK
        WHERE est-summ.company EQ probe.company
          AND est-summ.est-no  EQ probe.est-no
          AND est-summ.e-num   EQ probe.line
        USE-INDEX est-qty  NO-ERROR.
      
    DO WHILE AVAILABLE est-summ:
      CREATE mclean.
      ASSIGN
       mclean.rec-type = SUBSTRING(est-summ.summ-tot,01,20)
       mclean.form-no  = INTEGER(SUBSTRING(est-summ.summ-tot,21,10))
       mclean.descr    = SUBSTRING(est-summ.summ-tot,31).

      DO li = 1 TO 28:
        mclean.cost[li] = est-summ.per-m.

        FIND NEXT est-summ NO-LOCK
            WHERE est-summ.company EQ probe.company
              AND est-summ.est-no  EQ probe.est-no
              AND est-summ.e-num   EQ probe.line
            USE-INDEX est-qty  NO-ERROR.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-quote B-table-Win 
PROCEDURE create-quote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER xprobe FOR probe.
DEFINE BUFFER bf-qhd FOR quotehd.
DEFINE BUFFER bf-notes FOR notes.

/* generating quote record - main code from  quote.a with new table */

DEFINE VARIABLE j AS INTEGER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

DEFINE BUFFER bf-eb FOR eb.
DEFINE BUFFER bf-ef FOR ef.
DEFINE BUFFER b-probemk FOR reftable.

DEFINE VARIABLE li-q-no LIKE quotehd.q-no NO-UNDO.
DEFINE VARIABLE li-line AS INTEGER NO-UNDO .  /* for quoteitm.line */
DEFINE VARIABLE ll-new-quote AS LOGICAL NO-UNDO.
DEFINE VARIABLE ll-first AS LOGICAL NO-UNDO.
DEFINE VARIABLE li-first-qty AS INTEGER NO-UNDO.  /* first qty for quoteitm */
DEFINE VARIABLE li-prep-qty LIKE quotechg.prep-qty NO-UNDO.
DEFINE VARIABLE li-cnt AS INTEGER NO-UNDO.
DEFINE VARIABLE ld-cost AS DECIMAL NO-UNDO.
DEFINE VARIABLE li-value AS INTEGER NO-UNDO.
DEFINE VARIABLE lv-quo-price-char AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-tot-mat AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-tot-lab AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-nk-found AS LOGICAL NO-UNDO.
DEFINE VARIABLE lv-cust LIKE eb.cust-no NO-UNDO.
DEFINE VARIABLE cNotes LIKE quotehd.comment NO-UNDO.
{est/checkuse.i}

SESSION:SET-WAIT-STATE("general").

DISABLE TRIGGERS FOR LOAD OF quoteqty.

IF CAN-FIND(FIRST xprobe
            WHERE xprobe.company EQ probe.company
              AND xprobe.est-no  EQ probe.est-no
              AND xprobe.do-quote) THEN DO:
    /*
  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "QUOPRICE"
      NO-LOCK NO-ERROR.
  lv-quo-price-char = IF AVAILABLE sys-ctrl THEN sys-ctrl.char-fld ELSE "M". */
  FIND FIRST bf-eb NO-LOCK WHERE bf-eb.company EQ cocode
                     AND bf-eb.est-no  EQ est.est-no
                     AND bf-eb.cust-no GT ""
                    NO-ERROR.
  IF AVAILABLE bf-eb THEN
      lv-cust = bf-eb.cust-no.
  ELSE
      lv-cust = "".
  RUN sys/ref/nk1look.p    (INPUT cocode,
                            INPUT "QUOPRICE", /* N-K-1 name */
                            INPUT "C", /* data type */
                            INPUT YES, /* use customer values */
                            INPUT YES, /* shipto or vendor */
                            INPUT lv-cust,
                            INPUT "", /* shipto value */
                            OUTPUT lv-quo-price-char, /* returned value */
                            OUTPUT v-nk-found /* was it found? */).
  IF NOT v-nk-found THEN
    lv-quo-price-char = "M".

  IF est.est-type EQ 8 THEN DO:
    FIND FIRST xjob NO-ERROR.
    IF NOT AVAILABLE xjob THEN DO:
      MESSAGE "You must calculate a combo estimate before creating a quote..."
              VIEW-AS ALERT-BOX ERROR.
      RETURN.
    END.
  END.

  ASSIGN
   cocode = est.company
   locode = est.loc.

  FIND LAST quotehd WHERE quotehd.company EQ est.company AND
                                 quotehd.loc EQ est.loc AND
                                 quotehd.est-no EQ est.est-no NO-ERROR.
  IF AVAILABLE quotehd THEN DO:
     DEFINE VARIABLE li-choice AS INTEGER NO-UNDO.
     RUN est/d-qtcnfm.w (OUTPUT li-choice).
     IF li-choice EQ 1 THEN /*update */ ll-new-quote = NO.
     ELSE IF li-choice EQ 2 THEN /* new */ ll-new-quote = YES.
     ELSE RETURN. /* cancel */  
  END.
  ELSE ll-new-quote = YES.
  li-q-no = IF AVAILABLE quotehd THEN quotehd.q-no ELSE 1.

  FOR EACH w-probeit:
    DELETE w-probeit.
  END.

  i = 0.

  {cec/probeqt.i}  /* size limitation */

  RUN delete-old-part (li-q-no).

  FOR EACH w-probeit
      BREAK BY w-probeit.cust-no
            BY w-probeit.part-no
            BY w-probeit.bl-qty:

    FIND FIRST bf-eb NO-LOCK
        WHERE bf-eb.company EQ w-probeit.company
          AND bf-eb.est-no  EQ w-probeit.est-no
          AND bf-eb.cust-no EQ w-probeit.cust-no
          AND bf-eb.part-no EQ w-probeit.part-no
          AND bf-eb.form-no NE 0
         NO-ERROR.

    IF NOT AVAILABLE bf-eb THEN DO:
      FIND FIRST bf-eb NO-LOCK
          WHERE bf-eb.company EQ w-probeit.company
            AND bf-eb.est-no  EQ w-probeit.est-no
            AND bf-eb.form-no EQ 0
           NO-ERROR.
      FIND FIRST eb NO-LOCK
          WHERE eb.company EQ w-probeit.company
            AND eb.est-no  EQ w-probeit.est-no
            AND eb.form-no NE 0
           NO-ERROR.
    END.

    FIND FIRST quotehd
        WHERE quotehd.company EQ cocode
          AND quotehd.loc     EQ locode
          AND quotehd.q-no    EQ li-q-no
        NO-ERROR.

    IF FIRST-OF(w-probeit.cust-no) THEN DO:
      IF ll-new-quote OR NOT FIRST(w-probeit.cust-no) OR NOT AVAILABLE quotehd THEN DO:
        CREATE quotehd.
        quotehd.quo-date = TODAY.
        
      END.

      ASSIGN
       li-q-no            = quotehd.q-no  /* from create trigger */
       quotehd.e-num      = est.e-num
       quotehd.est-no     = est.est-no
       quotehd.cust-no    = bf-eb.cust-no
       quotehd.ship-no    = bf-eb.ship-no
       quotehd.ship-id    = bf-eb.ship-id
       quotehd.sold-no    = 1
       quotehd.part-dscr1 = bf-eb.part-dscr1
       quotehd.upd-date   = TODAY
       quotehd.quo-date   = TODAY
       quotehd.upd-user   = USERID("ASI").

      {custom/getrfq.i}
      
      FIND FIRST cust
          {sys/look/custW.i}
            AND cust.cust-no EQ quotehd.cust-no
          NO-LOCK NO-ERROR.
      FIND FIRST shipto NO-LOCK
          WHERE shipto.company EQ cocode
            AND shipto.cust-no EQ quotehd.cust-no
            AND shipto.ship-id EQ quotehd.ship-id
           NO-ERROR.
      FOR EACH soldto NO-LOCK
          WHERE soldto.company EQ cocode
            AND soldto.cust-no EQ quotehd.cust-no
          
          BREAK BY soldto.sold-no DESCENDING:
        IF soldto.sold-no EQ 1 OR LAST-OF(soldto.sold-no) THEN LEAVE.
      END.

      ASSIGN
       quotehd.sman      = eb.sman  /* bf-eb.sman */
       quotehd.carrier   = eb.carrier  /*bf-eb.carrier */
       quotehd.del-zone  = eb.dest-code  /* bf-eb.dest-code */
       quotehd.terms     = cust.terms
       quotehd.contact   = cust.contact.

      IF cust.cust-no EQ "TEMP" THEN
        ASSIGN
         quotehd.shipto[1] = eb.ship-name
         quotehd.shipto[2] = eb.ship-addr[1]
         quotehd.shipto[3] = eb.ship-addr[2]
         quotehd.shipto[4] = eb.ship-city + ", " + eb.ship-state +
                             " " + eb.ship-zip
         quotehd.billto[1] = quotehd.shipto[1]
         quotehd.billto[2] = quotehd.shipto[2]
         quotehd.billto[3] = quotehd.shipto[3]
         quotehd.billto[4] = quotehd.shipto[4] 
         quotehd.soldto[1] = quotehd.shipto[1]
         quotehd.soldto[2] = quotehd.shipto[2]
         quotehd.soldto[3] = quotehd.shipto[3]
         quotehd.soldto[4] = quotehd.shipto[4].

      ELSE
        ASSIGN
         quotehd.billto[1] = cust.name
         quotehd.billto[2] = cust.addr[1]
         quotehd.billto[3] = cust.addr[2]
         quotehd.billto[4] = cust.city + ", " + cust.state + " " + cust.zip
         quotehd.shipto[1] = shipto.ship-name
         quotehd.shipto[2] = shipto.ship-addr[1]
         quotehd.shipto[3] = shipto.ship-addr[2]
         quotehd.shipto[4] = shipto.ship-city + ", " + shipto.ship-state +
                             " " + shipto.ship-zip
         quotehd.soldto[1] = soldto.sold-name
         quotehd.soldto[2] = soldto.sold-addr[1]
         quotehd.soldto[3] = soldto.sold-addr[2]
         quotehd.soldto[4] = soldto.sold-city + ", " + soldto.sold-state +
                             " " + soldto.sold-zip.

       /* copy notes from old quotehd */
      IF ll-new-quote THEN DO:
          RUN est/GetQuoteDefNotes.p (INPUT quotehd.company,
                                      OUTPUT cNotes).
/*          FIND FIRST bf-qhd NO-LOCK NO-ERROR. */
/*          IF AVAILABLE bf-qhd THEN                */
             ASSIGN  quotehd.comment[1] = cNotes[1]
                     quotehd.comment[2] = cNotes[2]
                     quotehd.comment[3] = cNotes[3]
                     quotehd.comment[4] = cNotes[4]
                     quotehd.comment[5] = cNotes[5].
      END.

      IF est.est-type GE 7 THEN DO:

        FOR EACH quotechg OF quotehd:
          DELETE quotechg.
        END.
      END.
    END.

    FIND FIRST quoteitm
        WHERE quoteitm.company EQ quotehd.company
          AND quoteitm.loc     EQ quotehd.loc
          AND quoteitm.q-no    EQ quotehd.q-no
          AND quoteitm.part-no EQ w-probeit.part-no
        NO-ERROR.

    IF FIRST-OF(w-probeit.part-no) THEN DO:
      FIND FIRST bf-ef NO-LOCK
          WHERE bf-ef.company  EQ est.company
            AND bf-ef.est-no   EQ est.est-no
            AND (bf-ef.form-no EQ bf-eb.form-no OR est.est-type EQ 6)
           NO-ERROR.

      IF ll-new-quote OR NOT AVAILABLE quoteitm THEN DO:
        FOR EACH quoteitm NO-LOCK
            WHERE quoteitm.company EQ quotehd.company
              AND quoteitm.loc     EQ quotehd.loc
              AND quoteitm.q-no    EQ quotehd.q-no
            
            BY quoteitm.line DESCENDING:
          li-line = quoteitm.line + 1.
          LEAVE.
        END.
        CREATE quoteitm.
      END.
      ELSE li-line = quoteitm.line.

      ASSIGN
       quoteitm.company    = quotehd.company
       quoteitm.loc        = quotehd.loc
       quoteitm.q-no       = quotehd.q-no
       quoteitm.est-no     = quotehd.est-no
       quoteitm.line       = li-line
       quoteitm.style      = bf-eb.style
       quoteitm.part-no    = bf-eb.part-no
       quoteitm.part-dscr1 = bf-eb.part-dscr1
       quoteitm.part-dscr2 = bf-eb.part-dscr2
       quoteitm.i-coldscr  = IF est.est-type EQ 6 THEN eb.i-coldscr
                                                  ELSE bf-eb.i-coldscr
       quoteitm.i-dscr     = bf-ef.brd-dscr
       quoteitm.qty        = w-probeit.bl-qty
       quoteitm.uom        = lv-quo-price-char
       quoteitm.price      = w-probeit.sell-price
       quoteitm.upd-date   = TODAY
       quoteitm.upd-user   = USERID("ASI")
       /*RCO400 only */
       quoteitm.i-no       = bf-eb.stock-no.

      RUN sys/inc/calcsize.p (IF est.est-type EQ 6 THEN ROWID(bf-eb) ELSE ROWID(eb),
                              OUTPUT quoteitm.size).

      IF quoteitm.uom NE "M" THEN
        RUN sys/ref/convcuom.p("M",quoteitm.uom, 0, 0, 0, 0,
                               quoteitm.price, OUTPUT quoteitm.price).
   
      IF bf-ef.brd-dscr EQ '' THEN DO:
        FIND FIRST ITEM NO-LOCK
            WHERE item.company EQ cocode
              AND item.i-no    EQ bf-ef.board
             NO-ERROR.
        IF AVAILABLE ITEM THEN
          quoteitm.i-dscr = IF item.i-name   GT "" THEN item.i-name   ELSE
                            IF item.est-dscr GT "" THEN item.est-dscr ELSE
                            item.i-dscr.
      END. /* IF brd-dscr */
    END.

    FIND FIRST quoteqty
        WHERE quoteqty.company EQ quoteitm.company
          AND quoteqty.loc     EQ quoteitm.loc
          AND quoteqty.q-no    EQ quoteitm.q-no
          AND quoteqty.LINE    EQ quoteitm.line
          AND quoteqty.qty     EQ w-probeit.bl-qty
          AND quoteqty.rels    EQ INTEGER(w-probeit.freight)
        NO-ERROR.

    IF ll-new-quote OR NOT AVAILABLE quoteqty THEN CREATE quoteqty.

    ASSIGN
     quoteqty.company    = quoteitm.company
     quoteqty.loc        = quoteitm.loc
     quoteqty.q-no       = quoteitm.q-no
     quoteqty.line       = quoteitm.line
     quoteqty.qty        = w-probeit.bl-qty
     quoteqty.uom        = lv-quo-price-char
     quoteqty.price      = w-probeit.sell-price
     quoteqty.rels       = w-probeit.freight
     quoteqty.quote-date = /*IF ll-new-quote THEN TODAY ELSE */ w-probeit.probe-date
     quoteqty.quote-user = USERID("ASI")
     quoteqty.prof-on    = w-probeit.prof-on
     quoteqty.mat-cost   = w-probeit.mat-cost
     quoteqty.lab-cost   = w-probeit.lab-cost
     quoteqty.vo-cost    = w-probeit.vo-cost
     quoteqty.fo-cost    = w-probeit.fo-cost
     quoteqty.tot-lbs    = w-probeit.tot-lbs
     quoteqty.profit     = IF w-probeit.prof-on EQ "Net" THEN w-probeit.net-profit
                                                         ELSE w-probeit.gross-profit.

    IF quoteqty.uom NE "M" THEN
      RUN sys/ref/convcuom.p("M",quoteqty.uom, 0, 0, 0, 0,
                             quoteqty.price, OUTPUT quoteqty.price).

    /* update rfqitem qty - start */
    &SCOPED-DEFINE getrfq
    {custom/rfq-qty.i}
    /* update rfqitem qty - end */

    IF LAST-OF(w-probeit.cust-no) OR est.est-type LT 7 THEN DO:
      li-first-qty = IF est.est-type GE 7 THEN 0 ELSE w-probeit.bl-qty.

      FOR EACH quotechg
          WHERE quotechg.company EQ quoteqty.company
            AND quotechg.loc     EQ quoteqty.loc
            AND quotechg.q-no    EQ quoteqty.q-no
            AND ((quotechg.line  EQ quoteqty.line AND li-first-qty NE 0) OR
                 quotechg.line   EQ 0)
            AND quotechg.qty     EQ li-first-qty:
        DELETE quotechg.
      END.

      FOR EACH bf-ef NO-LOCK
          WHERE bf-ef.company EQ quotehd.company
            AND bf-ef.est-no  EQ quotehd.est-no:

        li-prep-qty = 0.

        IF est.est-type GE 7 THEN
        FOR EACH bf-eb FIELDS(bl-qty) NO-LOCK
            WHERE bf-eb.company EQ bf-ef.company
              AND bf-eb.est-no  EQ bf-ef.est-no
              AND bf-eb.form-no EQ bf-ef.form-no
              AND bf-eb.cust-no EQ w-probeit.cust-no:
          li-prep-qty = li-prep-qty + bf-eb.bl-qty.
        END.

        ELSE li-prep-qty = w-probeit.bl-qty.

        FOR EACH est-prep NO-LOCK
            WHERE est-prep.company EQ bf-ef.company
              AND est-prep.est-no  EQ bf-ef.est-no
              AND est-prep.s-num   EQ bf-ef.form-no
              AND est-prep.simon   EQ "S":

          CREATE quotechg.
          ASSIGN
           quotechg.company  = quoteqty.company
           quotechg.loc      = quoteqty.loc
           quotechg.q-no     = quoteqty.q-no
           quotechg.line     = IF li-first-qty EQ 0 THEN 0 ELSE quoteqty.line
           quotechg.qty      = li-first-qty
           quotechg.quote-date = quoteqty.quote-date
           quotechg.CODE     = est-prep.CODE
           quotechg.charge   = est-prep.dscr
           quotechg.bill     = IF est-prep.ml THEN "M" ELSE "L"
           quotechg.amt      = IF ceprepprice-chr EQ "Profit" THEN
                                  est-prep.qty * est-prep.cost /
                                  (1 - (est-prep.mkup / 100)) *
                                  (IF est-prep.amtz NE 0 THEN est-prep.amtz / 100 ELSE 1)
                               ELSE /*Cost Markup*/
                                  est-prep.qty * est-prep.cost *
                                  (1 + (est-prep.mkup / 100)) *
                                  (IF est-prep.amtz NE 0 THEN est-prep.amtz / 100 ELSE 1)  
           quotechg.mkup     = est-prep.mkup
           quotechg.spare-dec-1    = est-prep.spare-dec-1
           quotechg.cost     = est-prep.cost
           quotechg.amtz     = est-prep.amtz
           quotechg.prep-qty = est-prep.qty
           quotechg.s-num    = est-prep.s-num
           quotechg.b-num    = est-prep.b-num
           quotechg.simon    = est-prep.simon. 

          IF ceprep-cha EQ "Dollar" THEN DO:
             {sys/inc/roundup.i quotechg.amt}
          END.
          ELSE IF ceprep-cha EQ "FiveDollar" THEN DO:
             {sys/inc/roundupfive.i quotechg.amt}
          END.
        END.

        DO j = 1 TO 6:
          IF bf-ef.mis-simon[j] EQ "S" AND bf-ef.mis-cost[j] NE "" THEN DO:
            CREATE quotechg.

            IF (bf-ef.mis-labf[j] NE 0 OR bf-ef.mis-labm[j] NE 0) AND
               (bf-ef.mis-matf[j] EQ 0 AND bf-ef.mis-matm[j] EQ 0) THEN
              quotechg.bill = "L".
            ELSE
            IF (bf-ef.mis-labf[j] EQ 0 AND bf-ef.mis-labm[j] EQ 0) AND
               (bf-ef.mis-matf[j] NE 0 OR bf-ef.mis-matm[j] NE 0) THEN
              quotechg.bill = "M".
            ELSE
            IF (bf-ef.mis-labf[j] NE 0 OR bf-ef.mis-labm[j] NE 0) OR
               (bf-ef.mis-matf[j] NE 0 OR bf-ef.mis-matm[j] NE 0) THEN
              quotechg.bill = "T".

            ASSIGN
             quotechg.company  = quoteqty.company
             quotechg.loc      = quoteqty.loc
             quotechg.q-no     = quoteqty.q-no
             quotechg.line     = IF li-first-qty EQ 0 THEN 0 ELSE quoteqty.line
             quotechg.qty      = li-first-qty
             quotechg.quote-date = quoteqty.quote-date
             quotechg.prep-qty = li-prep-qty
             quotechg.s-num    = bf-ef.mis-snum[j]
             quotechg.b-num    = bf-ef.mis-bnum[j]
             quotechg.charge   = bf-ef.mis-cost[j]
             quotechg.labf     = bf-ef.mis-labf[j]
             quotechg.matf     = bf-ef.mis-matf[j]
             quotechg.labm     = bf-ef.mis-labm[j]
             quotechg.matm     = bf-ef.mis-matm[j]
             quotechg.mkup     = bf-ef.mis-mkup[j]
             quotechg.simon    = bf-ef.mis-simon[j]
             quotechg.amtz     = 100.
            
            {est/qt-misc.i "MAT" j}
            quotechg.matm = ld-cost.
            {est/qt-misc.i "LAB" j}

            ASSIGN
             quotechg.labm = ld-cost.

            IF ceprepprice-chr EQ "Profit" THEN
               ASSIGN
                  v-tot-mat = (quotechg.matf + (quotechg.matm * (quotechg.prep-qty / 1000))) /
                              (1 - (quotechg.mkup / 100))
                  v-tot-lab = (quotechg.labf + (quotechg.labm * (quotechg.prep-qty / 1000))) /
                              (1 - (quotechg.mkup / 100)).
            ELSE
               ASSIGN
                  v-tot-mat = (quotechg.matf + (quotechg.matm * (quotechg.prep-qty / 1000))) *
                              (1 + (quotechg.mkup / 100))
                  v-tot-lab = (quotechg.labf + (quotechg.labm * (quotechg.prep-qty / 1000))) *
                              (1 + (quotechg.mkup / 100)).

            quotechg.amt = v-tot-mat + v-tot-lab.
            
            IF quotechg.prep-qty NE 0 THEN
               quotechg.cost = ((quotechg.matf + (quotechg.matm * (quotechg.prep-qty / 1000)))
                             + (quotechg.labf + (quotechg.labm * (quotechg.prep-qty / 1000)))) / quotechg.prep-qty.
            

            IF ceprep-cha EQ "Dollar" THEN DO:
               {sys/inc/roundup.i quotechg.amt}
            END.
            ELSE IF ceprep-cha EQ "FiveDollar" THEN DO:
               {sys/inc/roundupfive.i quotechg.amt}
            END.
          END.
        END.
      END.
    END.

    DELETE w-probeit.
  END.  /* each w-probeit */
END.

IF AVAILABLE quotechg THEN FIND CURRENT quotechg NO-LOCK NO-ERROR.
IF AVAILABLE quoteqty THEN FIND CURRENT quoteqty NO-LOCK NO-ERROR.
IF AVAILABLE quoteitm THEN FIND CURRENT quoteitm NO-LOCK NO-ERROR.
IF AVAILABLE quotehd  THEN DO:
  FIND CURRENT quotehd  NO-LOCK NO-ERROR.

  RELEASE quotechg.
  RELEASE quoteqty.
  RELEASE quoteitm.

  FOR EACH quoteitm OF quotehd NO-LOCK,
      FIRST bf-eb NO-LOCK
      WHERE bf-eb.company EQ quotehd.company
        AND bf-eb.est-no  EQ quotehd.est-no
        AND bf-eb.part-no EQ quoteitm.part-no,
      FIRST itemfg NO-LOCK
      WHERE itemfg.company EQ bf-eb.company
        AND itemfg.i-no    EQ bf-eb.stock-no,
      EACH quoteqty OF quoteitm NO-LOCK:

    RUN fg/makenote.p (BUFFER oe-ordl,
                       BUFFER quoteqty,
                       BUFFER ar-invl,
                       NO,
                       itemfg.rec_key).
  END.
END.

RUN repo-query (ROWID(probe)).

SESSION:SET-WAIT-STATE("").

  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-old-part B-table-Win 
PROCEDURE delete-old-part :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-q-no LIKE quotehd.q-no NO-UNDO.


  /* delete quoteitm for old part-no */
  DISABLE TRIGGERS FOR LOAD OF quoteitm.
  FOR EACH quoteitm
       WHERE quoteitm.company EQ cocode
         AND quoteitm.loc     EQ locode
         AND quoteitm.q-no    EQ ip-q-no:
    FIND FIRST w-probeit NO-LOCK WHERE w-probeit.part-no EQ quoteitm.part-no  NO-ERROR.
    IF NOT AVAILABLE w-probeit THEN DELETE quoteitm.
  END.
  /* end delete quoteitm */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-probe B-table-Win 
PROCEDURE display-probe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN printProbe (NO).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE est-summ B-table-Win 
PROCEDURE est-summ :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE li AS INTEGER NO-UNDO.
  DEFINE VARIABLE li1 AS INTEGER NO-UNDO.
  DEFINE BUFFER bff-probe FOR probe .
  
  FIND LAST est-summ NO-LOCK
      WHERE est-summ.company EQ est.company
        AND est-summ.est-no  EQ est.est-no
      USE-INDEX est-qty  NO-ERROR.
  li1 = IF AVAILABLE est-summ THEN est-summ.eqty ELSE 0.
  
  FOR EACH mclean:
    DO li = 1 TO 28:
      FOR EACH bff-probe NO-LOCK
          WHERE bff-probe.company    EQ est.company
            AND bff-probe.est-no     EQ est.est-no
            AND bff-probe.probe-date EQ TODAY
            AND bff-probe.est-qty    EQ qtty[li]
            AND bff-probe.freight    EQ IF est.est-type LE 6 THEN rels[li] ELSE 1
          
          BY bff-probe.probe-time DESCENDING:

        CREATE est-summ.
        ASSIGN
         est-summ.company  = bff-probe.company
         est-summ.est-no   = bff-probe.est-no
         li1               = li1 + 1
         est-summ.eqty     = li1
         est-summ.summ-tot = STRING(mclean.rec-type,"x(20)")     +
                             STRING(mclean.form-no,"9999999999") +
                             mclean.descr
         est-summ.e-num    = bff-probe.line
         est-summ.per-m    = mclean.cost[li].

        LEAVE.
      END.
    END.

    DELETE mclean.
  END.

  RELEASE est-summ.

  IF est.est-type EQ 6 AND vmclean THEN
  DO li = 1 TO 28:
    IF qtty[li] EQ 0 THEN NEXT.

    FOR EACH bff-probe NO-LOCK
        WHERE bff-probe.company    EQ xest.company
          AND bff-probe.est-no     EQ xest.est-no
          AND bff-probe.probe-date EQ TODAY
          AND bff-probe.est-qty    EQ qtty[li]
          AND bff-probe.freight    EQ rels[li]
        
        BY bff-probe.probe-time DESCENDING:
      LEAVE.
    END.

    IF AVAILABLE bff-probe THEN RUN cec/pr4-mcl1.p (ROWID(bff-probe)).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-dir-proc B-table-Win 
PROCEDURE get-dir-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-search AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER op-tmp-dir AS CHARACTER NO-UNDO.

   DEFINE VARIABLE viDirCount AS INTEGER NO-UNDO.

   DO viDirCount = 1 TO 3:

      CASE viDirCount:
          WHEN 1 THEN
             op-tmp-dir = lv-cebrowse-dir.
          WHEN 2 THEN
             op-tmp-dir = "users\".
          WHEN 3 THEN
             op-tmp-dir = ".\".
      END CASE.

      IF LOOKUP(SUBSTRING(op-tmp-dir,LENGTH(op-tmp-dir)),"\,/") EQ 0 THEN
         op-tmp-dir = op-tmp-dir + "\".

      op-tmp-dir = REPLACE(op-tmp-dir,"/","\").

      IF viDirCount EQ 3 OR SEARCH(op-tmp-dir + ip-search) NE ? THEN
         LEAVE.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-est-type B-table-Win 
PROCEDURE get-est-type :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER op-est-type AS INTEGER.


  op-est-type = IF AVAILABLE est THEN est.est-type ELSE 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE import-price B-table-Win 
PROCEDURE import-price :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lv-price AS DECIMAL DECIMALS 10 NO-UNDO.
  DEFINE VARIABLE lv-subprice AS DECIMAL DECIMALS 10 NO-UNDO.
  DEFINE VARIABLE lv-subquantity AS DECIMAL DECIMALS 10 NO-UNDO.
  DEFINE BUFFER b-eb FOR eb.
  
  {est/checkuse.i}

  RUN save-fields.

  DO WITH FRAME {&FRAME-NAME}:

    IF est.est-type NE 8 THEN
      FOR EACH quotehd OF est NO-LOCK,
          
          EACH quoteitm OF quotehd NO-LOCK,
         
          EACH quoteqty OF quoteitm
          NO-LOCK WHERE quoteqty.qty EQ probe.est-qty
          
      
          BY quoteqty.q-no DESCENDING
          BY quoteqty.qty  DESCENDING:
         
          ASSIGN
           lv-changed = "S"
           lv-price   = IF quoteqty.uom EQ "EA" THEN (quoteqty.price * 1000)
                        ELSE
                        IF quoteqty.uom EQ "MSF" THEN (quoteqty.price * ROUND(quoteqty.tot-lbs / 1000,2) / ROUND(quoteqty.qty / 1000,2))
                        ELSE quoteqty.price
           probe.sell-price:{&SVB} = STRING(lv-price).
         
          RUN calc-fields NO-ERROR.
          IF ERROR-STATUS:ERROR THEN RETURN.
         
          RUN dispatch ("update-record").
          LEAVE.
       END.
    ELSE /*combo/tandem*/
    DO:
       FOR EACH quotehd OF est NO-LOCK,
          EACH b-eb OF est NO-LOCK WHERE
               ,
          FIRST quoteitm OF quotehd NO-LOCK WHERE
                quoteitm.part-no EQ b-eb.part-no AND
                quoteitm.i-no EQ b-eb.stock-no
               ,
          
          FIRST quoteqty OF quoteitm NO-LOCK WHERE
               quoteqty.qty EQ b-eb.bl-qty
      
           BREAK BY quoteqty.q-no DESCENDING
                 BY quoteqty.qty  DESCENDING:

          ASSIGN
             lv-price = IF quoteqty.uom EQ "EA" THEN (quoteqty.price * 1000)
                        ELSE
                        IF quoteqty.uom EQ "MSF" THEN (quoteqty.price * ROUND(quoteqty.tot-lbs / 1000,2) / ROUND(quoteqty.qty / 1000,2))
                        ELSE quoteqty.price
             lv-subprice = lv-subprice + (lv-price * quoteqty.qty)
             lv-subquantity = lv-subquantity + quoteqty.qty.

          IF LAST-OF(quoteqty.q-no) THEN
             LEAVE.
       END.

       ASSIGN
          lv-changed = "S".

       IF lv-subquantity NE 0 THEN
          probe.sell-price:{&SVB} = STRING(ROUND(lv-subprice / lv-subquantity,2)).
       ELSE
          probe.sell-price:{&SVB} = "0".
         
       RUN calc-fields NO-ERROR.
       IF ERROR-STATUS:ERROR THEN RETURN.
       
       RUN dispatch ("update-record").
    END.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ld AS DECIMAL NO-UNDO.
  DEFINE VARIABLE ld-tot-fact AS DECIMAL NO-UNDO.
  DEFINE VARIABLE ld-tot-full AS DECIMAL NO-UNDO.
  DEFINE VARIABLE ld-tot-pric AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lc-calling-progs AS CHARACTER NO-UNDO.
  lc-calling-progs = PROGRAM-NAME(1) 
        + (IF PROGRAM-NAME(2) NE ? THEN "," + PROGRAM-NAME(2) ELSE "")
        + (IF PROGRAM-NAME(3) NE ? THEN "," + PROGRAM-NAME(3) ELSE "")
        + (IF PROGRAM-NAME(4) NE ? THEN "," + PROGRAM-NAME(4) ELSE "")
        + (IF PROGRAM-NAME(5) NE ? THEN "," + PROGRAM-NAME(5) ELSE "")
        + (IF PROGRAM-NAME(6) NE ? THEN "," + PROGRAM-NAME(6) ELSE "")
        + (IF PROGRAM-NAME(7) NE ? THEN "," + PROGRAM-NAME(7) ELSE "").
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  /*since commission column not enabled, below is necessary*/
  probe.comm = DECIMAL(probe.comm:{&SVB}).

  IF ll-use-margin THEN DO:         
    /* Get Commission% */
    /*(RUN est/getsmanmtrx.p (ROWID(est), "C",
                           INPUT-OUTPUT probe.comm,
                           INPUT-OUTPUT probe.market-price). */

    FIND FIRST probe-ref
        WHERE probe-ref.reftable EQ "probe-ref"
          AND probe-ref.company  EQ probe.company
          AND probe-ref.loc      EQ ""
          AND probe-ref.code     EQ probe.est-no
          AND probe-ref.code2    EQ STRING(probe.line,"9999999999")
        NO-ERROR.
    IF AVAILABLE probe-ref THEN
    DO:
    
      ASSIGN
       probe-ref.val[2]  = probe.comm * 100000
       probe-ref.val[6]  = probe.comm / 100 *
                           probe.sell-price * (probe.est-qty / 1000).

      FIND CURRENT probe-ref NO-LOCK.
    END.
  END.

  probe.sell-price-wo = probe.sell-price -
                        (probe.sell-price * probe.market-price / 100).
        
  FIND FIRST ce-ctrl {sys/look/ce-ctrlw.i} NO-LOCK.

  FIND FIRST est NO-LOCK
      WHERE est.company EQ probe.company
        AND est.est-no  EQ probe.est-no
      NO-ERROR.

  IF ce-ctrl.sell-by EQ "S" THEN
    probe.gross-profit = (1 - (100 / (100 + probe.gross-profit))) * 100.

  FOR EACH probeit NO-LOCK
      WHERE probeit.company EQ probe.company
        AND probeit.est-no  EQ probe.est-no
        AND probeit.line    EQ probe.line:
    ASSIGN
     ld          = (IF est.est-type LT 7 OR
                       probeit.yrprice   THEN probeit.yld-qty
                                         ELSE probeit.bl-qty) / 1000
     ld-tot-fact = ld-tot-fact + (probeit.fact-cost  * ld)
     ld-tot-full = ld-tot-full + (probeit.full-cost  * ld)
     ld-tot-pric = ld-tot-pric + (probeit.sell-price * ld).
  END.

  ld = probe.est-qty / 1000.
  IF INDEX(lc-calling-progs, "update-item") EQ 0 THEN DO:
      /* IF this is a result of update to items, don't 
         adjust probeit. Should only do this IF probe has
         been changed */
      FOR EACH probeit
          WHERE probeit.company EQ probe.company
            AND probeit.est-no  EQ probe.est-no
            AND probeit.line    EQ probe.line:
        ASSIGN
         probeit.fact-cost  = (probe.fact-cost  * ld) *
                              (probeit.fact-cost  / ld-tot-fact)
         probeit.full-cost  = (probe.full-cost  * ld) *
                              (probeit.full-cost  / ld-tot-full)
         probeit.sell-price = (probe.sell-price * ld) *
                              (probeit.sell-price / ld-tot-pric).
      END.
  END.

  FOR EACH probeit
      WHERE probeit.company EQ probe.company
        AND probeit.est-no  EQ probe.est-no
        AND probeit.line    EQ probe.line:
    ASSIGN
     ld          = (IF est.est-type LT 7 OR
                       probeit.yrprice   THEN probeit.yld-qty
                                         ELSE probeit.bl-qty) / 1000
     ld-tot-fact = probeit.fact-cost  / ld
     ld-tot-full = probeit.full-cost  / ld
     ld-tot-pric = probeit.sell-price / ld.
  END.
  IF v-ceSellPrice EQ "F" THEN DO:
      /* For type 'F', multicell, make correction to gross-profit 
         after it was updated by the db trigger on probe */
      FIND CURRENT probe.

      IF DECIMAL(probe.gross-profit:{&SVB})NE probe.gross-profit THEN DO:
        ASSIGN probe.gross-profit:{&SVB} = STRING(probe.gross-profit).
      END.
  END.
              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
  DEFINE VARIABLE phandle AS HANDLE NO-UNDO.
  
  
  /* Code placed here will execute PRIOR to standard behavior. */
  {est/checkuse.i}

  {methods/template/local/delete.i}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields B-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  FIND FIRST ce-ctrl {sys/look/ce-ctrlw.i} NO-LOCK.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  DO WITH FRAME {&FRAME-NAME}:
    fi_sort-by:SCREEN-VALUE = TRIM(lv-sort-by-lab)               + " " +
                              TRIM(STRING(ll-sort-asc,"As/Des")) + "cending".
  END.

  IF v-called-setCellColumns = NO THEN DO:
      RUN setCellColumns.
      v-called-setCellColumns = YES.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE li AS INTEGER NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  ll-no-valid = YES.
  /* 11061210
  DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY "cursor-left" TO {&BROWSE-NAME}.
    END.
  END.
    */
  {est/checkuse.i}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN save-fields.

  DO WITH FRAME {&FRAME-NAME}:
    probe.do-quote:{&SVB} = "Y".
    APPLY "entry" TO probe.full-cost IN BROWSE {&browse-name} .
  END.
  ll-no-valid = NO.

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
  RUN p-probe-security-proc.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
  {methods/winReSizeLocInit.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lo\n-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
 
  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT AVAILABLE est THEN RETURN "adm-error".
  FOR EACH probe EXCLUSIVE-LOCK 
      WHERE probe.company EQ est.company
        AND probe.est-no  EQ est.est-no:
    ASSIGN
     probe.boardCostPerM = probe.boardCostTotal / (probe.est-qty / 1000)
     probe.boardCostPct = probe.boardCostPerM / probe.sell-price * 100
     probe.boardContributionPerM = probe.sell-price - probe.boardCostPerM
     probe.boardContributionTotal = probe.boardContributionPerM * (probe.est-qty / 1000).
  END.
  FIND xest NO-LOCK WHERE RECID(xest) EQ RECID(est) NO-ERROR.
  FIND xef NO-LOCK WHERE RECID(xef) EQ RECID(ef) NO-ERROR.
  FIND xeb NO-LOCK WHERE RECID(xeb) EQ RECID(eb) NO-ERROR.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ll-use-margin = NO.

  IF cerunc EQ "Fibre" THEN
    RUN est/usemargin.p (ROWID(est), OUTPUT ll-use-margin).

  ASSIGN
     probe.gross-profit:VISIBLE IN BROWSE {&browse-name} = NOT(ll-use-margin AND cerunf = "Fibre" AND cerunc = "Fibre")
     probe.comm:VISIBLE IN BROWSE {&browse-name} = NOT(probe.gross-profit:VISIBLE IN BROWSE {&browse-name}).

  FIND FIRST sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ cocode
                      AND sys-ctrl.name    EQ "SETPRINT"
                       NO-ERROR.
  vmclean2 = AVAILABLE sys-ctrl AND LOOKUP(sys-ctrl.char-fld,"McLEAN,CERunC 2") NE 0 /*sys-ctrl.char-fld EQ "McLean"*/ AND est.est-type EQ 6.
  IF vmclean2 THEN v-match-up = sys-ctrl.dec-fld.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resort-query B-table-Win 
PROCEDURE resort-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
 

  &SCOPED-DEFINE open-query          ~
       OPEN QUERY {&browse-name}     ~
            {&for-each1}        
              
   
   IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                  ELSE {&open-query} {&sortby-phrase-desc}.
 
   RUN dispatch ("row-changed").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE move-columns B-table-Win 
PROCEDURE move-columns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
         br_table:COLUMN-MOVABLE = v-col-move
         br_table:COLUMN-RESIZABLE = v-col-move
         v-col-move = NOT v-col-move.
      FI_moveCol = IF v-col-move = NO THEN "Move" ELSE "Sort".
      DISPLAY FI_moveCol.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE li AS INTEGER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
    
    RUN valid-profit (probe.gross-profit:HANDLE IN BROWSE {&browse-name}) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
    RUN valid-profit (probe.net-profit:HANDLE IN BROWSE {&browse-name}) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
    /*RUN valid-profit (reftable.val[3]:HANDLE IN BROWSE {&browse-name}) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.*/
  END.
  
  RUN calc-fields NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /*RUN cec/uprobeit.p (RECID(probe)).*/

  IF est.est-type EQ 8 THEN DO:
    {cec/com/probe.u}
    RUN per-1000.
  END.
  ELSE
  IF vmclean THEN RUN cec/pr4-mcl1.p (ROWID(probe)).
/*
  DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY "cursor-left" TO {&BROWSE-NAME}.
    END.
  END.
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-sell-price B-table-Win 
PROCEDURE new-sell-price :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  lv-changed = "S".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE p-probe-security-proc B-table-Win 
PROCEDURE p-probe-security-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER b-prgrms FOR prgrms.
DEFINE VARIABLE v-prgmname AS CHARACTER INITIAL "p-probe." NO-UNDO.
DEFINE VARIABLE num-groups AS INTEGER NO-UNDO.
DEFINE VARIABLE g_groups AS CHARACTER NO-UNDO.

FIND FIRST b-prgrms NO-LOCK WHERE
     b-prgrms.prgmname EQ v-prgmname AND
     b-prgrms.DIR_group EQ ""
      NO-ERROR.

IF AVAILABLE b-prgrms THEN
DO:
  FOR EACH usergrps NO-LOCK:
      IF CAN-DO(usergrps.users,USERID("ASI")) THEN
         g_groups = g_groups + usergrps.usergrps + ",".
  END.

  DO num-groups = 1 TO NUM-ENTRIES(g_groups):
     IF NOT CAN-DO(TRIM(b-prgrms.can_update),ENTRY(num-groups,g_groups)) THEN
        NEXT.
    
     IF NOT v-can-update AND CAN-DO(TRIM(b-prgrms.can_update),ENTRY(num-groups,g_groups)) THEN
        v-can-update = YES.
  END.
  
  IF NOT v-can-update AND CAN-DO(TRIM(b-prgrms.can_update),USERID("ASI")) THEN
     v-can-update = YES.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE per-1000 B-table-Win 
PROCEDURE per-1000 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER b-probe FOR probe.

  FOR EACH b-probe NO-LOCK
      WHERE b-probe.company EQ probe.company
        AND b-probe.est-no  EQ probe.est-no
      BY b-probe.probe-date
      BY b-probe.est-qty:

    IF b-probe.LINE LT 100 THEN
    DO:
       RUN get-dir-proc(INPUT TRIM(est.est-no) + ".s" + STRING(b-probe.line,"99"),
                     OUTPUT tmp-dir).

       OS-COPY VALUE(tmp-dir + TRIM(est.est-no) + ".s" + STRING(b-probe.line,"99"))
               VALUE(lv-cebrowse-dir + TRIM(est.est-no) + ".p" + STRING(b-probe.line,"99")).

       RUN get-dir-proc(INPUT TRIM(est.est-no) + ".a" + STRING(b-probe.line,"99"),
                        OUTPUT tmp-dir).

       OS-APPEND VALUE(tmp-dir + TRIM(est.est-no) + ".a" + STRING(b-probe.line,"99"))
                 VALUE(lv-cebrowse-dir + TRIM(est.est-no) + ".p" + STRING(b-probe.line,"99")).
    END.
    ELSE
    DO:
       RUN get-dir-proc(INPUT TRIM(est.est-no) + ".s" + STRING(b-probe.line,"999"),
                        OUTPUT tmp-dir).

       OS-COPY VALUE(tmp-dir + TRIM(est.est-no) + ".s" + STRING(b-probe.line,"999"))
               VALUE(lv-cebrowse-dir + TRIM(est.est-no) + ".p" + STRING(b-probe.line,"999")).

       RUN get-dir-proc(INPUT TRIM(est.est-no) + ".a" + STRING(b-probe.line,"999"),
                        OUTPUT tmp-dir).

       OS-APPEND VALUE(tmp-dir + TRIM(est.est-no) + ".a" + STRING(b-probe.line,"999"))
                 VALUE(lv-cebrowse-dir + TRIM(est.est-no) + ".p" + STRING(b-probe.line,"999")).
    END.

    tmp-dir = lv-cebrowse-dir.

    RUN cec/probeu3.p (ROWID(b-probe)).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-box B-table-Win 
PROCEDURE print-box :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
  DEFINE VARIABLE list-text AS CHARACTER FORMAT 'x(176)' NO-UNDO.
  DEFINE VARIABLE result AS LOGICAL NO-UNDO.
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
  DEFINE VARIABLE h_q-boxdes AS HANDLE NO-UNDO.

  {est/checkuse.i}

  FIND FIRST box-design-hdr NO-LOCK
      WHERE box-design-hdr.design-no EQ 0
        AND box-design-hdr.company EQ eb.company 
        AND box-design-hdr.est-no EQ eb.est-no
        AND box-design-hdr.form-no EQ eb.form-no
        AND box-design-hdr.blank-no EQ eb.blank-no NO-ERROR.
  IF NOT AVAILABLE box-design-hdr THEN DO:
     RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,'box-calc-target',OUTPUT char-hdl).     
     IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN 
     RUN build-box1 IN WIDGET-HANDLE(char-hdl) ('B').
     ELSE DO:
       RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,'container-source',OUTPUT char-hdl).
       RUN init-box-design IN WIDGET-HANDLE(char-hdl) (THIS-PROCEDURE).
     END.
  END.
  
  /* box only print not share with othere */

  IF probe.LINE LT 100 THEN
     ls-outfile = lv-cebrowse-dir + TRIM(est.est-no) + '.x' + STRING(probe.line,'99').
  ELSE
     ls-outfile = lv-cebrowse-dir + TRIM(est.est-no) + '.x' + STRING(probe.line,'999').

  OUTPUT TO VALUE(ls-outfile).
  PUT '</PROGRESS><PREVIEW><P11>'.
  OUTPUT CLOSE.

  RUN printBoxImage.

  FILE-INFO:FILE-NAME = ls-outfile.
  RUN printfile (FILE-INFO:FILE-NAME).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-box-est B-table-Win 
PROCEDURE print-box-est :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-dest AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ip-font AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ip-ornt AS CHARACTER NO-UNDO.

  DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
  DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
  DEFINE VARIABLE result AS LOGICAL NO-UNDO.

  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lv-input AS CHARACTER FORMAT "x(180)" NO-UNDO.
  DEFINE VARIABLE v-box-input AS CHARACTER NO-UNDO.

  DEFINE VARIABLE ls-fax-file AS CHARACTER NO-UNDO.
  DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lv-pdf-file AS CHARACTER NO-UNDO.
  DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ret-code AS INTEGER NO-UNDO.
  DEFINE VARIABLE ls-mail-file2 AS CHARACTER NO-UNDO.
  DEFINE VARIABLE v-line-count AS INTEGER NO-UNDO .

  {est/checkuse.i}

  FIND FIRST box-design-hdr NO-LOCK
      WHERE box-design-hdr.design-no EQ 0
        AND box-design-hdr.company EQ eb.company 
        AND box-design-hdr.est-no    EQ eb.est-no
        AND box-design-hdr.form-no   EQ eb.form-no
        AND box-design-hdr.blank-no  EQ eb.blank-no
         NO-ERROR.
  IF NOT AVAILABLE box-design-hdr THEN DO:
     RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"box-calc-target",OUTPUT char-hdl).     
     IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN 
            RUN build-box1 IN WIDGET-HANDLE(char-hdl)  ("B")  .
     ELSE DO:
       RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"container-source",OUTPUT char-hdl).
       RUN init-box-design IN WIDGET-HANDLE(char-hdl) (THIS-PROCEDURE).
     END.
  END.

  IF probe.LINE LT 100 THEN
     ls-outfile = lv-cebrowse-dir + TRIM(est.est-no) + ".x" + STRING(probe.line,"99").
  ELSE
     ls-outfile = lv-cebrowse-dir + TRIM(est.est-no) + ".x" + STRING(probe.line,"999").
  
  /*
  FIND CURRENT xest.
  IF v-prt-box THEN RUN cec/probbox.p (RECID(probe)).
  FIND CURRENT xest NO-LOCK.
  */

  /* box only print not share with othere */

  OUTPUT TO VALUE(ls-outfile) .
  IF ip-dest EQ 1 THEN PUT "<PRINTER?></PROGRESS><P11>".  /*<REVIEW>*/
  ELSE IF ip-dest EQ 2 THEN DO:
      IF NOT lBussFormModle THEN
         PUT "<PREVIEW=ZoomToWidth><MODAL=NO></PROGRESS><P11>".
      ELSE
         PUT "<PREVIEW=ZoomToWidth></PROGRESS><P11>".          
  END.
  ELSE IF ip-dest EQ 4 THEN DO:
     ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".tIF".
     PUT UNFORMATTED "<PRINTER?><EXPORT=" Ls-fax-file ",BW></PROGRESS><P11>".
  END.        
  ELSE IF ip-dest EQ 5 THEN DO:
      ASSIGN
         init-dir = v-dir
         lv-pdf-file = v-dir + "Est" + TRIM(est.est-no).
      PUT "<PREVIEW><PDF-LEFT=5mm><PDF-TOP=10mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf></PROGRESS><P11>" FORMAT "x(180)".
  END.
  OUTPUT CLOSE.

  IF probe.LINE LT 100 THEN
  DO:
     RUN get-dir-proc(INPUT TRIM(xest.est-no) + ".p" + STRING(probe.line,"99"),
                      OUTPUT tmp-dir).
     OS-APPEND VALUE(tmp-dir + TRIM(xest.est-no) + ".p" + STRING(probe.line,"99"))
               VALUE(ls-outfile). /*page skip problem */
  END.
  ELSE
  DO:
     RUN get-dir-proc(INPUT TRIM(xest.est-no) + ".p" + STRING(probe.line,"999"),
                      OUTPUT tmp-dir).
     OS-APPEND VALUE(tmp-dir + TRIM(xest.est-no) + ".p" + STRING(probe.line,"999"))
               VALUE(ls-outfile). /*page skip problem */
  END.

  lv-input = "".
  
  IF v-prt-box THEN RUN printBoxImage.

ASSIGN v-line-count = LINE-COUNTER .
  IF v-prt-note THEN DO:
    OUTPUT TO VALUE(ls-outfile) APPEND PAGE-SIZE 64  .
    RUN print-notes(v-line-count) .
    OUTPUT CLOSE.
  END.

  FILE-INFO:FILE-NAME = ls-outfile.
  list-name = ls-outfile  /*file-info:FULL-PATHNAME*/ .

      CASE ip-dest:
           WHEN 1 THEN RUN printfile (FILE-INFO:FILE-NAME).
           WHEN 2 THEN RUN printfile (FILE-INFO:FILE-NAME).
           WHEN 3 THEN DO:
               {custom/out2file.i}
           END.
           WHEN 4 THEN DO:
               ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".txt". 
               OS-COPY VALUE(list-name) VALUE(ls-fax-file).
               RUN custom/asifax.p ("",ls-fax-file,"",
                                 'Estimate',
                                 'Estimate',OUTPUT ret-code).
           END.   
           WHEN 5 THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              RUN custom/xpmail.p ("CUSTOMER",lv-pdf-file + ".pdf","",
                                'Estimate',
                                'Estimate',OUTPUT ret-code).
           END. 
           WHEN 6 THEN RUN custom/d-print.w (list-name).
       END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-notes B-table-Win 
PROCEDURE print-notes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER v-line-count AS INTEGER NO-UNDO .
  {custom/notesdef.i}
  DEFINE VARIABLE v-inst2 AS CHARACTER EXTENT 200 NO-UNDO.    
  DEFINE VARIABLE v-dept-inst AS CHARACTER FORMAT "x(80)" EXTENT 200 NO-UNDO.
  DEFINE VARIABLE v-note-length AS INTEGER INITIAL 80 NO-UNDO.
  DEFINE VARIABLE lv-k AS INTEGER NO-UNDO.
  
  /*determine number of lines needed*/
  ASSIGN v-tmp-lines = 0
       j = 0
       K = 0
       lv-got-return = 0.

  FOR EACH notes NO-LOCK WHERE notes.rec_key EQ xest.rec_key AND
      v-prt-note AND notes.note_code GE v-from-dept AND
      notes.note_code LE v-to-dept :
    
    IF v-prev-note-rec NE ? AND
       v-prev-note-rec NE RECID(notes) THEN v-prev-extent = k.

    DO i = 1 TO LENGTH(notes.note_text) :        
       IF i - j GE lv-line-chars THEN ASSIGN j = i
                                             lv-got-return = lv-got-return + 1.
              
       v-tmp-lines = ( i - j ) / lv-line-chars.
       {SYS/INC/ROUNDUP.I v-tmp-lines}
       k = v-tmp-lines + lv-got-return +
       IF (v-prev-note-rec NE RECID(notes) AND v-prev-note-rec NE ?) THEN v-prev-extent ELSE 0.
    
       IF SUBSTRING(note_text,i,1) EQ CHR(10) OR SUBSTRING(note_text,i,1) EQ CHR(13) THEN                
       DO:
          ASSIGN
             lv-got-return = lv-got-return + 1
             j = i.
       END.
    END.
    ASSIGN v-prev-note-rec = RECID(notes)
           j = 0
           lv-got-return = 0.
  END.

  lv-k = k.

  {custom/notespr2.i job v-inst2 lv-k "notes.rec_key = xest.rec_key and v-prt-note and notes.note_code >= v-from-dept and notes.note_code <= v-to-dept" }
  
  PUT SKIP(1)
      "Department Notes: " SKIP.
  

  IF lv-k GT EXTENT(v-dept-inst) THEN lv-k = EXTENT(v-dept-inst).
       
  DO i = 1 TO lv-k:
      IF v-line-count GT 62 THEN DO:
          PAGE.
          v-line-count = 0 .
      END.
     v-dept-inst[i] = v-inst2[i].
     PUT v-dept-inst[i] AT 2 SKIP.
     v-line-count = v-line-count + 1 .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-probe B-table-Win 
PROCEDURE print-probe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN printProbe (YES).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-spec B-table-Win 
PROCEDURE print-spec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
  DEFINE VARIABLE list-text AS CHARACTER FORMAT 'x(176)' NO-UNDO.
  DEFINE VARIABLE result AS LOGICAL NO-UNDO.
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
  DEFINE VARIABLE h_q-boxdes AS HANDLE NO-UNDO.

  {est/checkuse.i}
  
  RUN est/r-specsh.w (INPUT ROWID(eb)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print4 B-table-Win 
PROCEDURE print4 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE call_id AS RECID NO-UNDO.  
  DEFINE VARIABLE fil_id AS RECID NO-UNDO.
  DEFINE VARIABLE lv-error AS LOGICAL NO-UNDO.
  DEFINE VARIABLE v-vend-no   LIKE e-item-vend.vend-no INITIAL "".
  DEFINE VARIABLE v-vend-list AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lv-ef-recid AS RECID NO-UNDO.

  ASSIGN
    lv-ef-recid = RECID(ef)
    tmp-dir = lv-cebrowse-dir.

  {cec/print4p.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print42 B-table-Win 
PROCEDURE print42 :
/*------------------------------------------------------------------------------
  Purpose:   what IF for (Corr. set - est.est-type = 6)  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-tmp-dir LIKE tmp-dir NO-UNDO.

  {sys/FORM/r-top.i}
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE call_id AS RECID NO-UNDO.  
  DEFINE VARIABLE fil_id AS RECID NO-UNDO.
  DEFINE VARIABLE xxx AS DECIMAL NO-UNDO.
  DEFINE VARIABLE zzz AS DECIMAL NO-UNDO.
  DEFINE VARIABLE  x  AS INTEGER NO-UNDO.  
  DEFINE VARIABLE tmpstore AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lv-error AS LOGICAL NO-UNDO.
  DEFINE VARIABLE module AS CHARACTER NO-UNDO.

  ASSIGN
     vprint = YES
     module = str-tit
     tmp-dir = ip-tmp-dir
     v-update-qty-gsa = NO
     ld-gsa-brd = 0
     ld-gsa-mat = 0
     ld-gsa-lab = 0
     dTotalManHrs = 0.

  {cec/print42p.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE printBoxImage B-table-Win 
PROCEDURE printBoxImage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
  DEFINE VARIABLE xefRowID AS ROWID NO-UNDO.
  DEFINE VARIABLE xebRowID AS ROWID NO-UNDO.
  DEFINE VARIABLE xestQtyRowID AS ROWID NO-UNDO.
  DEFINE VARIABLE pHandle AS HANDLE NO-UNDO.

  DEFINE BUFFER xest-qty FOR est-qty.

  ASSIGN /* save current xef and xeb rowid's */
    xefRowID = ROWID(xef)
    xebRowID = ROWID(xeb).
  {methods/run_link.i "RECORD-SOURCE" "getEstQtyRowID" "(OUTPUT xestQtyRowID)"}
  FIND xest-qty NO-LOCK WHERE ROWID(xest-qty) EQ xestQtyRowID.
  FIND CURRENT xest.
  FOR EACH xef NO-LOCK WHERE xef.company EQ xest-qty.company
                         AND xef.est-no EQ xest-qty.est-no
                         AND xef.eqty EQ xest-qty.eqty,
      EACH xeb NO-LOCK WHERE xeb.company EQ xef.company
                         AND xeb.est-no EQ xef.est-no
                         AND xeb.form-no EQ xef.form-no BREAK BY xeb.stock-no:
    RUN cec/probbox.p (RECID(probe)).
    IF xest.est-type EQ 6 THEN
    DO:
       IF probe.LINE LT 100 THEN
       DO:
          RUN get-dir-proc(INPUT TRIM(xest.est-no) + '-01.b' + STRING(probe.line,'99'),
                           OUTPUT tmp-dir).
          OS-APPEND VALUE(tmp-dir + TRIM(xest.est-no) + '-01.b' + STRING(probe.line,'99')) VALUE(ls-outfile).
       END.
       ELSE
       DO:
          RUN get-dir-proc(INPUT TRIM(xest.est-no) + '-01.b' + STRING(probe.line,'999'),
                           OUTPUT tmp-dir).
          OS-APPEND VALUE(tmp-dir + TRIM(xest.est-no) + '-01.b' + STRING(probe.line,'999')) VALUE(ls-outfile).
       END.
    END.
    ELSE
    DO:
       IF probe.LINE LT 100 THEN
       DO:
          RUN get-dir-proc(INPUT TRIM(xest.est-no) + '.b' + STRING(probe.line,'99'),
                           OUTPUT tmp-dir).
          OS-APPEND VALUE(tmp-dir + TRIM(xest.est-no) + '.b' + STRING(probe.line,'99')) VALUE(ls-outfile).
       END.
       ELSE
       DO:
          RUN get-dir-proc(INPUT TRIM(xest.est-no) + '.b' + STRING(probe.line,'999'),
                           OUTPUT tmp-dir).
          OS-APPEND VALUE(tmp-dir + TRIM(xest.est-no) + '.b' + STRING(probe.line,'999')) VALUE(ls-outfile).
       END.
    END.
        
    OUTPUT TO VALUE(ls-outfile) APPEND.
    IF  NOT LAST(xeb.stock-no) THEN
    PUT UNFORMATTED CHR(12) SKIP.
    OUTPUT CLOSE.
  END. /* each xef */
  FIND CURRENT xest NO-LOCK.
  /* restore to saved xef and xeb rowid's */
  FIND xef NO-LOCK WHERE ROWID(xef) EQ xefRowID.
  FIND xeb NO-LOCK WHERE ROWID(xeb) EQ xebRowID.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE printProbe B-table-Win 
PROCEDURE printProbe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipPrompt AS LOGICAL NO-UNDO.

  DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
  DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
  DEFINE VARIABLE result AS LOGICAL NO-UNDO.
  DEFINE VARIABLE v-print-fmt AS CHARACTER NO-UNDO.
  DEFINE VARIABLE is-xprint-form AS LOGICAL NO-UNDO.
  DEFINE VARIABLE v-error AS LOGICAL NO-UNDO.
  DEFINE VARIABLE v-cinput AS CHARACTER FORMAT "x(255)" NO-UNDO.

  DEFINE VARIABLE lv-dest AS INTEGER NO-UNDO.
  DEFINE VARIABLE lv-font AS INTEGER NO-UNDO.
  DEFINE VARIABLE lv-ornt AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lv-lines AS INTEGER NO-UNDO.
  DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
  DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ls-fax-file AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ret-code AS INTEGER NO-UNDO.
  DEFINE VARIABLE ls-mail-file2 AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lv-dir AS CHARACTER NO-UNDO.
  DEFINE VARIABLE v-probe-fmt AS CHARACTER NO-UNDO.
  
  {est/checkuse.i}
  IF ipPrompt THEN DO:
    RUN est/d-estprt.w (OUTPUT v-prt-note,OUTPUT v-prt-box,OUTPUT v-from-dept,
                        OUTPUT v-to-dept,OUTPUT lv-dest,OUTPUT lv-font,
                        OUTPUT lv-ornt,OUTPUT lv-lines,OUTPUT v-error, OUTPUT v-print-cm).
    IF v-error THEN RETURN ERROR.

  END. /* IF ipprompt */
  ELSE
  ASSIGN
    is-xprint-form = YES
    v-prt-note = YES
    v-prt-box = YES
    v-from-dept = ''
    v-to-dept = 'zzzzz'
    lv-dest = 2
    lv-font = 15
    lv-ornt = 'P'
    lv-lines = 63.

    IF probe.spare-char-1 NE "" THEN 
        lv-cebrowse-dir = probe.spare-char-1.
    ELSE 
        lv-cebrowse-dir = cCEBrowseBaseDir.
        
  ASSIGN
     v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999"
     ls-outfile = lv-cebrowse-dir + TRIM(est.est-no) + ".p" + STRING(probe.line,v-probe-fmt).

  FIND CURRENT xest.
  IF xest.est-type LT 7 THEN RUN cec/probeu1.p (RECID(probe)).
  FIND CURRENT xest NO-LOCK.

  v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999".

  RUN get-dir-proc(INPUT TRIM(est.est-no) + ".s" + STRING(probe.line,v-probe-fmt),
                   OUTPUT lv-dir).

  IF OPSYS EQ "unix" THEN
     UNIX SILENT cp  VALUE(lv-dir + TRIM(est.est-no) + ".s" + STRING(probe.line,v-probe-fmt))
                     VALUE(ls-outfile).
  ELSE DO:
    FIND FIRST sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ cocode
                        AND sys-ctrl.name EQ "CEPrint"  NO-ERROR.
    is-xprint-form = AVAILABLE sys-ctrl AND sys-ctrl.char-fld NE 'Text'.
    FIND FIRST sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ cocode
                        AND sys-ctrl.name EQ "JOBCARDC"  NO-ERROR.
    IF AVAILABLE sys-ctrl THEN ASSIGN v-print-fmt = sys-ctrl.char-fld.
    ELSE v-print-fmt = "".
     i = 0 . 
     IF is-xprint-form THEN lv-lines = 64.
     OUTPUT TO VALUE(ls-outfile) PAGE-SIZE VALUE(lv-lines). /* create .x file with page size */

     INPUT FROM VALUE(lv-dir + TRIM(est.est-no) + ".s" + STRING(probe.line,v-probe-fmt)) NO-ECHO.
     REPEAT:
           v-cinput = "".
           IMPORT UNFORMATTED v-cinput.
           IF v-cinput EQ "" THEN PUT SKIP(1).
           ELSE DO:
               IF LOOKUP(SUBSTRING(v-cinput,1,2),"01,02,03,04,05,06,07,08,09,10,11,12") GT 0 AND
                  SUBSTRING(v-cinput,3,1) EQ "/" AND
                  SUBSTRING(v-cinput,6,1) EQ "/"    THEN PAGE. /*seperate page per form*/  
               PUT UNFORMATTED v-cinput SKIP.
               i = i + 1 .
           END.
     END.

     IF NOT is-xprint-form AND v-prt-note THEN RUN print-notes(LINE-COUNTER).

    INPUT CLOSE.
    OUTPUT CLOSE.
    
  END.
  
  IF is-xprint-form THEN RUN print-box-est (lv-dest,lv-font,lv-ornt).
  ELSE DO:
     list-name = ls-outfile.
     OUTPUT CLOSE. /* if output still open, will blowout in scr-rptest.w */
     CASE lv-dest:
          WHEN 1 THEN RUN custom/prntproc.p (list-name,lv-font,lv-ornt).
          WHEN 2 THEN RUN scr-rptest.w (list-name,"Estimate Analysis",lv-font,lv-ornt).
          WHEN 3 THEN DO:
              {custom/out2file.i}
          END.
          WHEN 4 THEN DO:
              ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".txt". 
              OS-COPY VALUE(list-name) VALUE(ls-fax-file).
              RUN custom/asifax.p ("",ls-fax-file,"",
                                'Estimate',
                                'Estimate',OUTPUT ret-code).
          END.   
          WHEN 5 THEN DO:
             ls-mail-file2 = v-dir + "att" + STRING(TIME) + ".txt". 
             OS-COPY VALUE(list-name) VALUE(ls-mail-file2).
             RUN custom/xpmail.p ("CUSTOMER",ls-mail-file2,"",
                                'Estimate',
                                'Estimate',OUTPUT ret-code).
          END. 
          WHEN 6 THEN RUN custom/d-print.w (list-name).
      END CASE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recalc-fields B-table-Win 
PROCEDURE recalc-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-old-price AS DECIMAL NO-UNDO.
    
  DEFINE VARIABLE ld-commc AS DECIMAL NO-UNDO.
  DEFINE VARIABLE ld-factc AS DECIMAL NO-UNDO.
  DEFINE VARIABLE ld-fullc AS DECIMAL NO-UNDO.
  DEFINE VARIABLE ld-price AS DECIMAL NO-UNDO.

  {cec/combasis.i}
  {sys/inc/ceround.i}

  DO WITH FRAME {&FRAME-NAME}:
    ld-price = 0.
    FOR EACH probeit FIELDS(sell-price yld-qty)
        NO-LOCK WHERE probeit.company EQ probe.company
          AND probeit.est-no  EQ probe.est-no
          AND probeit.line    EQ probe.line
        :
    
      ld-price = ld-price +
                 (probeit.sell-price * (probeit.yld-qty / probe.est-qty)).
    END.
  
    ASSIGN
       lv-changed = IF ld-price EQ ip-old-price THEN "" ELSE "S"
       probe.sell-price:{&SVB} =
           STRING(ld-price,probe.sell-price:{&FVB})
       lv-price = STRING(ip-old-price).

    RUN calc-fields.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE release-shared-buffers B-table-Win 
PROCEDURE release-shared-buffers :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RELEASE xest.
  RELEASE xef.
  RELEASE xeb.
  RELEASE xop.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query B-table-Win 
PROCEDURE repo-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    RUN dispatch ('open-query').
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN RUN dispatch ("row-changed").
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-screen-calc B-table-Win 
PROCEDURE run-screen-calc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE VARIABLE lv-eb-recid AS RECID NO-UNDO.
  DEFINE VARIABLE lv-ef-recid AS RECID NO-UNDO.
  DEFINE VARIABLE lv-probe-line LIKE probe.LINE NO-UNDO.
  DEFINE VARIABLE tmp-outfile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE viewfile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE l-new-record AS LOGICAL NO-UNDO.
  DEFINE VARIABLE li AS INTEGER NO-UNDO.
  DEFINE BUFFER bf-probe FOR probe.
  
  IF AVAILABLE est THEN
  FIND FIRST est-summ NO-LOCK WHERE est-summ.company EQ est.company 
                        AND est-summ.est-no EQ est.est-no 
                       NO-ERROR.

  IF NOT AVAILABLE est-summ THEN
      l-new-record = TRUE.

  {est/checkuse.i}
  IF AVAILABLE probe THEN
    lv-probe-line = probe.LINE.
  FIND CURRENT est.
  FIND xef WHERE RECID(xef) EQ RECID(ef).
  FIND xeb WHERE RECID(xeb) EQ RECID(eb).
  vprint = YES.
  lv-eb-recid = RECID(eb).
  lv-ef-recid = RECID(ef).
  
  FOR EACH mclean:
    DELETE mclean.
  END.

  IF cepdies-log AND NOT SatisfiedPDies() THEN RETURN.

  FOR EACH est-op NO-LOCK
      WHERE est-op.company EQ est.company
        AND est-op.est-no  EQ est.est-no
        AND est-op.line    LT 500,
      FIRST mach NO-LOCK
      {sys/look/machW.i}
        AND mach.m-code EQ est-op.m-code:
    IF mach.obsolete THEN DO:
        MESSAGE "Machine: " + TRIM(mach.m-code) +
                " is Inactive, please replace to complete calculation..."
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
  END.

  IF est.est-type EQ 8 THEN RUN cec/com/print4.p NO-ERROR.

  ELSE DO:
    FIND FIRST probe NO-LOCK WHERE probe.company EQ est.company
                       AND probe.est-no EQ est.est-no
                      NO-ERROR.


    /*IF AVAILABLE probe THEN RUN est/d-probeu.w (OUTPUT lv-override). */
    lv-override = NO.

    IF est.est-type EQ 5 THEN RUN print4 NO-ERROR.
                         ELSE RUN print42 (lv-cebrowse-dir) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN DO:
      SESSION:SET-WAIT-STATE("").
      RETURN.
    END.

    /*RUN est-summ. */

  END.
  FOR EACH tt-est-op.
      DELETE tt-est-op.
  END.
  FOR EACH est-op WHERE est-op.company EQ xest.company AND
                        est-op.est-no EQ xest.est-no AND est-op.line GT 500 
                        EXCLUSIVE-LOCK :
           CREATE tt-est-op.
           BUFFER-COPY est-op TO tt-est-op.
           DELETE est-op.  
  END.

  RUN release-shared-buffers.

  SESSION:SET-WAIT-STATE("").

  FIND eb NO-LOCK WHERE RECID(eb) EQ lv-eb-recid .
  FIND ef NO-LOCK WHERE RECID(ef) EQ lv-ef-recid .
  FIND CURRENT est NO-LOCK NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-whatif B-table-Win 
PROCEDURE run-whatif :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lv-eb-recid AS RECID NO-UNDO.
  DEFINE VARIABLE lv-ef-recid AS RECID NO-UNDO.
  DEFINE VARIABLE lv-probe-line LIKE probe.LINE NO-UNDO.
  DEFINE VARIABLE tmp-outfile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE viewfile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE l-new-record AS LOGICAL NO-UNDO.
  DEFINE VARIABLE li AS INTEGER NO-UNDO.
  DEFINE BUFFER bf-probe FOR probe.
  DEFINE VARIABLE ll-return AS LOGICAL     NO-UNDO.
  
  IF AVAILABLE est THEN
  FIND FIRST est-summ NO-LOCK WHERE est-summ.company EQ est.company 
                        AND est-summ.est-no EQ est.est-no 
                       NO-ERROR.

  IF NOT AVAILABLE est-summ THEN
      l-new-record = TRUE. 
  
  IF NOT checkNCBrd() THEN RETURN.

  {est/checkuse.i}
  IF AVAILABLE probe THEN
    lv-probe-line = probe.LINE.
  FIND CURRENT est.
  FIND xef WHERE RECID(xef) EQ RECID(ef).
  FIND xeb WHERE RECID(xeb) EQ RECID(eb).
  vprint = YES.
  lv-eb-recid = RECID(eb).
  lv-ef-recid = RECID(ef).
  
  FOR EACH mclean:
    DELETE mclean.
  END.

  IF cepdies-log AND NOT SatisfiedPDies() THEN RETURN.

  FOR EACH est-op NO-LOCK
      WHERE est-op.company EQ est.company
        AND est-op.est-no  EQ est.est-no
        AND est-op.line    LT 500,
      FIRST mach NO-LOCK
      {sys/look/machW.i}
        AND mach.m-code EQ est-op.m-code:
   IF mach.obsolete THEN DO:
    MESSAGE "Machine: " + TRIM(mach.m-code) +
            " is Inactive, please replace to complete calculation..."
        VIEW-AS ALERT-BOX ERROR.
    RETURN.
   END.
  END.
  RUN est/EstimateProcs.p (est.company, OUTPUT cCeBrowseBaseDir, OUTPUT tmp-dir).
  lv-cebrowse-dir = tmp-dir.
  RUN est\CostResetHeaders.p(?,?).
  IF est.est-type EQ 8 THEN
    RUN cec/com/print4.p NO-ERROR.
   ELSE DO:
    FIND FIRST probe NO-LOCK WHERE probe.company EQ est.company
                       AND probe.est-no EQ est.est-no
                      NO-ERROR.


    IF AVAILABLE probe THEN RUN est/d-probeu.w (OUTPUT lv-override).

    IF v-cestcalc EQ "Prompt on Purge" AND lv-override THEN DO:  /* Task 12101301 */
        MESSAGE "Warning, all existing calculated quantities will be deleted."
            VIEW-AS ALERT-BOX INFO BUTTONS YES-NO
            UPDATE ll-return.
        IF NOT ll-return THEN 
            RETURN .
    END.

    IF est.est-type EQ 5 THEN RUN print4 NO-ERROR.
                         ELSE RUN print42 (lv-cebrowse-dir) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN DO:
      SESSION:SET-WAIT-STATE("").
      RETURN.
    END.

    RUN est-summ.

  END.
  RUN est\CostExportHeaders.p(est.company,TRIM(est.est-no) + "Est").
  
  FOR EACH est-op WHERE est-op.company EQ xest.company AND
                        est-op.est-no EQ xest.est-no AND est-op.line GT 500 
                        EXCLUSIVE-LOCK :
           DELETE est-op.  
  END.

  RUN release-shared-buffers.

  SESSION:SET-WAIT-STATE("").

  FIND eb NO-LOCK WHERE RECID(eb) EQ lv-eb-recid .
  FIND ef NO-LOCK WHERE RECID(ef) EQ lv-ef-recid .
  FIND CURRENT est NO-LOCK NO-ERROR.
  RUN dispatch ('open-query').     

  IF v-ceSellPrice EQ "F" THEN DO:
    
      IF NOT lv-override THEN DO:
         FIND LAST bf-probe NO-LOCK WHERE bf-probe.company EQ xest.company
                              AND bf-probe.est-no  EQ xest.est-no
                             USE-INDEX LINE NO-ERROR.
         IF AVAILABLE bf-probe THEN
            REPOSITION {&browse-name} TO ROWID ROWID(bf-probe) NO-ERROR.
    
      END.
      ELSE DO:
        IF lv-probe-line GT 0 THEN DO:
         FIND LAST bf-probe NO-LOCK WHERE bf-probe.company EQ xest.company
                              AND bf-probe.est-no  EQ xest.est-no
                              AND bf-probe.LINE    EQ lv-probe-line
                             USE-INDEX LINE NO-ERROR.
         IF AVAILABLE bf-probe THEN
            REPOSITION {&browse-name} TO ROWID ROWID(bf-probe) NO-ERROR.
        END.
      END.

  
      lv-changed = "G".
      RUN calc-fields.    
      RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
  
      /* For type 'F', multicell, make correction to gross-profit 
         after it was updated by the db trigger on probe */
      FIND CURRENT probe.

      IF DECIMAL(probe.gross-profit:{&SVB})NE probe.gross-profit THEN DO:
        ASSIGN probe.gross-profit:{&SVB} = STRING(probe.gross-profit).
        RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
      END.

  END.
  RUN dispatch ('open-query').
  RUN dispatch ('open-query').     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE save-fields B-table-Win 
PROCEDURE save-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     lv-changed = ""
     lv-fullc   = probe.full-cost:{&SVB}
     lv-nprof   = probe.net-profit:{&SVB}
     lv-gprof   = probe.gross-profit:{&SVB}
     lv-price   = probe.sell-price:{&SVB}
     lv-brd-%   = probe.boardCostPct:{&SVB}
     lv-brdcm   = probe.boardContributionPerM:{&SVB}
     lv-brdc$   = probe.boardContributionTotal:{&SVB}
     lv-comm = probe.comm:{&SVB}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key B-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "company" "probe" "company"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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
  {src/adm/template/snd-list.i "est"}
  {src/adm/template/snd-list.i "ef"}
  {src/adm/template/snd-list.i "eb"}
  {src/adm/template/snd-list.i "probe"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-item B-table-Win 
PROCEDURE update-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: run this procedure only for est-type = 6  cec/box/probeit.p      
------------------------------------------------------------------------------*/
  DEFINE VARIABLE xxx AS DECIMAL NO-UNDO.
  DEFINE VARIABLE ld-tot-pric AS DECIMAL NO-UNDO.
  DEFINE VARIABLE ld-tot-fact AS DECIMAL NO-UNDO.
  DEFINE VARIABLE ld-tot-full AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lv-qty LIKE probe.est-qty NO-UNDO.
  DEFINE VARIABLE ll-price-change AS LOGICAL NO-UNDO.
  DEFINE VARIABLE qm AS DECIMAL.
  DEFINE VARIABLE v-comm LIKE tt-tot NO-UNDO.
  DEFINE VARIABLE v-prf-s AS DECIMAL NO-UNDO.
  DEFINE VARIABLE v-pct-s AS DECIMAL NO-UNDO.
  DEFINE VARIABLE v-probe-fmt AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ld-marg% AS DECIMAL NO-UNDO.
  DEFINE VARIABLE v-old-price AS DECIMAL NO-UNDO.
  DEFINE VARIABLE ld-commc AS DECIMAL NO-UNDO.
  DEFINE VARIABLE ld-fullc AS DECIMAL NO-UNDO.
  DEFINE VARIABLE v-old-full-cost AS DECIMAL NO-UNDO.

  {est/checkuse.i}

  IF est.est-type NE 5 THEN DO:
    FOR EACH tt-probeit:
      DELETE tt-probeit.
    END.

    FOR EACH probeit NO-LOCK
        WHERE probeit.company EQ probe.company
          AND probeit.est-no  EQ probe.est-no
          AND probeit.line    EQ probe.line
        USE-INDEX est-no :
      CREATE tt-probeit.
      BUFFER-COPY probeit TO tt-probeit
      ASSIGN tt-probeit.row-id = ROWID(probeit).
    END.

    RUN cec/d-probit.w  (RECID(probe)).

    ll-price-change = NO.
    FOR EACH tt-probeit:
      FIND FIRST probeit NO-LOCK WHERE ROWID(probeit) EQ tt-probeit.row-id NO-ERROR.
      ll-price-change = NOT AVAILABLE probeit OR
                        probeit.sell-price NE tt-probeit.sell-price.
      IF ll-price-change THEN LEAVE.
    END.

    IF ll-price-change THEN DO TRANSACTION:
      FIND CURRENT probe.

      probe.do-quote = YES.

      IF est.est-type EQ 6 THEN DO:

        v-old-price = probe.sell-price.

        RUN cec/uprobit2.p (RECID(probe)). 

        RUN recalc-fields(INPUT v-old-price).
    
        RUN dispatch ("assign-record").

        RUN dispatch ("end-update").

        IF vmclean THEN RUN cec/pr4-mcl1.p (ROWID(probe)).
      END.

      ELSE DO:
        FOR EACH probeit {&where-probeit}:
          ASSIGN
           lv-qty      = IF probeit.yrprice THEN probeit.yld-qty
                                            ELSE probeit.bl-qty
           ld-tot-pric = ld-tot-pric +
                         (probeit.sell-price * (lv-qty / 1000))
           ld-tot-fact = ld-tot-fact +
                         (probeit.fact-cost  * (lv-qty / 1000))
           ld-tot-full = ld-tot-full +
                         (probeit.full-cost  * (lv-qty / 1000)).
        END.

        {cec/combasis.i}
            
        ASSIGN
         v-com = probe.comm
         qm                 = probe.est-qty / 1000
         v-old-price        = probe.sell-price
         v-old-full-cost    = probe.full-cost
         probe.sell-price   = ROUND(ld-tot-pric / qm,2)
         probe.fact-cost    = ROUND(ld-tot-fact / qm,2)
         probe.full-cost    = ROUND(ld-tot-full / qm,2)
         probe.net-profit   = (1 - (probe.full-cost / probe.sell-price)) * 100
         probe.gross-profit = (1 - (probe.fact-cost / probe.sell-price)) * 100.

         IF ll-use-margin THEN DO:  /* Get Commission% */
            ASSIGN
               ld-commc = (v-old-price - (IF v-basis EQ "G" THEN probe.fact-cost ELSE 0))
                        * (v-com / 100)
               ld-fullc = v-old-full-cost - ld-commc
               ld-marg% = ROUND((probe.sell-price - ld-fullc) / probe.sell-price * 100,2) .

            RUN est/getsmanmtrx.p (ROWID(est), "C",
                                   INPUT-OUTPUT v-com,
                                   INPUT-OUTPUT ld-marg%).

            ASSIGN
               probe.comm = v-com /*v-com has to be updated with new comm*/
               ld-commc = (probe.sell-price - (IF v-basis EQ "G" THEN probe.fact-cost ELSE 0))
                        * (probe.comm / 100)
               ld-fullc = ld-fullc + ld-commc
               probe.full-cost = ld-fullc
               probe.net-profit = (1 - (probe.full-cost / probe.sell-price)) * 100
               probe.gross-profit = (1 - (probe.fact-cost / probe.sell-price)) * 100
               probe.market-price = ld-marg%.
         END.
         ELSE
            probe.market-price = probe.net-profit + probe.comm.

         /*probe.net-profit   = probe.net-profit   - v-com
         probe.gross-profit = probe.gross-profit - v-com*/

        ASSIGN
         v-prf-s = probe.sell-price - probe.fact-cost
         v-pct-s = v-prf-s / probe.fact-cost * 100.

        {cec/com/probeu.i}

        v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999".

        RUN get-dir-proc(INPUT TRIM(est.est-no) + ".s"
                               + STRING(probe.line,v-probe-fmt),
                         OUTPUT tmp-dir).
        
        OS-COPY VALUE(tmp-dir + TRIM(est.est-no) + ".s" + STRING(probe.line,v-probe-fmt))
                VALUE(lv-cebrowse-dir + TRIM(est.est-no) + ".p" + STRING(probe.line,v-probe-fmt)).

        RUN per-1000.
      END.

      FIND CURRENT probe NO-LOCK.

      RUN dispatch ('display-fields').
    END.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-quote B-table-Win 
PROCEDURE update-quote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.


  {est/checkuse.i}

  RUN create-quote.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"container-source", OUTPUT char-hdl).
  RUN select-page IN WIDGET-HANDLE(char-hdl) (10).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-profit B-table-Win 
PROCEDURE valid-profit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-focus AS HANDLE NO-UNDO.


  IF NOT ll-no-valid THEN
  DO WITH FRAME {&FRAME-NAME}:
    IF CAN-DO(lv-valid-profit,ip-focus:NAME) AND
       DECIMAL(ip-focus:SCREEN-VALUE) GE 100     THEN DO:
      MESSAGE TRIM(ip-focus:LABEL) +
              " must be less than 100" VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION checkNCBrd B-table-Win 
FUNCTION checkNCBrd RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Based on NK CECPromptNCBrd, this function will prompt the user
            that they have a manufactured form and layout NC = N
    Notes:  Returns user choice of yes/no to continue.
            BV - Task: 08281207
------------------------------------------------------------------------------*/
DEFINE VARIABLE lc-formlist AS CHARACTER INITIAL "" NO-UNDO.
DEFINE VARIABLE li-formcount AS INTEGER INITIAL 0 NO-UNDO.
DEFINE VARIABLE ll-return AS LOGICAL     NO-UNDO.
DEFINE BUFFER lb-ef FOR ef.
DEFINE BUFFER lb-eb FOR eb.



FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.NAME EQ "CECPromptNCBrd"
      AND sys-ctrl.log-fld = YES
     NO-ERROR.

IF AVAILABLE sys-ctrl THEN DO:
    IF AVAILABLE est THEN
        FOR EACH lb-ef NO-LOCK WHERE lb-ef.company EQ est.company
            AND lb-ef.est-no EQ est.est-no:
            IF NOT lb-ef.nc THEN DO:
                FIND FIRST lb-eb NO-LOCK WHERE lb-eb.company EQ lb-ef.company
                    AND lb-eb.est-no EQ lb-ef.est-no
                    AND lb-eb.form-no EQ lb-ef.form-no
                    AND NOT lb-eb.pur-man  NO-ERROR.
                IF AVAILABLE lb-eb THEN
                    ASSIGN 
                        li-formcount = li-formcount + 1
                        lc-formlist = lc-formlist + STRING(lb-ef.form-no) + " ".
            END.
        END.
    IF li-formcount GT 0 THEN DO:
        IF li-formcount GT 1 THEN
            lc-formlist = "Forms " + lc-formlist.
        ELSE lc-formlist = "Form " + lc-formlist.
        MESSAGE lc-formlist "Board is set to NC so board Cost will be zero.  Proceed?"
            VIEW-AS ALERT-BOX INFO BUTTONS YES-NO
            UPDATE ll-return.
        RETURN ll-return.   /* Function return value. */
    END.
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION cvt-time B-table-Win 
FUNCTION cvt-time RETURNS CHARACTER
  ( INPUT ip-time AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ls-time AS CHARACTER NO-UNDO.
  ls-time = STRING(ip-time,"HH:MM:SS").
  RETURN ls-time.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-gp B-table-Win 
FUNCTION display-gp RETURNS DECIMAL
  ( INPUT ip-type AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lv-gp AS DECIMAL NO-UNDO.


  FIND FIRST ce-ctrl {sys/look/ce-ctrlw.i} NO-LOCK.

  DO WITH FRAME {&FRAME-NAME}:
    lv-gp = IF ce-ctrl.sell-by EQ "S" THEN
              IF ip-type EQ 1 THEN
                (probe.sell-price - probe.fact-cost) / probe.fact-cost * 100
              ELSE
                (DECIMAL(probe.sell-price:{&SVB}) -
                 DECIMAL(probe.fact-cost:{&SVB})) /
                DECIMAL(probe.fact-cost:{&SVB}) * 100
            ELSE
              IF ip-type EQ 1 THEN
                probe.gross-profit
              ELSE
                DECIMAL(gross-profit:{&SVB}).
  END.

  RETURN lv-gp.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fDirectMatPctSellPrice B-table-Win 
FUNCTION fDirectMatPctSellPrice RETURNS DECIMAL
  ( INPUT ip-type AS INT ):
/*------------------------------------------------------------------------------
 Purpose:  Calculates Mat %
 Notes: Ticket 24941 
------------------------------------------------------------------------------*/
        DEFINE VARIABLE dMatPct AS DECIMAL NO-UNDO.
        DEFINE VARIABLE dPrice AS DECIMAL NO-UNDO.
    
    ASSIGN 
        dPrice = 0
        dMatPct = 0.
    IF ip-type EQ 2 THEN do:
        IF AVAILABLE probe THEN 
            dPrice = DEC(probe.sell-price:{&SVB}).
        IF dPrice GT 0 THEN 
            dMatPct = DECIMAL(probe.spare-dec-1:{&SVB}) / dPrice * 100.
    END.
    ELSE DO:
        IF AVAILABLE probe THEN 
            dPrice = DEC(probe.sell-price).
        IF dPrice GT 0 THEN 
            dMatPct = probe.spare-dec-1 / dPrice * 100.
    END.
    
    RETURN dMatPct.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SatisfiedPDies B-table-Win 
FUNCTION SatisfiedPDies RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE BUFFER bf-eb FOR eb.
  
  DEFINE VARIABLE lSatisfied AS LOGICAL NO-UNDO.
  DEFINE VARIABLE cMachine AS CHARACTER NO-UNDO.

  lSatisfied = YES.
  cMachine = "".

  DieLoop:
  FOR EACH bf-eb NO-LOCK WHERE bf-eb.company EQ est.company
                   AND bf-eb.est-no EQ est.est-no
                   AND bf-eb.blank-no GT 0
                   AND bf-eb.pur-man EQ NO :
  
    FIND FIRST style NO-LOCK WHERE style.company EQ bf-eb.company
                 AND style.style EQ bf-eb.style
                 AND style.flute EQ bf-eb.flute
                 AND style.test EQ bf-eb.test 
                 AND (style.TYPE EQ "p" OR style.TYPE EQ "R") 
         NO-ERROR.
    IF NOT AVAILABLE style THEN
       FIND FIRST style NO-LOCK WHERE style.company EQ bf-eb.company
                 AND style.style EQ bf-eb.style
                 AND style.flute EQ ""
                 AND style.test EQ ""
                 AND (style.TYPE EQ "p" OR style.TYPE EQ "R") 
         NO-ERROR.
    IF NOT AVAILABLE style THEN NEXT.


    FOR EACH est-op NO-LOCK
        WHERE est-op.company EQ est.company
          AND est-op.est-no  EQ est.est-no
          AND est-op.s-num EQ bf-eb.form-no
          AND est-op.line    LT 500:

         IF CAN-DO(cePDies-cha,est-op.m-code) AND 
            est-op.att-type[1] EQ "" AND
            est-op.att-type[2] EQ "" AND
            est-op.att-type[3] EQ "" 
            THEN DO:
            lSatisfied = NO.
            cMachine = est-op.m-code.
            LEAVE DieLoop.
         END.
                       
    END.

  END. /* each eb */

  IF NOT lSatisfied THEN 
     MESSAGE "Not Die Exists for Machine " cMachine
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.

  RETURN lSatisfied.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION voverall B-table-Win 
FUNCTION voverall RETURNS DECIMAL
  ( INPUT ip-type AS INTEGER )  :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lv-overall AS DECIMAL NO-UNDO.


  IF AVAILABLE probe THEN
    lv-overall = ROUND((IF ip-type EQ 1 THEN probe.sell-price
                                        ELSE DECIMAL(probe.sell-price:{&SVB}))
                 / probe.bsf,2).

  ELSE lv-overall = 0.

  IF lv-overall EQ ? THEN lv-overall = 0.
   
  RETURN lv-overall.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION vtot-msf B-table-Win 
FUNCTION vtot-msf RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lv-tot-msf AS DECIMAL NO-UNDO.

  
  IF AVAILABLE probe THEN lv-tot-msf = probe.tot-lbs / 1000.
  ELSE lv-tot-msf = 0.

  RETURN lv-tot-msf.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

