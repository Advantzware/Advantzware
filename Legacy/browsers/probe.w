&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
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

def var voverall as dec form ">>,>>>,>>9.99" no-undo.
def var vtot-msf as dec form ">>>>9.99" no-undo.
 
{jcrep/r-ticket.i "new shared"}
{cecrep/jobtick.i "new shared"}

{cec/print4.i "new shared" "new shared"}
{cec/print42.i "new shared"}

{sys/inc/var.i "new shared"}

{custom/globdefs.i}

DEF BUFFER probe-ref FOR reftable.
DEF BUFFER b-probemk FOR reftable.

def new shared var k_frac as dec init "6.25" no-undo.
def new shared var day_str as cha form "x(10)" no-undo.
def new shared var tim_str as cha form "x(8)" no-undo.
def new shared var maxpage as int form ">9" no-undo.
def new shared var tmp-dir as cha no-undo.
def new shared var col-norm as cha init "White/Blue" no-undo. 
def new shared var qty as int NO-UNDO.
DEF NEW SHARED VAR v-shared-rel AS INT NO-UNDO.
def new shared var v-do-gsa like do-gsa no-undo.
def new shared buffer xop for est-op.


def new shared var v-qtty like qtty no-undo.
def new shared var v-drop-rc as log no-undo.
DEF NEW SHARED VAR v-update-qty-gsa AS LOG NO-UNDO.
DEF NEW SHARED VAR ld-gsa-brd AS DEC NO-UNDO.
DEF NEW SHARED VAR ld-gsa-mat AS DEC NO-UNDO.
DEF NEW SHARED VAR ld-gsa-lab AS DEC NO-UNDO.

def var v as int no-undo.
def var vn-out like ef.n-out-l init 1 no-undo.
def var v-outf as dec no-undo.
def var v-on-f as dec no-undo.
def var v-on-l as dec no-undo.
def var sh-tmp like sh-len no-undo.
def var v-widp as log no-undo.
def var v-brd-only like sys-ctrl.log-fld init no no-undo.
def var v-brd-cost as dec no-undo.
def var v-module as char format "x(60)" no-undo.
def new shared var v-prep-mat like tprep-mat no-undo.  /* for probemk cost */
def new shared var v-prep-lab like tprep-lab no-undo.
def var v-bqty as int no-undo.
def var v-gsa as log init no no-undo.
def var ls-outfile as cha no-undo.
def var ls-probetime as cha no-undo.  /* time display */
DEF VAR v-tmp-int AS INT NO-UNDO.
DEF VAR v-can-update AS LOG NO-UNDO.
DEF VAR v-orig-gp AS CHAR NO-UNDO.
DEF VAR v-orig-cm-pct AS CHAR NO-UNDO.
DEF VAR v-ceSellPrice AS CHAR NO-UNDO.

def new shared workfile w-form
    field form-no like ef.form-no
    field min-msf as   log init no.

DEF TEMP-TABLE w-probeit LIKE probeit
    FIELD mat-cost   LIKE probe.mat-cost
    FIELD lab-cost   LIKE probe.lab-cost
    FIELD vo-cost    LIKE probe.vo-cost
    FIELD fo-cost    LIKE probe.fo-cost
    FIELD probe-date LIKE probe.probe-date.

def TEMP-TABLE q-sort no-undo field qty as dec field rel as int.
def TEMP-TABLE q-sort1 no-undo field qty as dec field rel as int.
def TEMP-TABLE q-sort2 no-undo field qty as dec field rel as int.

def new shared temp-table tt-qtty field qtty like qtty
                                  field rel like rels.

DEF TEMP-TABLE tt-bqty NO-UNDO FIELD tt-bqty AS INT FIELD tt-brel AS INT.

DEF TEMP-TABLE tt-probeit LIKE probeit
                          FIELD row-id AS ROWID.
DEF NEW SHARED TEMP-TABLE tt-est-op LIKE est-op.    
&SCOPED-DEFINE where-probeit WHERE probeit.company EQ probe.company ~
                               AND probeit.est-no  EQ probe.est-no  ~
                               AND probeit.line    EQ probe.line

DEF VAR lv-override AS LOG NO-UNDO. /* probe override or new creatation */
DEF VAR module AS cha FORM "x(60)" NO-UNDO.
DEF VAR lv-changed AS CHAR NO-UNDO.
DEF VAR lv-fullc AS CHAR NO-UNDO.
DEF VAR lv-gprof AS CHAR NO-UNDO.
DEF VAR lv-nprof AS CHAR NO-UNDO.
DEF VAR lv-price AS CHAR NO-UNDO.
DEF VAR lv-brd-% AS CHAR NO-UNDO.
DEF VAR lv-brdcm AS CHAR NO-UNDO.
DEF VAR lv-brdc$ AS CHAR NO-UNDO.
DEF VAR lv-comm AS CHAR NO-UNDO.
DEF VAR hold-value AS CHAR NO-UNDO.
DEF VAR ll-use-margin AS LOG NO-UNDO.
DEF VAR v-prt-note AS LOG NO-UNDO.
DEF VAR v-prt-box AS LOG NO-UNDO.
DEF VAR v-from-dept AS cha NO-UNDO.
DEF VAR v-to-dept AS cha NO-UNDO.
DEF VAR ll-no-valid AS LOG NO-UNDO.
DEF VAR lv-col-no AS INT NO-UNDO.
DEF VAR lv-int AS INT NO-UNDO.
DEF VAR lv-valid-profit AS CHAR NO-UNDO
    INIT "market-price,gross-profit,net-profit".

DEF NEW SHARED VAR lv-cebrowse-dir AS CHAR NO-UNDO.
DEF VAR cerunc-dec AS DEC NO-UNDO.
DEF VAR v-dir AS CHAR FORMAT "X(80)" NO-UNDO.
DEF VAR v-cestcalc AS CHAR NO-UNDO.

{custom/xprint.i}

ASSIGN
 cocode = g_company
 locode = g_loc.

find first sys-ctrl where
    sys-ctrl.company eq cocode AND
    sys-ctrl.name    eq "CESTCALC"
    no-lock no-error.

if not avail sys-ctrl then DO TRANSACTION:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "CESTCALC"
   sys-ctrl.descrip = "Corrugated Estimate Calc"
   sys-ctrl.log-fld = NO
   sys-ctrl.char-fld = ""
   sys-ctrl.int-fld = 0.
end.
  v-cestcalc = sys-ctrl.char-fld.


find first sys-ctrl where
    sys-ctrl.company eq cocode AND
    sys-ctrl.name    eq "CEBROWSE"
    no-lock no-error.

if not avail sys-ctrl then DO TRANSACTION:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "CEBROWSE"
   sys-ctrl.descrip = "# of Records to be displayed in browser"
   sys-ctrl.log-fld = YES
   sys-ctrl.char-fld = "CE"
   sys-ctrl.int-fld = 30.
end.

IF sys-ctrl.char-fld NE "" THEN
   tmp-dir = sys-ctrl.char-fld.
ELSE
   tmp-dir = "users\".

IF LOOKUP(SUBSTRING(tmp-dir,LENGTH(tmp-dir)),"\,/") EQ 0 THEN
   tmp-dir = tmp-dir + "\".

ASSIGN
  tmp-dir = REPLACE(tmp-dir,"/","\").
  lv-cebrowse-dir = tmp-dir.

FIND FIRST users WHERE
     users.user_id EQ USERID("ASI")
     NO-LOCK NO-ERROR.

IF AVAIL users AND users.user_program[2] NE "" THEN
   v-dir = users.user_program[2] + "\".
ELSE
   v-dir = "c:\tmp\".

FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.
FIND FIRST loc WHERE loc.loc EQ locode NO-LOCK NO-ERROR.

ASSIGN
   module = IF AVAIL company THEN company.NAME ELSE cocode
   module = module + " - " + IF AVAILABLE loc THEN loc.dscr ELSE locode.

DO TRANSACTION:
  {sys/inc/cerun.i C}
  
  ASSIGN
   do-speed  = sys-ctrl.log-fld
   vmclean   = sys-ctrl.char-fld ne ""
   vsuthrlnd = lookup(sys-ctrl.char-fld,"Suthrlnd,Clevelnd,Brick") ne 0
   cerunc-dec = sys-ctrl.dec-fld.

  {sys/inc/cerun.i F}
  {sys/inc/cewhatif.i}
  {sys/inc/ceprint.i}
  {sys/inc/cepdies.i}
END.

{sys/inc/ceprepprice.i}
{sys/inc/ceprep.i}
{sys/inc/f16to32.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES est ef eb
&Scoped-define FIRST-EXTERNAL-TABLE est


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR est, ef, eb.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES probe reftable

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table probe.est-qty probe.fact-cost ~
probe.full-cost probe.market-price display-gp (1) @ probe.gross-profit ~
display-gp (1) @ probe.gross-profit probe.gross-profit reftable.val[11] ~
probe.comm probe.net-profit probe.sell-price probe.gsh-qty probe.do-quote ~
voverall(1) @ voverall probe.probe-date reftable.val[2] reftable.val[3] ~
reftable.val[4] reftable.val[5] probe.probe-user vtot-msf() @ vtot-msf ~
cvt-time(probe.probe-time) @ ls-probetime reftable.val[8] reftable.val[9] ~
reftable.val[10] probe.line 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table probe.full-cost ~
probe.market-price probe.gross-profit reftable.val[11] probe.net-profit ~
probe.sell-price probe.do-quote reftable.val[3] reftable.val[4] ~
reftable.val[5] reftable.val[8] reftable.val[9] reftable.val[10] 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table probe reftable
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table probe
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-br_table reftable
&Scoped-define QUERY-STRING-br_table FOR EACH probe WHERE probe.company = eb.company and ~
ASI.probe.est-no = eb.est-no ~
      AND probe.probe-date ne ? NO-LOCK, ~
      FIRST reftable WHERE reftable.reftable EQ "probe.board" AND ~
reftable.company  EQ probe.company AND ~
reftable.loc      EQ ""            AND ~
reftable.code     EQ probe.est-no  AND ~
reftable.code2    EQ STRING(probe.line,"9999999999") NO-LOCK ~
    BY probe.company ~
       BY probe.est-no ~
        BY probe.probe-date ~
         BY probe.est-qty
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH probe WHERE probe.company = eb.company and ~
ASI.probe.est-no = eb.est-no ~
      AND probe.probe-date ne ? NO-LOCK, ~
      FIRST reftable WHERE reftable.reftable EQ "probe.board" AND ~
reftable.company  EQ probe.company AND ~
reftable.loc      EQ ""            AND ~
reftable.code     EQ probe.est-no  AND ~
reftable.code2    EQ STRING(probe.line,"9999999999") NO-LOCK ~
    BY probe.company ~
       BY probe.est-no ~
        BY probe.probe-date ~
         BY probe.est-qty.
&Scoped-define TABLES-IN-QUERY-br_table probe reftable
&Scoped-define FIRST-TABLE-IN-QUERY-br_table probe
&Scoped-define SECOND-TABLE-IN-QUERY-br_table reftable


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 

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
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calc-cm B-table-Win 
FUNCTION calc-cm RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calc-cmah B-table-Win 
FUNCTION calc-cmah RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calc-cmoh B-table-Win 
FUNCTION calc-cmoh RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD checkNCBrd B-table-Win 
FUNCTION checkNCBrd RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD cvt-time B-table-Win 
FUNCTION cvt-time RETURNS CHARACTER
  ( input ip-time as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-gp B-table-Win 
FUNCTION display-gp RETURNS DECIMAL
  ( INPUT ip-type AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SatisfiedPDies B-table-Win 
FUNCTION SatisfiedPDies RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD voverall B-table-Win 
FUNCTION voverall RETURNS DECIMAL
  ( INPUT ip-type AS INT )   FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD vtot-msf B-table-Win 
FUNCTION vtot-msf RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      probe, 
      reftable SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      probe.est-qty FORMAT ">>>>>>>9":U COLUMN-FONT 0
      probe.fact-cost COLUMN-LABEL "Tot.Fact!Cost" FORMAT ">>,>>>,>>9.99":U
            WIDTH 19 COLUMN-FONT 0
      probe.full-cost FORMAT ">>,>>>,>>9.99":U WIDTH 19 COLUMN-FONT 0
      probe.market-price COLUMN-LABEL "Margin%" FORMAT "->>9.99":U
      display-gp (1) @ probe.gross-profit
      display-gp (1) @ probe.gross-profit
      probe.gross-profit COLUMN-LABEL "Gross%" FORMAT "->>9.99":U
            COLUMN-FONT 0
      reftable.val[11] COLUMN-LABEL "CM%" FORMAT "->>,>>9.99":U
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
      reftable.val[2] COLUMN-LABEL "Board/M" FORMAT "->,>>>,>>9.99":U
            WIDTH 17
      reftable.val[3] COLUMN-LABEL "Board%" FORMAT "->>9.99":U
      reftable.val[4] COLUMN-LABEL "Board!Contrib/M" FORMAT "->,>>>,>>9.99":U
            WIDTH 17
      reftable.val[5] COLUMN-LABEL "Board!Contrib$" FORMAT "->>>,>>>,>>9.99":U
            WIDTH 19
      probe.probe-user COLUMN-LABEL "Probe By" FORMAT "X(8)":U
      vtot-msf() @ vtot-msf COLUMN-LABEL "Total!MSF" COLUMN-FONT 0
      cvt-time(probe.probe-time) @ ls-probetime COLUMN-LABEL "Time" FORMAT "x(8)":U
      reftable.val[8] COLUMN-LABEL "CM$" FORMAT "->>,>>>,>>9.99":U
            WIDTH 19
      reftable.val[9] COLUMN-LABEL "CMAH" FORMAT "->>,>>>,>>9.99":U
            WIDTH 19
      reftable.val[10] COLUMN-LABEL "CMOH" FORMAT "->>,>>>,>>9.99":U
            WIDTH 19
      probe.line FORMAT ">>9":U
  ENABLE
      probe.full-cost
      probe.market-price
      probe.gross-profit
      reftable.val[11]
      probe.net-profit
      probe.sell-price
      probe.do-quote
      reftable.val[3]
      reftable.val[4]
      reftable.val[5]
      reftable.val[8]
      reftable.val[9]
      reftable.val[10]
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 143 BY 13.1
         FONT 0
         TITLE "Estimate  Analysis Per Thousand".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
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
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       probe.comm:VISIBLE IN BROWSE br_table = FALSE
       probe.sell-price:AUTO-RESIZE IN BROWSE br_table = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "ASI.probe WHERE ASI.eb <external> ...,ASI.reftable WHERE ASI.probe ..."
     _Options          = "NO-LOCK KEY-PHRASE"
     _TblOptList       = ", FIRST"
     _OrdList          = "ASI.probe.company|yes,ASI.probe.est-no|yes,ASI.probe.probe-date|yes,ASI.probe.est-qty|yes"
     _JoinCode[1]      = "probe.company = eb.company and
ASI.probe.est-no = ASI.eb.est-no"
     _Where[1]         = "ASI.probe.probe-date ne ?"
     _JoinCode[2]      = "reftable.reftable EQ ""probe.board"" AND
reftable.company  EQ probe.company AND
reftable.loc      EQ """"            AND
reftable.code     EQ probe.est-no  AND
reftable.code2    EQ STRING(probe.line,""9999999999"")"
     _FldNameList[1]   > ASI.probe.est-qty
"probe.est-qty" ? ? "integer" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.probe.fact-cost
"probe.fact-cost" "Tot.Fact!Cost" ">>,>>>,>>9.99" "decimal" ? ? 0 ? ? ? no ? no no "19" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.probe.full-cost
"probe.full-cost" ? ">>,>>>,>>9.99" "decimal" ? ? 0 ? ? ? yes ? no no "19" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.probe.market-price
"probe.market-price" "Margin%" "->>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"display-gp (1) @ probe.gross-profit" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"display-gp (1) @ probe.gross-profit" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.probe.gross-profit
"probe.gross-profit" "Gross%" "->>9.99" "decimal" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.reftable.val[11]
"reftable.val[11]" "CM%" ? "decimal" ? ? ? ? ? ? yes ? no no "9.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.probe.comm
"probe.comm" ? ? "decimal" ? ? ? ? ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.probe.net-profit
"probe.net-profit" "Net%" "->>9.99" "decimal" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.probe.sell-price
"probe.sell-price" ? ">>,>>>,>>9.99" "decimal" ? ? 0 ? ? ? yes ? no no "19" yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.probe.gsh-qty
"probe.gsh-qty" "Total!Sheets" ">>>>>>9" "integer" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.probe.do-quote
"probe.do-quote" "Q" ? "logical" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"voverall(1) @ voverall" "Price!/BSF" ? ? ? ? 0 ? ? ? no ? no no "19" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   = ASI.probe.probe-date
     _FldNameList[16]   > ASI.reftable.val[2]
"reftable.val[2]" "Board/M" "->,>>>,>>9.99" "decimal" ? ? ? ? ? ? no ? no no "17" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > ASI.reftable.val[3]
"reftable.val[3]" "Board%" "->>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ASI.reftable.val[4]
"reftable.val[4]" "Board!Contrib/M" "->,>>>,>>9.99" "decimal" ? ? ? ? ? ? yes ? no no "17" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > ASI.reftable.val[5]
"reftable.val[5]" "Board!Contrib$" "->>>,>>>,>>9.99" "decimal" ? ? ? ? ? ? yes ? no no "19" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > ASI.probe.probe-user
"probe.probe-user" "Probe By" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > "_<CALC>"
"vtot-msf() @ vtot-msf" "Total!MSF" ? ? ? ? 0 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > "_<CALC>"
"cvt-time(probe.probe-time) @ ls-probetime" "Time" "x(8)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > ASI.reftable.val[8]
"reftable.val[8]" "CM$" "->>,>>>,>>9.99" "decimal" ? ? ? ? ? ? yes ? no no "19" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > ASI.reftable.val[9]
"reftable.val[9]" "CMAH" "->>,>>>,>>9.99" "decimal" ? ? ? ? ? ? yes ? no no "19" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > ASI.reftable.val[10]
"reftable.val[10]" "CMOH" "->>,>>>,>>9.99" "decimal" ? ? ? ? ? ? yes ? no no "19" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > ASI.probe.line
"probe.line" ? ">>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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

   def var phandle as widget-handle no-undo.
   def var char-hdl as cha no-undo.   
   RUN get-link-handle IN adm-broker-hdl
      (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
   phandle = WIDGET-HANDLE(char-hdl).
   
   RUN new-state in phandle ('update-begin':U).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON return OF br_table IN FRAME F-Main /* Estimate  Analysis Per Thousand */
anywhere
DO:
   RUN get-attribute ("FIELDS-ENABLED":U).
   if return-value = "YES" then do:  /* update mode */
      apply "tab" to self.
      return no-apply.
   end.
   else do:
       apply "default-action" to browse {&browse-name}.
       return no-apply. 
   end.   
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
     {est/brsleave.i}   /* same but update will be like add */
     
     if keyfunction(lastkey) = "return" then reposition {&browse-name} forward 0.
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


&Scoped-define SELF-NAME probe.market-price
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL probe.market-price br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF probe.market-price IN BROWSE br_table /* Margin% */
DO:
  IF NOT ll-use-margin THEN DO:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL probe.market-price br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF probe.market-price IN BROWSE br_table /* Margin% */
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL probe.market-price br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF probe.market-price IN BROWSE br_table /* Margin% */
DO:
  lv-changed = "M".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME probe.gross-profit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL probe.gross-profit br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF probe.gross-profit IN BROWSE br_table /* Gross% */
DO:

  v-orig-gp = probe.gross-profit:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}.
  IF ll-use-margin OR v-ceSellPrice = "F" THEN DO:
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
  IF v-orig-gp <> probe.gross-profit:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} 
      OR reftable.val[11]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = "" THEN
    ASSIGN reftable.val[11]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} 
              = probe.gross-profit:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}.
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


&Scoped-define SELF-NAME reftable.val[11]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL reftable.val[11] br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF reftable.val[11] IN BROWSE br_table /* CM% */
DO:
    IF v-ceSellPrice NE "F" THEN DO:
      APPLY "tab" TO reftable.val[11] IN BROWSE {&browse-name}.
      RETURN NO-APPLY.
    END.
    ELSE
        v-orig-cm-pct = reftable.val[11]:SCREEN-VALUE IN BROWSE {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL reftable.val[11] br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF reftable.val[11] IN BROWSE br_table /* CM% */
DO:
    /*
  IF v-orig-cm-pct NE reftable.val[11]:SCREEN-VALUE IN BROWSE {&browse-name} THEN
    probe.gross-profit:SCREEN-VALUE IN BROWSE {&browse-name} = 
      reftable.val[11]:SCREEN-VALUE IN BROWSE {&browse-name}.
      */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL reftable.val[11] br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF reftable.val[11] IN BROWSE br_table /* CM% */
DO:
  IF v-ceSellPrice = "F" THEN
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
  DEF VAR ld AS DEC NO-UNDO.


  IF cewhatif-cha EQ "PerMSF" THEN DO:
    ASSIGN
     hold-value = {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}
     ld         = DEC(hold-value).

    per-msf:
    DO ON ERROR UNDO, RETRY:
      RUN est/d-whatif.w (ROWID(probe), INPUT-OUTPUT ld) NO-ERROR.

      IF NOT ERROR-STATUS:ERROR THEN DO:
        IF ROUND(ld,2) NE ROUND(DEC(hold-value),2) THEN DO:
          {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(ld).
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


&Scoped-define SELF-NAME reftable.val[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL reftable.val[3] br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF reftable.val[3] IN BROWSE br_table /* Board% */
DO:
  IF LASTKEY NE -1 THEN DO:
    /*RUN valid-profit (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.*/

    RUN calc-fields NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL reftable.val[3] br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF reftable.val[3] IN BROWSE br_table /* Board% */
DO:
  lv-changed = "B".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME reftable.val[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL reftable.val[4] br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF reftable.val[4] IN BROWSE br_table /* Board!Contrib/M */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN calc-fields NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL reftable.val[4] br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF reftable.val[4] IN BROWSE br_table /* Board!Contrib/M */
DO:
  lv-changed = "BCM".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME reftable.val[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL reftable.val[5] br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF reftable.val[5] IN BROWSE br_table /* Board!Contrib$ */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN calc-fields NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL reftable.val[5] br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF reftable.val[5] IN BROWSE br_table /* Board!Contrib$ */
DO:
  lv-changed = "BC$".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

DEF VAR lv-col-hand AS HANDLE.
FIND FIRST ce-ctrl {sys/look/ce-ctrlw.i} NO-LOCK.
IF AVAIL ce-ctrl THEN
  v-ceSellPrice = ce-ctrl.sell-by.
IF v-ceSellPrice NE "F" THEN DO lv-int = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME} 
    WITH FRAME f-main:

  lv-col-hand = BROWSE br_table:GET-BROWSE-COLUMN(lv-int).

  IF  lv-col-hand:LABEL = "cm%" THEN DO:
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
  DEF VAR ld-marg% AS DEC NO-UNDO.
  DEF VAR ld-commc AS DEC NO-UNDO.
  DEF VAR ld-factc AS DEC NO-UNDO.
  DEF VAR ld-fullc AS DEC NO-UNDO.
  DEF VAR ld-price AS DEC NO-UNDO.
  DEF VAR ld-brd-m AS DEC NO-UNDO.
  DEF VAR ld-brd-% AS DEC NO-UNDO.
  DEF VAR ld-brdcm AS DEC NO-UNDO.
  DEF VAR ld-brdc$ AS DEC NO-UNDO.
  DEF VAR lv-changed2 LIKE lv-changed NO-UNDO.
  DEF VAR v-tmp-set-markup LIKE probe.set-chg NO-UNDO.
  DEF VAR v-tmp-value LIKE probe.set-chg NO-UNDO.
  DEF VAR lv-orig-changed AS CHAR NO-UNDO.
  DEF VAR v-freight AS DEC NO-UNDO.

  {cec/combasis.i}

  {sys/inc/ceround.i}
  v-freight = 0.
  FOR EACH est-summ
      WHERE est-summ.company EQ probe.company
        AND est-summ.est-no  EQ probe.est-no
        AND est-summ.e-num   EQ probe.line
        AND SUBSTR(est-summ.summ-tot,31) BEGINS "Freight"
      USE-INDEX est-qty NO-LOCK:
      v-freight = v-freight + est-summ.per-m.
  END.
  FIND FIRST ce-ctrl {sys/look/ce-ctrlw.i} NO-LOCK.

  IF lv-changed NE "" THEN
  DO WITH FRAME {&FRAME-NAME}:

    IF est.est-type EQ 6 AND probe.set-chg NE 0 AND vmclean2 THEN
       v-tmp-set-markup = probe.set-chg.

    ASSIGN
     lv-orig-changed = lv-changed
     v-com = probe.comm
     lv-changed2 = lv-changed
     ld-price    = DEC(lv-price)
     ld-marg%    = DEC(probe.market-price:SCREEN-VALUE IN BROWSE {&browse-name})
     ld-factc    = DEC(probe.fact-cost:SCREEN-VALUE IN BROWSE {&browse-name})
     ld-commc    = (ld-price - (IF v-basis EQ "G" THEN ld-factc ELSE 0)) *
                   (v-com / 100)   
     ld-fullc    = DEC(probe.full-cost:SCREEN-VALUE IN BROWSE {&browse-name}) -
                   ld-commc
     ld-brd-m    = DEC(reftable.val[2]:SCREEN-VALUE IN BROWSE {&browse-name})
     ld-brd-%    = DEC(reftable.val[3]:SCREEN-VALUE IN BROWSE {&browse-name})
     ld-brdcm    = DEC(reftable.val[4]:SCREEN-VALUE IN BROWSE {&browse-name})
     ld-brdc$    = DEC(reftable.val[5]:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF lv-changed EQ "S" THEN DO:
      ASSIGN
       ld-price = DEC(probe.sell-price:SCREEN-VALUE IN BROWSE {&browse-name})
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
          IF ce-ctrl.sell-by = "F" THEN DO:
          
            IF  DEC(reftable.val[11]:SCREEN-VALUE IN BROWSE {&browse-name}) > 0
               THEN
              v-pct = DEC(reftable.val[11]:SCREEN-VALUE IN BROWSE {&browse-name}).
            ELSE IF v-pct = 0 THEN
              v-pct = DEC(probe.gross-profit:SCREEN-VALUE IN BROWSE {&browse-name}).
            IF DEC(reftable.val[11]:SCREEN-VALUE IN BROWSE {&browse-name}) = 0 THEN
                reftable.val[11]:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(v-pct).
          END.
          ELSE
            v-pct = DEC(probe.gross-profit:SCREEN-VALUE IN BROWSE {&browse-name}).

          IF ce-ctrl.sell-by EQ "S" THEN lv-changed2 = "S".
          IF ce-ctrl.sell-by EQ "F" THEN lv-changed2 = "F".
        END.
      
        ELSE v-pct = DEC(probe.net-profit:SCREEN-VALUE IN BROWSE {&browse-name}).

        IF ll-use-margin THEN
          ASSIGN
           v-com       = 0
           lv-changed2 = "N"
           v-pct       = IF lv-changed EQ "M" THEN ld-marg% ELSE (v-pct + v-com).
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

    probe.full-cost:SCREEN-VALUE IN BROWSE {&browse-name} =
       STRING(ld-fullc,probe.full-cost:FORMAT IN BROWSE {&browse-name}) NO-ERROR.

    IF lv-changed NE "BC$" AND NOT ERROR-STATUS:ERROR THEN
      reftable.val[5]:SCREEN-VALUE IN BROWSE {&browse-name} =
          STRING(ld-brdc$,reftable.val[5]:FORMAT IN BROWSE {&browse-name}) NO-ERROR.

    IF lv-changed NE "BCM" AND NOT ERROR-STATUS:ERROR THEN
      reftable.val[4]:SCREEN-VALUE IN BROWSE {&browse-name} =
          STRING(ld-brdcm,reftable.val[4]:FORMAT IN BROWSE {&browse-name}) NO-ERROR.

    IF lv-changed NE "B" AND NOT ERROR-STATUS:ERROR THEN
      reftable.val[3]:SCREEN-VALUE IN BROWSE {&browse-name} =
          STRING(ld-brd-%,reftable.val[3]:FORMAT IN BROWSE {&browse-name}) NO-ERROR.

    IF lv-changed NE "M" AND lv-orig-changed NE "M" AND NOT ERROR-STATUS:ERROR THEN
      probe.market-price:SCREEN-VALUE IN BROWSE {&browse-name} =
          STRING(ld-marg%,probe.market-price:FORMAT IN BROWSE {&browse-name}) NO-ERROR.

    IF lv-changed NE "S" AND NOT ERROR-STATUS:ERROR THEN
      probe.sell-price:SCREEN-VALUE IN BROWSE {&browse-name} =
          STRING(ld-price,probe.sell-price:FORMAT IN BROWSE {&browse-name}) NO-ERROR.
        
    IF lv-changed NE "N" AND NOT ERROR-STATUS:ERROR THEN
    DO:
      IF probe.set-chg NE 0 AND est.est-type EQ 6 AND vmclean2 THEN
         probe.net-profit:SCREEN-VALUE IN BROWSE {&browse-name} =
             STRING((1 - (ld-fullc / ld-price)) * 100 - probe.set-chg) NO-ERROR. 
      ELSE
         probe.net-profit:SCREEN-VALUE IN BROWSE {&browse-name} =
             STRING((1 - (ld-fullc / ld-price)) * 100) NO-ERROR.
    END.

    IF lv-changed NE "G" AND NOT ERROR-STATUS:ERROR THEN
      probe.gross-profit:SCREEN-VALUE IN BROWSE {&browse-name} =
          STRING((1 - (ld-factc / ld-price)) * 100) NO-ERROR.

    ASSIGN
       probe.comm:SCREEN-VALUE IN BROWSE {&browse-name} =
                  STRING(v-com,probe.comm:FORMAT IN BROWSE {&browse-name})
       lv-changed = lv-changed2.

    IF ERROR-STATUS:ERROR                                                    OR
       TRIM(probe.full-cost:SCREEN-VALUE IN BROWSE {&browse-name})    EQ "?" OR
       TRIM(probe.gross-profit:SCREEN-VALUE IN BROWSE {&browse-name}) EQ "?" OR
       TRIM(probe.net-profit:SCREEN-VALUE IN BROWSE {&browse-name})   EQ "?" OR
       TRIM(probe.sell-price:SCREEN-VALUE IN BROWSE {&browse-name})   EQ "?" OR
       TRIM(reftable.val[3]:SCREEN-VALUE IN BROWSE {&browse-name})    EQ "?" OR
       TRIM(reftable.val[4]:SCREEN-VALUE IN BROWSE {&browse-name})    EQ "?" OR
       TRIM(reftable.val[5]:SCREEN-VALUE IN BROWSE {&browse-name})    EQ "?" OR
       ld-price GT 99999999.99                                              THEN DO:

      MESSAGE "Value(s) invalid, please try again" VIEW-AS ALERT-BOX ERROR.

      ASSIGN
       probe.full-cost:SCREEN-VALUE IN BROWSE {&browse-name}    = lv-fullc
       probe.net-profit:SCREEN-VALUE IN BROWSE {&browse-name}   = lv-nprof
       probe.gross-profit:SCREEN-VALUE IN BROWSE {&browse-name} = lv-gprof
       probe.sell-price:SCREEN-VALUE IN BROWSE {&browse-name}   = lv-price
       reftable.val[3]:SCREEN-VALUE IN BROWSE {&browse-name}    = lv-brd-%
       reftable.val[4]:SCREEN-VALUE IN BROWSE {&browse-name}    = lv-brdcm
       reftable.val[5]:SCREEN-VALUE IN BROWSE {&browse-name}    = lv-brdc$
       probe.comm:SCREEN-VALUE IN BROWSE {&browse-name} = lv-comm.

      IF lv-changed EQ "BC$" THEN
        APPLY "entry" TO reftable.val[5] IN BROWSE {&browse-name}.
      ELSE
      IF lv-changed EQ "BCM" THEN
        APPLY "entry" TO reftable.val[4] IN BROWSE {&browse-name}.
      ELSE
      IF lv-changed EQ "B" THEN
        APPLY "entry" TO reftable.val[3] IN BROWSE {&browse-name}.
      ELSE
      IF lv-changed EQ "M" THEN
        APPLY "entry" TO probe.market-price IN BROWSE {&browse-name}.
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
      voverall:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(voverall (0)).
      IF lv-changed2 NE "S" THEN 
        probe.gross-profit:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(display-gp (0)).
    END.
    RUN recalc-multicell.
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
  DEF OUTPUT PARAM op-combo AS LOG NO-UNDO.
  

  op-combo = AVAIL est AND est.est-type EQ 8.

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
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-summ-tot LIKE est-summ.summ-tot NO-UNDO.
  
  
  FOR EACH mclean:
    DELETE mclean.
  END.

  FOR EACH probe
      WHERE probe.company    EQ est.company
        AND probe.est-no     EQ est.est-no
        AND probe.probe-date NE ?
      NO-LOCK:

    FIND FIRST est-summ
        WHERE est-summ.company EQ probe.company
          AND est-summ.est-no  EQ probe.est-no
          AND est-summ.e-num   EQ probe.line
        USE-INDEX est-qty NO-LOCK NO-ERROR.
      
    DO WHILE AVAIL est-summ:
      CREATE mclean.
      ASSIGN
       mclean.rec-type = SUBSTR(est-summ.summ-tot,01,20)
       mclean.form-no  = INT(SUBSTR(est-summ.summ-tot,21,10))
       mclean.descr    = SUBSTR(est-summ.summ-tot,31).

      DO li = 1 TO 28:
        mclean.cost[li] = est-summ.per-m.

        FIND NEXT est-summ
            WHERE est-summ.company EQ probe.company
              AND est-summ.est-no  EQ probe.est-no
              AND est-summ.e-num   EQ probe.line
            USE-INDEX est-qty NO-LOCK NO-ERROR.
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
def buffer xprobe for probe.
DEF BUFFER bf-qhd FOR quotehd.
DEF BUFFER bf-notes FOR notes.

/* generating quote record - main code from  quote.a with new table */

def var j as int no-undo.
def var i as int no-undo.

def buffer bf-eb for eb.
DEF buffer bf-ef for ef.
DEF BUFFER b-probemk FOR reftable.

def var li-q-no LIKE quotehd.q-no NO-UNDO.
def var li-line as int no-undo .  /* for quoteitm.line */
def var ll-new-quote as log no-undo.
def var ll-first as log no-undo.
def var li-first-qty as int no-undo.  /* first qty for quoteitm */
DEF VAR li-prep-qty LIKE quotechg.prep-qty NO-UNDO.
def var li-cnt as int no-undo.
def var ld-cost as dec no-undo.
def var li-value as int no-undo.
DEF VAR lv-quo-price-char AS CHAR NO-UNDO.
DEF VAR v-tot-mat AS DEC NO-UNDO.
DEF VAR v-tot-lab AS DEC NO-UNDO.
DEF VAR v-nk-found AS LOG NO-UNDO.
DEF VAR lv-cust LIKE eb.cust-no NO-UNDO.
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
  lv-quo-price-char = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "M". */
  FIND FIRST bf-eb WHERE bf-eb.company EQ cocode
                     AND bf-eb.est-no  EQ est.est-no
                     AND bf-eb.cust-no GT ""
                   NO-LOCK NO-ERROR.
  IF AVAIL bf-eb THEN
      lv-cust = bf-eb.cust-no.
  ELSE
      lv-cust = "".
  RUN sys/ref/nk1look.p    (input cocode,
                            input "QUOPRICE", /* N-K-1 name */
                            input "C", /* data type */
                            input YES, /* use customer values */
                            input YES, /* shipto or vendor */
                            INPUT lv-cust,
                            input "", /* shipto value */
                            output lv-quo-price-char, /* returned value */
                            output v-nk-found /* was it found? */).
  IF NOT v-nk-found THEN
    lv-quo-price-char = "M".

  IF est.est-type EQ 8 THEN DO:
    FIND FIRST xjob NO-ERROR.
    IF NOT AVAIL xjob THEN DO:
      MESSAGE "You must calculate a combo estimate before creating a quote..."
              VIEW-AS ALERT-BOX ERROR.
      RETURN.
    END.
  END.

  assign
   cocode = est.company
   locode = est.loc.

  find last quotehd where quotehd.company = est.company and
                                 quotehd.loc = est.loc and
                                 quotehd.est-no = est.est-no no-error.
  IF AVAIL quotehd THEN DO:
     def var li-choice as int no-undo.
     run est/d-qtcnfm.w (output li-choice).
     if li-choice = 1 then /*update */ ll-new-quote = no.
     else if li-choice = 2 then /* new */ ll-new-quote = yes.
     else return. /* cancel */  
  end.
  else ll-new-quote = yes.
  li-q-no = if avail quotehd then quotehd.q-no else 1.

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

    FIND FIRST bf-eb
        WHERE bf-eb.company EQ w-probeit.company
          AND bf-eb.est-no  EQ w-probeit.est-no
          AND bf-eb.cust-no EQ w-probeit.cust-no
          AND bf-eb.part-no EQ w-probeit.part-no
          AND bf-eb.form-no NE 0
        NO-LOCK NO-ERROR.

    IF NOT AVAIL bf-eb THEN DO:
      FIND FIRST bf-eb
          WHERE bf-eb.company EQ w-probeit.company
            AND bf-eb.est-no  EQ w-probeit.est-no
            AND bf-eb.form-no EQ 0
          NO-LOCK NO-ERROR.
      FIND FIRST eb
          WHERE eb.company EQ w-probeit.company
            AND eb.est-no  EQ w-probeit.est-no
            AND eb.form-no NE 0
          NO-LOCK NO-ERROR.
    END.

    FIND FIRST quotehd
        WHERE quotehd.company EQ cocode
          AND quotehd.loc     EQ locode
          AND quotehd.q-no    EQ li-q-no
        NO-ERROR.

    IF FIRST-OF(w-probeit.cust-no) THEN DO:
      IF ll-new-quote OR NOT FIRST(w-probeit.cust-no) OR NOT AVAIL quotehd THEN DO:
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
      FIND FIRST shipto
          WHERE shipto.company EQ cocode
            AND shipto.cust-no EQ quotehd.cust-no
            AND shipto.ship-id EQ quotehd.ship-id
          NO-LOCK NO-ERROR.
      FOR EACH soldto
          WHERE soldto.company EQ cocode
            AND soldto.cust-no EQ quotehd.cust-no
          NO-LOCK
          BREAK BY soldto.sold-no DESC:
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
/*          IF AVAIL bf-qhd THEN                */
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
      FIND FIRST bf-ef
          WHERE bf-ef.company  EQ est.company
            AND bf-ef.est-no   EQ est.est-no
            AND (bf-ef.form-no EQ bf-eb.form-no OR est.est-type EQ 6)
          NO-LOCK NO-ERROR.

      IF ll-new-quote OR NOT AVAIL quoteitm THEN DO:
        FOR EACH quoteitm
            WHERE quoteitm.company EQ quotehd.company
              AND quoteitm.loc     EQ quotehd.loc
              AND quoteitm.q-no    EQ quotehd.q-no
            NO-LOCK
            BY quoteitm.line DESC:
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
        FIND FIRST item
            WHERE item.company EQ cocode
              AND item.i-no    EQ bf-ef.board
            NO-LOCK NO-ERROR.
        IF AVAIL ITEM THEN
          quoteitm.i-dscr = IF item.i-name   GT "" THEN item.i-name   ELSE
                            IF item.est-dscr GT "" THEN item.est-dscr ELSE
                            item.i-dscr.
      END. /* if brd-dscr */
    END.

    FIND FIRST quoteqty
        WHERE quoteqty.company EQ quoteitm.company
          AND quoteqty.loc     EQ quoteitm.loc
          AND quoteqty.q-no    EQ quoteitm.q-no
          AND quoteqty.LINE    EQ quoteitm.line
          AND quoteqty.qty     EQ w-probeit.bl-qty
          AND quoteqty.rels    EQ INT(w-probeit.freight)
        NO-ERROR.

    IF ll-new-quote OR NOT AVAIL quoteqty THEN CREATE quoteqty.

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

      FOR EACH bf-ef
          WHERE bf-ef.company EQ quotehd.company
            AND bf-ef.est-no  EQ quotehd.est-no
          NO-LOCK:

        li-prep-qty = 0.

        IF est.est-type GE 7 THEN
        FOR EACH bf-eb FIELDS(bl-qty)
            WHERE bf-eb.company EQ bf-ef.company
              AND bf-eb.est-no  EQ bf-ef.est-no
              AND bf-eb.form-no EQ bf-ef.form-no
              AND bf-eb.cust-no EQ w-probeit.cust-no
            NO-LOCK:
          li-prep-qty = li-prep-qty + bf-eb.bl-qty.
        END.

        ELSE li-prep-qty = w-probeit.bl-qty.

        FOR EACH est-prep
            WHERE est-prep.company EQ bf-ef.company
              AND est-prep.est-no  EQ bf-ef.est-no
              AND est-prep.s-num   EQ bf-ef.form-no
              AND est-prep.simon   EQ "S"
            NO-LOCK:

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
           quotechg.bill     = if est-prep.ml then "M" else "L"
           quotechg.amt      = IF ceprepprice-chr EQ "Profit" THEN
                                  est-prep.qty * est-prep.cost /
                                  (1 - (est-prep.mkup / 100)) *
                                  (if est-prep.amtz ne 0 then est-prep.amtz / 100 else 1)
                               ELSE /*Cost Markup*/
                                  est-prep.qty * est-prep.cost *
                                  (1 + (est-prep.mkup / 100)) *
                                  (if est-prep.amtz ne 0 then est-prep.amtz / 100 else 1)  
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

IF AVAIL quotechg THEN FIND CURRENT quotechg NO-LOCK NO-ERROR.
IF AVAIL quoteqty THEN FIND CURRENT quoteqty NO-LOCK NO-ERROR.
IF AVAIL quoteitm THEN FIND CURRENT quoteitm NO-LOCK NO-ERROR.
IF AVAIL quotehd  THEN DO:
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
  DEF INPUT PARAM ip-q-no LIKE quotehd.q-no NO-UNDO.


  /* delete quoteitm for old part-no */
  DISABLE TRIGGERS FOR LOAD OF quoteitm.
  FOR EACH quoteitm
       WHERE quoteitm.company EQ cocode
         AND quoteitm.loc     EQ locode
         AND quoteitm.q-no    EQ ip-q-no:
    FIND FIRST w-probeit WHERE w-probeit.part-no = quoteitm.part-no NO-LOCK NO-ERROR.
    IF NOT AVAIL w-probeit THEN DELETE quoteitm.
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
  DEF VAR li AS INT NO-UNDO.
  DEF VAR li1 AS INT NO-UNDO.

  
  FIND LAST est-summ
      WHERE est-summ.company EQ est.company
        AND est-summ.est-no  EQ est.est-no
      USE-INDEX est-qty NO-LOCK NO-ERROR.
  li1 = IF AVAIL est-summ THEN est-summ.eqty ELSE 0.

  FOR EACH mclean:
    DO li = 1 TO 28:
      FOR EACH probe
          WHERE probe.company    EQ est.company
            AND probe.est-no     EQ est.est-no
            AND probe.probe-date EQ TODAY
            AND probe.est-qty    EQ qtty[li]
            AND probe.freight    EQ IF est.est-type LE 6 THEN rels[li] ELSE 1
          NO-LOCK
          BY probe.probe-time DESC:

        CREATE est-summ.
        ASSIGN
         est-summ.company  = probe.company
         est-summ.est-no   = probe.est-no
         li1               = li1 + 1
         est-summ.eqty     = li1
         est-summ.summ-tot = STRING(mclean.rec-type,"x(20)")     +
                             STRING(mclean.form-no,"9999999999") +
                             mclean.descr
         est-summ.e-num    = probe.line
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

    FOR EACH probe
        WHERE probe.company    EQ xest.company
          AND probe.est-no     EQ xest.est-no
          AND probe.probe-date EQ TODAY
          AND probe.est-qty    EQ qtty[li]
          AND probe.freight    EQ rels[li]
        NO-LOCK
        BY probe.probe-time DESC:
      LEAVE.
    END.

    IF AVAIL probe THEN RUN cec/pr4-mcl1.p (ROWID(probe)).
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
   DEF INPUT PARAMETER ip-search AS CHAR NO-UNDO.
   DEF OUTPUT PARAMETER op-tmp-dir AS CHAR NO-UNDO.

   DEF VAR viDirCount AS INT NO-UNDO.

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
  DEF OUTPUT PARAM op-est-type AS INT.


  op-est-type = IF AVAIL est THEN est.est-type ELSE 0.

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
  DEF VAR lv-price AS DEC DECIMALS 10 NO-UNDO.
  DEF VAR lv-subprice AS DEC DECIMALS 10 NO-UNDO.
  DEF VAR lv-subquantity AS DEC DECIMALS 10 NO-UNDO.
  DEF BUFFER b-eb FOR eb.
  
  {est/checkuse.i}

  RUN save-fields.

  DO WITH FRAME {&FRAME-NAME}:

    IF est.est-type NE 8 THEN
      FOR EACH quotehd OF est NO-LOCK,
          
          EACH quoteitm OF quotehd NO-LOCK,
         
          EACH quoteqty OF quoteitm
          WHERE quoteqty.qty EQ probe.est-qty
          NO-LOCK
      
          BY quoteqty.q-no DESC
          BY quoteqty.qty  DESC:
         
          ASSIGN
           lv-changed = "S"
           lv-price   = IF quoteqty.uom EQ "EA" THEN (quoteqty.price * 1000)
                        ELSE
                        IF quoteqty.uom EQ "MSF" THEN (quoteqty.price * ROUND(quoteqty.tot-lbs / 1000,2) / round(quoteqty.qty / 1000,2))
                        ELSE quoteqty.price
           probe.sell-price:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(lv-price).
         
          RUN calc-fields NO-ERROR.
          IF ERROR-STATUS:ERROR THEN RETURN.
         
          RUN dispatch ("update-record").
          LEAVE.
       END.
    ELSE /*combo/tandem*/
    DO:
       FOR EACH quotehd OF est NO-LOCK,
          EACH b-eb OF est WHERE
               NO-LOCK,
          FIRST quoteitm OF quotehd WHERE
                quoteitm.part-no EQ b-eb.part-no AND
                quoteitm.i-no EQ b-eb.stock-no
               NO-LOCK,
          
          FIRST quoteqty OF quoteitm WHERE
               quoteqty.qty EQ b-eb.bl-qty
          NO-LOCK
      
           BREAK BY quoteqty.q-no DESC
                 BY quoteqty.qty  DESC:

          ASSIGN
             lv-price = IF quoteqty.uom EQ "EA" THEN (quoteqty.price * 1000)
                        ELSE
                        IF quoteqty.uom EQ "MSF" THEN (quoteqty.price * ROUND(quoteqty.tot-lbs / 1000,2) / round(quoteqty.qty / 1000,2))
                        ELSE quoteqty.price
             lv-subprice = lv-subprice + (lv-price * quoteqty.qty)
             lv-subquantity = lv-subquantity + quoteqty.qty.

          IF LAST-OF(quoteqty.q-no) THEN
             LEAVE.
       END.

       ASSIGN
          lv-changed = "S".

       IF lv-subquantity NE 0 THEN
          probe.sell-price:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(ROUND(lv-subprice / lv-subquantity,2)).
       ELSE
          probe.sell-price:SCREEN-VALUE IN BROWSE {&browse-name} = "0".
         
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
  DEF VAR ld AS DEC NO-UNDO.
  DEF VAR ld-tot-fact AS DEC NO-UNDO.
  DEF VAR ld-tot-full AS DEC NO-UNDO.
  DEF VAR ld-tot-pric AS DEC NO-UNDO.
  DEF VAR lc-calling-progs AS CHAR NO-UNDO.
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
  probe.comm = DEC(probe.comm:SCREEN-VALUE IN BROWSE {&browse-name}).

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
    IF AVAIL probe-ref THEN
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

  FOR EACH probeit
      WHERE probeit.company EQ probe.company
        AND probeit.est-no  EQ probe.est-no
        AND probeit.line    EQ probe.line
      NO-LOCK:
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
      /* If this is a result of update to items, don't 
         adjust probeit. Should only do this if probe has
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
  IF v-ceSellPrice = "F" THEN DO:
      /* For type 'F', multicell, make correction to gross-profit 
         after it was updated by the db trigger on probe */
      FIND CURRENT probe.

      IF dec(probe.gross-profit:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})NE probe.gross-profit THEN DO:
        ASSIGN probe.gross-profit:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(probe.gross-profit).
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
  def var char-hdl as cha no-undo.
  def var phandle as handle no-undo.
  
  
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.


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
    probe.do-quote:SCREEN-VALUE IN BROWSE {&browse-name} = "Y".
    APPLY "entry" TO probe.market-price IN BROWSE {&browse-name}.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
 
  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT AVAIL est THEN RETURN "adm-error".

  FOR EACH probe NO-LOCK
      WHERE probe.company EQ est.company
        AND probe.est-no  EQ est.est-no:
    FIND FIRST reftable
        WHERE reftable.reftable EQ "probe.board"
          AND reftable.company  EQ probe.company
          AND reftable.loc      EQ ""
          AND reftable.code     EQ probe.est-no
          AND reftable.code2    EQ STRING(probe.line,"9999999999")
        NO-ERROR.
    IF NOT AVAIL reftable THEN DO:
      CREATE reftable.
      ASSIGN
       reftable.reftable = "probe.board"
       reftable.company  = probe.company
       reftable.loc      = ""
       reftable.code     = probe.est-no
       reftable.code2    = STRING(probe.line,"9999999999").
    END.
    ASSIGN
     reftable.val[2] = reftable.val[1] / (probe.est-qty / 1000)
     reftable.val[3] = reftable.val[2] / probe.sell-price * 100
     reftable.val[4] = probe.sell-price - reftable.val[2]
     reftable.val[5] = reftable.val[4] * (probe.est-qty / 1000).
    FIND CURRENT reftable NO-LOCK NO-ERROR.
  END.
  
  find xest where recid(xest) = recid(est) NO-LOCK NO-ERROR.
  find xef where recid(xef) = recid(ef) NO-LOCK NO-ERROR.
  find xeb where recid(xeb) = recid(eb) NO-LOCK NO-ERROR.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ll-use-margin = NO.

  IF cerunc EQ "Fibre" THEN
    RUN est/usemargin.p (ROWID(est), OUTPUT ll-use-margin).

  ASSIGN
     probe.gross-profit:VISIBLE IN BROWSE {&browse-name} = NOT(ll-use-margin AND cerunf = "Fibre" AND cerunc = "Fibre")
     probe.comm:VISIBLE IN BROWSE {&browse-name} = NOT(probe.gross-profit:VISIBLE IN BROWSE {&browse-name}).

  find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "SETPRINT"
                      no-lock no-error.
  vmclean2 = AVAIL sys-ctrl AND sys-ctrl.char-fld eq "McLean" AND est.est-type EQ 6.
  if vmclean2 then v-match-up = sys-ctrl.dec-fld.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
    RUN valid-profit (probe.market-price:HANDLE IN BROWSE {&browse-name}) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

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
DEFINE VARIABLE v-prgmname AS CHAR INIT "p-probe." NO-UNDO.
DEFINE VARIABLE num-groups AS INTEGER NO-UNDO.
DEF VAR g_groups AS CHAR NO-UNDO.

FIND FIRST b-prgrms WHERE
     b-prgrms.prgmname = v-prgmname AND
     b-prgrms.DIR_group = ""
     NO-LOCK NO-ERROR.

IF AVAILABLE b-prgrms THEN
DO:
  FOR EACH usergrps NO-LOCK:
      IF CAN-DO(usergrps.users,USERID("ASI")) THEN
         g_groups = g_groups + usergrps.usergrps + ",".
  END.

  DO num-groups = 1 TO NUM-ENTRIES(g_groups):
     IF NOT CAN-DO(b-prgrms.can_update,ENTRY(num-groups,g_groups)) THEN
        NEXT.
    
     IF NOT v-can-update AND CAN-DO(b-prgrms.can_update,ENTRY(num-groups,g_groups)) THEN
        v-can-update = YES.
  END.
  
  IF NOT v-can-update AND CAN-DO(b-prgrms.can_update,USERID("ASI")) THEN
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
  DEF BUFFER b-probe FOR probe.

  FOR EACH b-probe
      WHERE b-probe.company EQ probe.company
        AND b-probe.est-no  EQ probe.est-no
      NO-LOCK
      BY b-probe.probe-date
      BY b-probe.est-qty:

    IF b-probe.LINE LT 100 THEN
    DO:
       RUN get-dir-proc(INPUT trim(est.est-no) + ".s" + string(b-probe.line,"99"),
                     OUTPUT tmp-dir).

       if opsys eq "unix" then 
         unix silent cp  value(tmp-dir + trim(est.est-no) + ".s" + string(b-probe.line,"99"))
                         value(lv-cebrowse-dir + trim(est.est-no) + ".p" + string(b-probe.line,"99")).
       else
         dos silent copy value(tmp-dir + trim(est.est-no) + ".s" + string(b-probe.line,"99"))
                         value(lv-cebrowse-dir + trim(est.est-no) + ".p" + string(b-probe.line,"99")).

       RUN get-dir-proc(INPUT trim(est.est-no) + ".a" + string(b-probe.line,"99"),
                        OUTPUT tmp-dir).

       if opsys = "unix" then
          unix silent cat value(tmp-dir + trim(est.est-no) + ".a"
                                                    + string(b-probe.line,"99")) >>
                          value(lv-cebrowse-dir + trim(est.est-no) + ".p"
                                                    + string(b-probe.line,"99")).
       else /* if opsys = "MSDOS" then */
          dos silent type value(tmp-dir + trim(est.est-no) + ".a"
                                                    + string(b-probe.line,"99")) >>
                          value(lv-cebrowse-dir + trim(est.est-no) + ".p"
                                                    + string(b-probe.line,"99")).
    END.
    ELSE
    DO:
       RUN get-dir-proc(INPUT trim(est.est-no) + ".s" + string(b-probe.line,"999"),
                        OUTPUT tmp-dir).

       if opsys eq "unix" then 
         unix silent cp  value(tmp-dir + trim(est.est-no) + ".s" + string(b-probe.line,"999"))
                         value(lv-cebrowse-dir + trim(est.est-no) + ".p" + string(b-probe.line,"999")).
       else
         dos silent copy value(tmp-dir + trim(est.est-no) + ".s" + string(b-probe.line,"999"))
                         value(lv-cebrowse-dir + trim(est.est-no) + ".p" + string(b-probe.line,"999")).

       RUN get-dir-proc(INPUT trim(est.est-no) + ".a" + string(b-probe.line,"999"),
                        OUTPUT tmp-dir).

       if opsys = "unix" then
          unix silent cat value(tmp-dir + trim(est.est-no) + ".a"
                                + string(b-probe.line,"999")) >>
                          value(lv-cebrowse-dir + trim(est.est-no) + ".p"
                                + string(b-probe.line,"999")).
       else /* if opsys = "MSDOS" then */
          dos silent type value(tmp-dir + trim(est.est-no) + ".a"
                                + string(b-probe.line,"999")) >>
                          value(lv-cebrowse-dir + trim(est.est-no) + ".p"
                                + string(b-probe.line,"999")).
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
  IF NOT AVAIL box-design-hdr THEN DO:
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
  DEF INPUT param ip-dest AS int NO-UNDO.
  DEF INPUT PARAM ip-font AS INT NO-UNDO.
  DEF INPUT PARAM ip-ornt AS cha NO-UNDO.

  DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
  DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
  DEFINE VARIABLE result AS LOGICAL NO-UNDO.

  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR lv-input AS cha FORM "x(180)" NO-UNDO.
  DEF VAR v-box-input AS cha NO-UNDO.

  DEF VAR ls-fax-file AS cha NO-UNDO.
  DEF VAR init-dir AS cha NO-UNDO.
  DEF VAR lv-pdf-file AS cha NO-UNDO.
  DEF VAR list-name AS cha NO-UNDO.
  DEF VAR ret-code AS INT NO-UNDO.
  DEF VAR ls-mail-file2 AS cha NO-UNDO.

  {est/checkuse.i}

  FIND FIRST box-design-hdr
      where box-design-hdr.design-no eq 0
        and box-design-hdr.company = eb.company 
        AND box-design-hdr.est-no    eq eb.est-no
        and box-design-hdr.form-no   eq eb.form-no
        and box-design-hdr.blank-no  eq eb.blank-no
        no-lock no-error.
  IF NOT AVAIL box-design-hdr THEN DO:
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

  OUTPUT TO VALUE(ls-outfile).
  IF ip-dest EQ 1 THEN PUT "<PRINTER?></PROGRESS><P11>".  /*<REVIEW>*/
  ELSE IF ip-dest EQ 2 THEN PUT "<PREVIEW=ZoomToWidth></PROGRESS><P11>".  /*<REVIEW>*/
  ELSE IF ip-dest EQ 4 THEN DO:
     ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".tif".
     PUT UNFORMATTED "<PRINTER?><EXPORT=" Ls-fax-file ",BW></PROGRESS><P11>".
  END.        
  ELSE IF ip-dest = 5 THEN DO:
      ASSIGN
         init-dir = v-dir
         lv-pdf-file = v-dir + "Est" + TRIM(est.est-no).
      PUT "<PREVIEW><PDF-LEFT=5mm><PDF-TOP=10mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf></PROGRESS><P11>" FORMAT "x(100)".
  END.
  OUTPUT CLOSE.

  IF probe.LINE LT 100 THEN
  DO:
     RUN get-dir-proc(INPUT TRIM(xest.est-no) + ".p" + STRING(probe.line,"99"),
                      OUTPUT tmp-dir).
     DOS SILENT TYPE VALUE(tmp-dir + TRIM(xest.est-no) + ".p" + STRING(probe.line,"99"))
                      >> VALUE(ls-outfile). /*page skip problem */
  END.
  ELSE
  DO:
     RUN get-dir-proc(INPUT TRIM(xest.est-no) + ".p" + STRING(probe.line,"999"),
                      OUTPUT tmp-dir).
     DOS SILENT TYPE VALUE(tmp-dir + TRIM(xest.est-no) + ".p" + STRING(probe.line,"999"))
                      >> VALUE(ls-outfile). /*page skip problem */
  END.

  lv-input = "".
  
  IF v-prt-box THEN RUN printBoxImage.

  IF v-prt-note THEN DO:
    OUTPUT TO VALUE(ls-outfile) APPEND /*PAGE-SIZE 64 */ .
    RUN print-notes.
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
               run custom/asifax.p ("",ls-fax-file,"",
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
  {custom/notesdef.i}
  DEF VAR v-inst2 AS cha EXTENT 200 NO-UNDO.    
  DEF VAR v-dept-inst AS cha FORM "x(80)" EXTENT 200 NO-UNDO.
  DEF VAR v-note-length AS INT INIT 80 NO-UNDO.
  DEF VAR lv-k AS INT NO-UNDO.
  
  /*determine number of lines needed*/
  ASSIGN v-tmp-lines = 0
       j = 0
       K = 0
       lv-got-return = 0.

  FOR EACH notes WHERE notes.rec_key = xest.rec_key and
      v-prt-note and notes.note_code >= v-from-dept and
      notes.note_code <= v-to-dept NO-LOCK:
    
    IF v-prev-note-rec <> ? AND
       v-prev-note-rec <> RECID(notes) THEN v-prev-extent = k.

    DO i = 1 TO LENGTH(notes.note_text) :        
       IF i - j >= lv-line-chars THEN ASSIGN j = i
                                             lv-got-return = lv-got-return + 1.
              
       v-tmp-lines = ( i - j ) / lv-line-chars.
       {SYS/INC/ROUNDUP.I v-tmp-lines}
       k = v-tmp-lines + lv-got-return +
       IF (v-prev-note-rec <> RECID(notes) AND v-prev-note-rec <> ?) THEN v-prev-extent ELSE 0.
    
       IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13) THEN                
       do:
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
     v-dept-inst[i] = v-inst2[i].
     PUT v-dept-inst[i] AT 2 SKIP.
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
  def var i as int no-undo.
  def var j as int no-undo.
  def var call_id as recid no-undo.  
  def var fil_id as recid no-undo.
  def var lv-error as log no-undo.
  def var v-vend-no   like e-item-vend.vend-no init "".
  DEF var v-vend-list AS CHAR NO-UNDO.
  DEFINE VARIABLE lv-ef-recid AS RECID NO-UNDO.

  ASSIGN
    lv-ef-recid = recid(ef)
    tmp-dir = lv-cebrowse-dir.

  {cec/print4p.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print42 B-table-Win 
PROCEDURE print42 :
/*------------------------------------------------------------------------------
  Purpose:   what if for (Corr. set - est.est-type = 6)  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-tmp-dir LIKE tmp-dir NO-UNDO.

  {sys/FORM/r-top.i}
  def var i as int no-undo.
  def var j as int no-undo.
  def var call_id as recid no-undo.  
  def var fil_id as recid no-undo.
  def var xxx as DEC no-undo.
  def var zzz as DEC no-undo.
  def var x as int no-undo.  
  def var tmpstore as cha no-undo.
  def var lv-error as log no-undo.
  DEF VAR module AS cha NO-UNDO.

  ASSIGN
     vprint = YES
     module = str-tit
     tmp-dir = ip-tmp-dir
     v-update-qty-gsa = NO
     ld-gsa-brd = 0
     ld-gsa-mat = 0
     ld-gsa-lab = 0.

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
          DOS SILENT TYPE VALUE(tmp-dir + TRIM(xest.est-no) + '-01.b' + STRING(probe.line,'99')) >> VALUE(ls-outfile).
       END.
       ELSE
       DO:
          RUN get-dir-proc(INPUT TRIM(xest.est-no) + '-01.b' + STRING(probe.line,'999'),
                           OUTPUT tmp-dir).
          DOS SILENT TYPE VALUE(tmp-dir + TRIM(xest.est-no) + '-01.b' + STRING(probe.line,'999')) >> VALUE(ls-outfile).
       END.
    END.
    ELSE
    DO:
       IF probe.LINE LT 100 THEN
       DO:
          RUN get-dir-proc(INPUT TRIM(xest.est-no) + '.b' + STRING(probe.line,'99'),
                           OUTPUT tmp-dir).
          DOS SILENT TYPE VALUE(tmp-dir + TRIM(xest.est-no) + '.b' + STRING(probe.line,'99')) >> VALUE(ls-outfile).
       END.
       ELSE
       DO:
          RUN get-dir-proc(INPUT TRIM(xest.est-no) + '.b' + STRING(probe.line,'999'),
                           OUTPUT tmp-dir).
          DOS SILENT TYPE VALUE(tmp-dir + TRIM(xest.est-no) + '.b' + STRING(probe.line,'999')) >> VALUE(ls-outfile).
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
  DEF VAR v-print-fmt AS cha NO-UNDO.
  DEF VAR is-xprint-form AS LOG NO-UNDO.
  DEF VAR v-error AS LOG NO-UNDO.
  DEF VAR v-cinput AS cha FORM "x(255)" NO-UNDO.

  DEF VAR lv-dest AS int NO-UNDO.
  DEF VAR lv-font AS INT NO-UNDO.
  DEF VAR lv-ornt AS cha NO-UNDO.
  DEF VAR lv-lines AS INT NO-UNDO.
  DEF VAR list-name AS cha NO-UNDO.
  DEF VAR init-dir AS cha NO-UNDO.
  DEF VAR ls-fax-file AS cha NO-UNDO.
  DEF VAR ret-code AS INT NO-UNDO.
  DEF VAR ls-mail-file2 AS cha NO-UNDO.
  DEF VAR lv-dir AS CHAR NO-UNDO.
  DEF VAR v-probe-fmt AS CHAR NO-UNDO.
  
  {est/checkuse.i}
  IF ipPrompt THEN DO:
    RUN est/d-estprt.w (OUTPUT v-prt-note,OUTPUT v-prt-box,OUTPUT v-from-dept,
                        OUTPUT v-to-dept,OUTPUT lv-dest,OUTPUT lv-font,
                        OUTPUT lv-ornt,OUTPUT lv-lines,OUTPUT v-error, OUTPUT v-print-cm).
    IF v-error THEN RETURN ERROR.

  END. /* if ipprompt */
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
    lv-lines = 65.

  ASSIGN
     v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999"
     ls-outfile = lv-cebrowse-dir + trim(est.est-no) + ".p" + string(probe.line,v-probe-fmt).

  FIND CURRENT xest.
  IF xest.est-type LT 7 THEN RUN cec/probeu1.p (RECID(probe)).
  FIND CURRENT xest NO-LOCK.

  v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999".

  RUN get-dir-proc(INPUT trim(est.est-no) + ".s" + string(probe.line,v-probe-fmt),
                   OUTPUT lv-dir).

  if opsys eq "unix" THEN
     unix silent cp  value(lv-dir + trim(est.est-no) + ".s" + string(probe.line,v-probe-fmt))
                     value(ls-outfile).
  ELSE DO:
    find first sys-ctrl where sys-ctrl.company eq cocode
                        and sys-ctrl.name eq "CEPrint" no-lock no-error.
    is-xprint-form = avail sys-ctrl and sys-ctrl.char-fld ne 'Text'.
    find first sys-ctrl where sys-ctrl.company eq cocode
                        and sys-ctrl.name eq "JOBCARDC" no-lock no-error.
    if avail sys-ctrl THEN ASSIGN v-print-fmt = sys-ctrl.char-fld.
    ELSE v-print-fmt = "".
     i = 0 . 
     IF is-xprint-form THEN lv-lines = 66.
     OUTPUT TO VALUE(ls-outfile) PAGE-SIZE VALUE(lv-lines). /* create .x file with page size */

     input from value(lv-dir + trim(est.est-no) + ".s" + string(probe.line,v-probe-fmt)) NO-ECHO.
     repeat:
           v-cinput = "".
           import unformatted v-cinput.
           if v-cinput eq "" then put skip(1).
           else do:
               IF LOOKUP(SUBSTRING(v-cinput,1,2),"01,02,03,04,05,06,07,08,09,10,11,12") > 0 and
                  SUBSTRING(v-cinput,3,1) EQ "/" AND
                  SUBSTRING(v-cinput,6,1) EQ "/"    THEN PAGE. /*seperate page per form*/  
               put unformatted v-cinput skip.
               i = i + 1 .
           END.
     end.
     
     IF NOT is-xprint-form AND v-prt-note THEN RUN print-notes.
    input close.
    IF PAGE-NUM = 1 THEN
    OUTPUT CLOSE.
    ELSE IF v-prt-box THEN OUTPUT CLOSE.
  END.
  
  IF is-xprint-form THEN RUN print-box-est (lv-dest,lv-font,lv-ornt).
  ELSE DO:
     list-name = ls-outfile.
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
  DEFINE INPUT PARAMETER ip-old-price AS DEC NO-UNDO.
    
  DEF VAR ld-commc AS DEC NO-UNDO.
  DEF VAR ld-factc AS DEC NO-UNDO.
  DEF VAR ld-fullc AS DEC NO-UNDO.
  DEF VAR ld-price AS DEC NO-UNDO.

  {cec/combasis.i}
  {sys/inc/ceround.i}

  DO WITH FRAME {&FRAME-NAME}:
    ld-price = 0.
    FOR EACH probeit FIELDS(sell-price yld-qty)
        WHERE probeit.company EQ probe.company
          AND probeit.est-no  EQ probe.est-no
          AND probeit.line    EQ probe.line
        NO-LOCK:
    
      ld-price = ld-price +
                 (probeit.sell-price * (probeit.yld-qty / probe.est-qty)).
    END.
  
    ASSIGN
       lv-changed = IF ld-price EQ ip-old-price THEN "" ELSE "S"
       probe.sell-price:SCREEN-VALUE IN BROWSE {&browse-name} =
           STRING(ld-price,probe.sell-price:FORMAT IN BROWSE {&browse-name})
       lv-price = STRING(ip-old-price).

    RUN calc-fields.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recalc-multicell B-table-Win 
PROCEDURE recalc-multicell :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* Special logic for multicell */
DEF VAR v-mr AS DEC NO-UNDO.
DEF VAR v-cmah AS DEC NO-UNDO.
DEF VAR v-cmoh AS DEC NO-UNDO.

v-mr = calc-cm().
reftable.val[8]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(v-mr).
v-cmah = calc-cmah().
reftable.val[9]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(v-cmah).
v-cmoh = calc-cmoh().
reftable.val[10]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(v-cmoh).
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
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.


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
 def var lv-eb-recid as recid no-undo.
  def var lv-ef-recid as recid no-undo.
  DEF VAR lv-probe-line LIKE probe.LINE NO-UNDO.
  DEF VAR tmp-outfile AS cha NO-UNDO.
  DEF VAR viewfile AS cha NO-UNDO.
  DEF VAR l-new-record AS LOG NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF BUFFER bf-probe FOR probe.
  
  IF AVAIL est THEN
  FIND FIRST est-summ WHERE est-summ.company = est.company 
                        AND est-summ.est-no = est.est-no 
                      NO-LOCK NO-ERROR.

  IF NOT AVAIL est-summ THEN
      l-new-record = TRUE.

  {est/checkuse.i}
  IF AVAIL probe THEN
    lv-probe-line = probe.LINE.
  FIND CURRENT est.
  find xef where recid(xef) = recid(ef).
  find xeb where recid(xeb) = recid(eb).
  vprint = yes.
  lv-eb-recid = recid(eb).
  lv-ef-recid = recid(ef).
  
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
        AND mach.m-code EQ est-op.m-code,
      FIRST reftable NO-LOCK
      WHERE reftable.reftable EQ "mach.obsolete"
        AND reftable.company  EQ mach.company
        AND reftable.loc      EQ mach.loc
        AND reftable.code     EQ mach.m-code
        AND reftable.val[1]   EQ 1:
    MESSAGE "Machine: " + TRIM(mach.m-code) +
            " is obsolete, please replace to complete calculation..."
        VIEW-AS ALERT-BOX ERROR.
    RETURN.
  END.

  IF est.est-type EQ 8 THEN RUN cec/com/print4.p NO-ERROR.

  ELSE DO:
    FIND FIRST probe WHERE probe.company = est.company
                       AND probe.est-no = est.est-no
                     NO-LOCK NO-ERROR.


    /*IF AVAIL probe THEN RUN est/d-probeu.w (OUTPUT lv-override). */
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
  for each est-op where est-op.company = xest.company and
                        est-op.est-no = xest.est-no and est-op.line > 500 
                        exclusive-lock :
           CREATE tt-est-op.
           BUFFER-COPY est-op TO tt-est-op.
           delete est-op.  
  end.

  RUN release-shared-buffers.

  SESSION:SET-WAIT-STATE("").

  find eb where recid(eb) = lv-eb-recid no-lock.
  find ef where recid(ef) = lv-ef-recid no-lock.
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
  DEF VAR lv-probe-line LIKE probe.LINE NO-UNDO.
  DEF VAR tmp-outfile AS cha NO-UNDO.
  DEF VAR viewfile AS cha NO-UNDO.
  DEF VAR l-new-record AS LOG NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF BUFFER bf-probe FOR probe.
  DEFINE VARIABLE ll-return AS LOGICAL     NO-UNDO.
  
  IF AVAIL est THEN
  FIND FIRST est-summ WHERE est-summ.company = est.company 
                        AND est-summ.est-no = est.est-no 
                      NO-LOCK NO-ERROR.

  IF NOT AVAIL est-summ THEN
      l-new-record = TRUE. 
  
  IF NOT checkNCBrd() THEN RETURN.

  {est/checkuse.i}
  IF AVAIL probe THEN
    lv-probe-line = probe.LINE.
  FIND CURRENT est.
  find xef where recid(xef) = recid(ef).
  find xeb where recid(xeb) = recid(eb).
  vprint = yes.
  lv-eb-recid = recid(eb).
  lv-ef-recid = recid(ef).
  
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
        AND mach.m-code EQ est-op.m-code,
      FIRST reftable NO-LOCK
      WHERE reftable.reftable EQ "mach.obsolete"
        AND reftable.company  EQ mach.company
        AND reftable.loc      EQ mach.loc
        AND reftable.code     EQ mach.m-code
        AND reftable.val[1]   EQ 1:
    MESSAGE "Machine: " + TRIM(mach.m-code) +
            " is obsolete, please replace to complete calculation..."
        VIEW-AS ALERT-BOX ERROR.
    RETURN.
  END.

  IF est.est-type EQ 8 THEN RUN cec/com/print4.p NO-ERROR.

  ELSE DO:
    FIND FIRST probe WHERE probe.company = est.company
                       AND probe.est-no = est.est-no
                     NO-LOCK NO-ERROR.


    IF AVAIL probe THEN RUN est/d-probeu.w (OUTPUT lv-override).

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

  for each est-op where est-op.company = xest.company and
                        est-op.est-no = xest.est-no and est-op.line > 500 
                        exclusive-lock :
           delete est-op.  
  end.

  RUN release-shared-buffers.

  SESSION:SET-WAIT-STATE("").

  find eb where recid(eb) = lv-eb-recid no-lock.
  find ef where recid(ef) = lv-ef-recid no-lock.
  FIND CURRENT est NO-LOCK NO-ERROR.
  run dispatch ('open-query').     

  IF v-ceSellPrice = "F" THEN DO:
    
      IF NOT lv-override THEN DO:
         FIND LAST bf-probe WHERE bf-probe.company = xest.company
                              AND bf-probe.est-no  = xest.est-no
                            NO-LOCK USE-INDEX LINE NO-ERROR.
         IF AVAIL bf-probe THEN
            REPOSITION {&browse-name} TO ROWID ROWID(bf-probe) NO-ERROR.
    
      END.
      ELSE DO:
        IF lv-probe-line GT 0 THEN DO:
         FIND LAST bf-probe WHERE bf-probe.company = xest.company
                              AND bf-probe.est-no  = xest.est-no
                              AND bf-probe.LINE    = lv-probe-line
                            NO-LOCK USE-INDEX LINE NO-ERROR.
         IF AVAIL bf-probe THEN
            REPOSITION {&browse-name} TO ROWID ROWID(bf-probe) NO-ERROR.
        END.
      END.

  
      lv-changed = "G".
      RUN calc-fields.    
      RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
  
      /* For type 'F', multicell, make correction to gross-profit 
         after it was updated by the db trigger on probe */
      FIND CURRENT probe.

      IF dec(probe.gross-profit:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})NE probe.gross-profit THEN DO:
        ASSIGN probe.gross-profit:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(probe.gross-profit).
        RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
      END.

  END.
  run dispatch ('open-query').     
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
     lv-fullc   = probe.full-cost:SCREEN-VALUE IN BROWSE {&browse-name}
     lv-nprof   = probe.net-profit:SCREEN-VALUE IN BROWSE {&browse-name}
     lv-gprof   = probe.gross-profit:SCREEN-VALUE IN BROWSE {&browse-name}
     lv-price   = probe.sell-price:SCREEN-VALUE IN BROWSE {&browse-name}
     lv-brd-%   = reftable.val[3]:SCREEN-VALUE IN BROWSE {&browse-name}
     lv-brdcm   = reftable.val[4]:SCREEN-VALUE IN BROWSE {&browse-name}
     lv-brdc$   = reftable.val[5]:SCREEN-VALUE IN BROWSE {&browse-name}
     lv-comm = probe.comm:SCREEN-VALUE IN BROWSE {&browse-name}.
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
  {src/adm/template/snd-list.i "reftable"}

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
  DEF VAR xxx AS DEC NO-UNDO.
  DEF VAR ld-tot-pric AS DEC NO-UNDO.
  DEF VAR ld-tot-fact AS DEC NO-UNDO.
  DEF VAR ld-tot-full AS DEC NO-UNDO.
  DEF VAR lv-qty LIKE probe.est-qty NO-UNDO.
  DEF VAR ll-price-change AS LOG NO-UNDO.
  def var qm as dec.
  def var v-comm like tt-tot NO-UNDO.
  def var v-prf-s as dec NO-UNDO.
  def var v-pct-s as dec NO-UNDO.
  DEF VAR v-probe-fmt AS CHAR NO-UNDO.
  DEF VAR ld-marg% AS DEC NO-UNDO.
  DEF VAR v-old-price AS DEC NO-UNDO.
  DEF VAR ld-commc AS DEC NO-UNDO.
  DEF VAR ld-fullc AS DEC NO-UNDO.
  DEF VAR v-old-full-cost AS DEC NO-UNDO.

  {est/checkuse.i}

  IF est.est-type NE 5 THEN DO:
    FOR EACH tt-probeit:
      DELETE tt-probeit.
    END.

    FOR EACH probeit
        WHERE probeit.company EQ probe.company
          AND probeit.est-no  EQ probe.est-no
          AND probeit.line    EQ probe.line
        USE-INDEX est-no NO-LOCK:
      CREATE tt-probeit.
      BUFFER-COPY probeit TO tt-probeit
      ASSIGN tt-probeit.row-id = ROWID(probeit).
    END.

    RUN cec/d-probit.w  (RECID(probe)).

    ll-price-change = NO.
    FOR EACH tt-probeit:
      FIND FIRST probeit WHERE ROWID(probeit) EQ tt-probeit.row-id NO-LOCK NO-ERROR.
      ll-price-change = NOT AVAIL probeit OR
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
               ld-marg% = ROUND((probe.sell-price - ld-fullc) / probe.sell-price * 100,2).

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

        RUN get-dir-proc(INPUT trim(est.est-no) + ".s"
                               + string(probe.line,v-probe-fmt),
                         OUTPUT tmp-dir).
        
        if opsys eq "unix" then 
          unix silent cp  value(tmp-dir + trim(est.est-no) + ".s"
                                        + string(probe.line,v-probe-fmt))
                          value(lv-cebrowse-dir + trim(est.est-no) + ".p"
                                        + string(probe.line,v-probe-fmt)).
        else
          dos silent copy value(tmp-dir + trim(est.est-no) + ".s"
                                        + string(probe.line,v-probe-fmt))
                          value(lv-cebrowse-dir + trim(est.est-no) + ".p"
                                        + string(probe.line,v-probe-fmt)).

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
  def var char-hdl as cha no-undo.


  {est/checkuse.i}

  RUN create-quote.

  run get-link-handle in adm-broker-hdl(this-procedure,"container-source", output char-hdl).
  run select-page in widget-handle(char-hdl) (10).

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
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.


  IF NOT ll-no-valid THEN
  DO WITH FRAME {&FRAME-NAME}:
    IF CAN-DO(lv-valid-profit,ip-focus:NAME) AND
       DEC(ip-focus:SCREEN-VALUE) GE 100     THEN DO:
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calc-cm B-table-Win 
FUNCTION calc-cm RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR v-cm AS DEC NO-UNDO.
v-cm = DEC(probe.sell-price:SCREEN-VALUE IN BROWSE {&browse-name})
       - DEC(probe.fact-cost:SCREEN-VALUE IN BROWSE {&browse-name}).
  RETURN v-cm.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calc-cmah B-table-Win 
FUNCTION calc-cmah RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR v-cmah AS DEC NO-UNDO.
DEF VAR v-mr AS DEC NO-UNDO.
DEF VAR v-run AS DEC NO-UNDO.
FIND FIRST tt-est-op NO-LOCK NO-ERROR.
IF NOT AVAIL tt-est-op THEN DO:
    FIND CURRENT est.
    find xef where recid(xef) = recid(ef).
    find xeb where recid(xeb) = recid(eb).
    /* calc-opq is to get the correct num-sh by calculating
       est-op records where line > 500 and copying them to 
       tt-est-op */
    RUN cec/calc-opq.p (INPUT cocode, INPUT locode,
                        INPUT ROWID(est),
                        INPUT ROWID(ef),
                        INPUT ROWID(eb) ).
END.

/* Get total assembly machine hours */
RUN est/calc-mr.p (INPUT probe.est-no, INPUT "A", OUTPUT v-mr, OUTPUT v-run).

IF v-mr GT 0 THEN
  v-cmah = DEC(reftable.val[8]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) / 
     ((v-mr + v-run) / (INTEGER(probe.est-qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) / 1000)).
ELSE
  v-cmah = 0.
/* MESSAGE "probe, calc-cmah, v-mr" v-mr "v-run" v-run           */
/*    "qty" probe.est-qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}  */
/*    "cm" reftable.val[8]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} */
/*     "v-cmah" v-cmah                                           */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                        */
RETURN v-cmah.   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calc-cmoh B-table-Win 
FUNCTION calc-cmoh RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR v-cmoh AS DEC NO-UNDO.
DEF VAR v-mr AS DEC NO-UNDO.
DEF VAR v-run AS DEC NO-UNDO.

RUN est/calc-mr.p (INPUT probe.est-no, INPUT "N", OUTPUT v-mr, OUTPUT v-run).
IF v-mr GT 0 THEN
  v-cmoh = DEC(reftable.val[8]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) / 
    ((v-mr + v-run) / (INTEGER(probe.est-qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) / 1000)).
ELSE
  v-cmoh = 0.

RETURN v-cmoh.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION checkNCBrd B-table-Win 
FUNCTION checkNCBrd RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Based on NK CECPromptNCBrd, this function will prompt the user
            that they have a manufactured form and layout NC = N
    Notes:  Returns user choice of yes/no to continue.
            BV - Task: 08281207
------------------------------------------------------------------------------*/
DEFINE VARIABLE lc-formlist AS CHARACTER INIT "" NO-UNDO.
DEFINE VARIABLE li-formcount AS INT INIT 0 NO-UNDO.
DEFINE VARIABLE ll-return AS LOGICAL     NO-UNDO.
DEFINE BUFFER lb-ef FOR ef.
DEFINE BUFFER lb-eb FOR eb.



FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.NAME EQ "CECPromptNCBrd"
      AND sys-ctrl.log-fld = YES
    NO-LOCK NO-ERROR.

IF AVAIL sys-ctrl THEN DO:
    IF AVAIL est THEN
        FOR EACH lb-ef WHERE lb-ef.company = est.company
            AND lb-ef.est-no = est.est-no
            NO-LOCK:
            IF NOT lb-ef.nc THEN DO:
                FIND FIRST lb-eb WHERE lb-eb.company = lb-ef.company
                    AND lb-eb.est-no = lb-ef.est-no
                    AND lb-eb.form-no = lb-ef.form-no
                    AND NOT lb-eb.pur-man  NO-LOCK NO-ERROR.
                IF AVAIL lb-eb THEN
                    ASSIGN 
                        li-formcount = li-formcount + 1
                        lc-formlist = lc-formlist + string(lb-ef.form-no) + " ".
            END.
        END.
    IF li-formcount > 0 THEN DO:
        IF li-formcount > 1 THEN
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
  ( input ip-time as int ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var ls-time as cha no-undo.
  ls-time = string(ip-time,"HH:MM:SS").
  RETURN ls-time.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-gp B-table-Win 
FUNCTION display-gp RETURNS DECIMAL
  ( INPUT ip-type AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR lv-gp AS DEC NO-UNDO.


  FIND FIRST ce-ctrl {sys/look/ce-ctrlw.i} NO-LOCK.

  DO WITH FRAME {&FRAME-NAME}:
    lv-gp = IF ce-ctrl.sell-by EQ "S" THEN
              IF ip-type EQ 1 THEN
                (probe.sell-price - probe.fact-cost) / probe.fact-cost * 100
              ELSE
                (DEC(probe.sell-price:SCREEN-VALUE IN BROWSE {&browse-name}) -
                 DEC(probe.fact-cost:SCREEN-VALUE IN BROWSE {&browse-name})) /
                DEC(probe.fact-cost:SCREEN-VALUE IN BROWSE {&browse-name}) * 100
                (DECIMAL(probe.sell-price:SCREEN-VALUE IN BROWSE {&browse-name}) -
                 DECIMAL(probe.fact-cost:SCREEN-VALUE IN BROWSE {&browse-name})) /
                DECIMAL(probe.fact-cost:SCREEN-VALUE IN BROWSE {&browse-name}) * 100
            ELSE
              IF ip-type EQ 1 THEN
                probe.gross-profit
              ELSE
                DEC(gross-profit:SCREEN-VALUE IN BROWSE {&browse-name}).
                DECIMAL(gross-profit:SCREEN-VALUE IN BROWSE {&browse-name}).
  END.

  RETURN lv-gp.   /* Function return value. */

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
  DEF BUFFER bf-eb FOR eb.
  DEFINE BUFFER bf-eb FOR eb.
  
  DEF VAR lSatisfied AS LOG NO-UNDO.
  DEF VAR cMachine AS cha NO-UNDO.
  DEFINE VARIABLE lSatisfied AS LOGICAL NO-UNDO.
  DEFINE VARIABLE cMachine AS CHARACTER NO-UNDO.

  lSatisfied = YES.
  cMachine = "".

  DieLoop:
  FOR EACH bf-eb NO-LOCK WHERE bf-eb.company = est.company
                   AND bf-eb.est-no = est.est-no
                   AND bf-eb.blank-no > 0
                   AND bf-eb.pur-man = NO :
  FOR EACH bf-eb NO-LOCK WHERE bf-eb.company EQ est.company
                   AND bf-eb.est-no EQ est.est-no
                   AND bf-eb.blank-no GT 0
                   AND bf-eb.pur-man EQ NO :
  
    FIND FIRST style WHERE style.company = bf-eb.company
                 AND style.style = bf-eb.style
                 AND style.flute = bf-eb.flute
                 AND style.test = bf-eb.test 
                 AND (style.TYPE = "p" OR style.TYPE = "R") 
        NO-LOCK NO-ERROR.
    IF NOT AVAIL style THEN
       FIND FIRST style WHERE style.company = bf-eb.company
                 AND style.style = bf-eb.style
                 AND style.flute = ""
                 AND style.test = ""
                 AND (style.TYPE = "p" OR style.TYPE = "R") 
        NO-LOCK NO-ERROR.
    IF NOT AVAIL style THEN next.
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
            est-op.att-type[1] = "" AND
            est-op.att-type[2] = "" AND
            est-op.att-type[3] = "" 
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
  ( INPUT ip-type AS INT )  :
  ( INPUT ip-type AS INTEGER )  :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR lv-overall AS DEC NO-UNDO.
  DEFINE VARIABLE lv-overall AS DECIMAL NO-UNDO.


  IF AVAIL probe THEN
  IF AVAILABLE probe THEN
    lv-overall = ROUND((IF ip-type EQ 1 THEN probe.sell-price
                                        ELSE DEC(probe.sell-price:SCREEN-VALUE IN BROWSE {&browse-name}))
                                        ELSE DECIMAL(probe.sell-price:SCREEN-VALUE IN BROWSE {&browse-name}))
                 / probe.bsf,2).

  ELSE lv-overall = 0.

  IF lv-overall = ? then lv-overall = 0.
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
  def var lv-tot-msf as dec no-undo.
  DEFINE VARIABLE lv-tot-msf AS DECIMAL NO-UNDO.

  
  if avail probe then lv-tot-msf = probe.tot-lbs / 1000.
  else lv-tot-msf = 0.
  IF AVAILABLE probe THEN lv-tot-msf = probe.tot-lbs / 1000.
  ELSE lv-tot-msf = 0.

  RETURN lv-tot-msf.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

