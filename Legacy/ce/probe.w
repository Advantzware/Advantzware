&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: ce\probe.w

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

def var voverall as dec form ">>>,>>9.99" no-undo.
def var vtot-msf as dec form ">>>>9.99" no-undo.
def var vtot-lbs as dec form ">>>>>>>9" no-undo.
DEFINE VARIABLE dMatPctSellPrice LIKE probe.net-profit. 

def new shared buffer xest for est.
def new shared buffer xef for ef.
def new shared buffer xeb for eb.

{ce/print4.i "new shared" "new shared"}
{ce/print42.i "new shared"}

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
def new shared var v-do-gsa like do-gsa no-undo.
def new shared buffer xop for est-op.


def new shared var v-qtty like qtty no-undo.
def new shared var v-drop-rc as log no-undo.

def var v as int no-undo.
def var vn-out like ef.n-out-l init 1 no-undo.
def var v-outw like ef.n-out no-undo.
def var v-outl like ef.n-out-l no-undo.
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
def var ls-notefile as cha no-undo.
DEF VAR ls-xfile AS CHAR NO-UNDO.
def var ls-probetime as cha no-undo.  /* time display */

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
    
&SCOPED-DEFINE where-probeit WHERE probeit.company EQ probe.company ~
                               AND probeit.est-no  EQ probe.est-no  ~
                               AND probeit.line    EQ probe.line

DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-qty FOR reftable.

DEF TEMP-TABLE tt-ei NO-UNDO
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20.

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
DEF VAR lv-valid-profit AS CHAR NO-UNDO
    INIT "market-price,gross-profit,net-profit".


DEF NEW SHARED VAR lv-cebrowse-dir AS CHAR NO-UNDO.
DEF VAR v-dir AS CHAR FORMAT "X(80)" NO-UNDO.
DEF VAR v-tmp-int AS INT NO-UNDO.
DEF VAR v-can-update AS LOG NO-UNDO.
DEF NEW SHARED VAR gEstSummaryOnly AS LOG NO-UNDO.
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
     users.user_id EQ USERID("NOSWEAT")
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
  {sys/inc/ceround.i}

  {sys/inc/cerun.i F}
  {sys/inc/cerun.i C}
  vmclean = CAN-DO("McLean,HOP,CERunF 2",cerunf).

  {sys/inc/cewhatif.i}
END.

{sys/inc/ceprint.i}
{sys/inc/ceprep.i}
{sys/inc/ceprepprice.i}

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
&Scoped-define INTERNAL-TABLES probe

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table probe.est-qty probe.fact-cost ~
probe.full-cost probe.market-price display-gp (1) @ probe.gross-profit ~
probe.gross-profit display-gp (1) @ probe.gross-profit probe.comm ~
probe.net-profit probe.sell-price probe.gsh-qty probe.do-quote ~
voverall(1) @ voverall probe.probe-date probe.probe-user ~
vtot-lbs() @ vtot-lbs vtot-msf() @ vtot-msf ~
cvt-time(probe.probe-time) @ ls-probetime probe.spare-dec-1 ~
fDirectMatPctSellPrice() @ dMatPctSellPrice 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table probe.full-cost ~
probe.market-price probe.gross-profit probe.net-profit probe.sell-price ~
probe.do-quote 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table probe
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table probe
&Scoped-define QUERY-STRING-br_table FOR EACH probe WHERE probe.company = eb.company and ~
ASI.probe.est-no = eb.est-no ~
      AND probe.probe-date ne ? NO-LOCK ~
    BY probe.company ~
       BY probe.est-no ~
        BY probe.probe-date ~
         BY probe.est-qty
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH probe WHERE probe.company = eb.company and ~
ASI.probe.est-no = eb.est-no ~
      AND probe.probe-date ne ? NO-LOCK ~
    BY probe.company ~
       BY probe.est-no ~
        BY probe.probe-date ~
         BY probe.est-qty.
&Scoped-define TABLES-IN-QUERY-br_table probe
&Scoped-define FIRST-TABLE-IN-QUERY-br_table probe


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fDirectMatPctSellPrice B-table-Win 
FUNCTION fDirectMatPctSellPrice RETURNS DECIMAL
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD voverall B-table-Win 
FUNCTION voverall RETURNS DECIMAL
  ( INPUT ip-type AS INT )   FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD vtot-lbs B-table-Win 
FUNCTION vtot-lbs RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

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
      probe SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      probe.est-qty FORMAT ">>>>>>>9":U COLUMN-FONT 0
      probe.fact-cost COLUMN-LABEL "Tot.Fact!Cost" FORMAT ">>>,>>9.99":U
            WIDTH 15 COLUMN-FONT 0
      probe.full-cost FORMAT ">>>,>>9.99":U WIDTH 15 COLUMN-FONT 0
      probe.market-price COLUMN-LABEL "Margin%" FORMAT "->,>>9.99":U
      display-gp (1) @ probe.gross-profit
      probe.gross-profit COLUMN-LABEL "Gross%" FORMAT "->,>>9.99":U
            COLUMN-FONT 0
      display-gp (1) @ probe.gross-profit
      probe.comm FORMAT "->>,>>9.99<<<":U
      probe.net-profit COLUMN-LABEL "Net%" FORMAT "->,>>9.99":U
            COLUMN-FONT 0
      probe.sell-price FORMAT ">>>,>>9.99":U WIDTH 15 COLUMN-FONT 0
      probe.gsh-qty COLUMN-LABEL "Total!Sheets" FORMAT ">>>>>>9":U
            COLUMN-FONT 0
      probe.do-quote COLUMN-LABEL "Q" FORMAT "Y/N":U COLUMN-FONT 0
      voverall(1) @ voverall COLUMN-LABEL "Price!/BSF" WIDTH 15
            COLUMN-FONT 0
      probe.probe-date FORMAT "99/99/9999":U
      probe.probe-user COLUMN-LABEL "Probe By" FORMAT "X(8)":U
      vtot-lbs() @ vtot-lbs COLUMN-LABEL "Shipping!Weight" WIDTH 12.2
      vtot-msf() @ vtot-msf COLUMN-LABEL "Total!MSF" COLUMN-FONT 0
      cvt-time(probe.probe-time) @ ls-probetime COLUMN-LABEL "Time" FORMAT "x(8)":U
      probe.spare-dec-1 COLUMN-LABEL "Direct!Material" FORMAT "->>>,>>9.99":U
            WIDTH 15
      fDirectMatPctSellPrice() @ dMatPctSellPrice COLUMN-LABEL "Dir. Mat%"
  ENABLE
      probe.full-cost
      probe.market-price HELP "Enter Margin% to get Commission%"
      probe.gross-profit
      probe.net-profit HELP "Enter Net Profit"
      probe.sell-price
      probe.do-quote
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 143 BY 13.1
         FONT 0
         TITLE "Estimate  Analysis Per Thousand".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 143 RIGHT-ALIGNED
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
         HEIGHT             = 13.19
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

/* SETTINGS FOR BROWSE br_table IN FRAME F-Main
   ALIGN-R                                                              */
ASSIGN 
       probe.comm:VISIBLE IN BROWSE br_table = FALSE.

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
"probe.fact-cost" "Tot.Fact!Cost" ">>>,>>9.99" "decimal" ? ? 0 ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.probe.full-cost
"probe.full-cost" ? ">>>,>>9.99" "decimal" ? ? 0 ? ? ? yes ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.probe.market-price
"probe.market-price" "Margin%" "->,>>9.99" "decimal" ? ? ? ? ? ? yes "Enter Margin% to get Commission%" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"display-gp (1) @ probe.gross-profit" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.probe.gross-profit
"probe.gross-profit" "Gross%" "->,>>9.99" "decimal" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"display-gp (1) @ probe.gross-profit" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.probe.comm
"probe.comm" ? ? "decimal" ? ? ? ? ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.probe.net-profit
"probe.net-profit" "Net%" "->,>>9.99" "decimal" ? ? 0 ? ? ? yes "Enter Net Profit" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.probe.sell-price
"probe.sell-price" ? ">>>,>>9.99" "decimal" ? ? 0 ? ? ? yes ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.probe.gsh-qty
"probe.gsh-qty" "Total!Sheets" ">>>>>>9" "integer" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.probe.do-quote
"probe.do-quote" "Q" ? "logical" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"voverall(1) @ voverall" "Price!/BSF" ? ? ? ? 0 ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   = ASI.probe.probe-date
     _FldNameList[15]   > ASI.probe.probe-user
"probe.probe-user" "Probe By" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"vtot-lbs() @ vtot-lbs" "Shipping!Weight" ? ? ? ? ? ? ? ? no ? no no "12.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"vtot-msf() @ vtot-msf" "Total!MSF" ? ? ? ? 0 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"cvt-time(probe.probe-time) @ ls-probetime" "Time" "x(8)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > ASI.probe.spare-dec-1
"probe.spare-dec-1" "Direct!Material" "->>>,>>9.99" "decimal" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"fDirectMatPctSellPrice() @ dMatPctSellPrice" "Dir. Mat%" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
  IF ll-use-margin THEN DO:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN    
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

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

  {cec/combasis.i}

  FIND FIRST ce-ctrl {sys/look/ce-ctrlW.i} NO-LOCK.

  IF lv-changed NE "" THEN
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     lv-changed2 = lv-changed
     ld-price    = DEC(lv-price).

    FIND FIRST probe-ref NO-LOCK
        WHERE probe-ref.reftable EQ "probe-ref"
          AND probe-ref.company  EQ probe.company
          AND probe-ref.loc      EQ ""
          AND probe-ref.code     EQ probe.est-no
          AND probe-ref.code2    EQ STRING(probe.line,"9999999999")
        NO-ERROR.
    IF AVAIL probe-ref THEN
      v-com = (probe-ref.val[2] + probe-ref.val[3] +
               probe-ref.val[4] + probe-ref.val[5]) / 100000.

    /*IF probe.comm NE 0 THEN*/ v-com = probe.comm.

    ASSIGN
     ld-marg%    = DEC(probe.market-price:SCREEN-VALUE IN BROWSE {&browse-name})
     ld-factc    = DEC(probe.fact-cost:SCREEN-VALUE IN BROWSE {&browse-name})
     ld-commc    = (ld-price - (IF v-basis EQ "G" THEN ld-factc ELSE 0)) *
                   (v-com / 100)   
     ld-fullc    = DEC(probe.full-cost:SCREEN-VALUE IN BROWSE {&browse-name}) -
                   ld-commc
     /*ld-brd-m    = DEC(reftable.val[2]:SCREEN-VALUE IN BROWSE {&browse-name})
     ld-brd-%    = DEC(reftable.val[3]:SCREEN-VALUE IN BROWSE {&browse-name})
     ld-brdcm    = DEC(reftable.val[4]:SCREEN-VALUE IN BROWSE {&browse-name})
     ld-brdc$    = DEC(reftable.val[5]:SCREEN-VALUE IN BROWSE {&browse-name})*/.

    IF lv-changed EQ "S" THEN DO:
        ASSIGN
       ld-price = DEC(probe.sell-price:SCREEN-VALUE IN BROWSE {&browse-name})
       ld-commc = (ld-price - (IF v-basis EQ "G" THEN ld-factc ELSE 0)) *
                  (v-com / 100).
        IF ld-price = 0 THEN RETURN.
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
          v-pct = DEC(probe.gross-profit:SCREEN-VALUE IN BROWSE {&browse-name}).

          IF ce-ctrl.sell-by EQ "S" THEN lv-changed2 = "S".
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
                               ld-fullc - ld-factc,
                               v-com,
                               v-pct,
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

    IF ll-use-margin THEN DO:  /* Get Commission% */
      RUN est/getsmanmtrx.p (ROWID(est), "C",
                             INPUT-OUTPUT v-com,
                             INPUT-OUTPUT ld-marg%).

      ld-commc = ld-price * v-com / 100.
    END.

    ASSIGN
     ld-brd-% = ld-brd-m / ld-price * 100
     ld-brdcm = ld-price - ld-brd-m
     ld-brdc$ = ld-brdcm * (probe.est-qty / 1000)
     ld-fullc = ld-fullc + ld-commc.

    IF probe.comm:VISIBLE IN BROWSE {&browse-name} THEN
       probe.comm:SCREEN-VALUE IN BROWSE {&browse-name} =
                 STRING(v-com,probe.comm:FORMAT IN BROWSE {&browse-name}).

    probe.full-cost:SCREEN-VALUE IN BROWSE {&browse-name} =
       STRING(ld-fullc,probe.full-cost:FORMAT IN BROWSE {&browse-name}) NO-ERROR.

    /*IF lv-changed NE "BC$" AND NOT ERROR-STATUS:ERROR THEN
      reftable.val[5]:SCREEN-VALUE IN BROWSE {&browse-name} =
          STRING(ld-brdc$,reftable.val[5]:FORMAT IN BROWSE {&browse-name}) NO-ERROR.

    IF lv-changed NE "BCM" AND NOT ERROR-STATUS:ERROR THEN
      reftable.val[4]:SCREEN-VALUE IN BROWSE {&browse-name} =
          STRING(ld-brdcm,reftable.val[4]:FORMAT IN BROWSE {&browse-name}) NO-ERROR.

    IF lv-changed NE "B" AND NOT ERROR-STATUS:ERROR THEN
      reftable.val[3]:SCREEN-VALUE IN BROWSE {&browse-name} =
          STRING(ld-brd-%,reftable.val[3]:FORMAT IN BROWSE {&browse-name}) NO-ERROR.*/

    IF lv-changed NE "M" AND NOT ERROR-STATUS:ERROR THEN
      probe.market-price:SCREEN-VALUE IN BROWSE {&browse-name} =
          STRING(ld-marg%,probe.market-price:FORMAT IN BROWSE {&browse-name}) NO-ERROR.

    IF lv-changed NE "S" AND NOT ERROR-STATUS:ERROR THEN
      probe.sell-price:SCREEN-VALUE IN BROWSE {&browse-name} =
          STRING(ld-price,probe.sell-price:FORMAT IN BROWSE {&browse-name}) NO-ERROR.
        
    IF lv-changed NE "N" AND NOT ERROR-STATUS:ERROR THEN
      probe.net-profit:SCREEN-VALUE IN BROWSE {&browse-name} =
          STRING((1 - (ld-fullc / ld-price)) * 100) NO-ERROR.

    IF lv-changed NE "G" AND NOT ERROR-STATUS:ERROR THEN
      probe.gross-profit:SCREEN-VALUE IN BROWSE {&browse-name} =
          STRING((1 - (ld-factc / ld-price)) * 100) NO-ERROR.

    lv-changed = lv-changed2.

    IF ERROR-STATUS:ERROR                                                    OR
       TRIM(probe.full-cost:SCREEN-VALUE IN BROWSE {&browse-name})    EQ "?" OR
       TRIM(probe.gross-profit:SCREEN-VALUE IN BROWSE {&browse-name}) EQ "?" OR
       TRIM(probe.net-profit:SCREEN-VALUE IN BROWSE {&browse-name})   EQ "?" OR
       TRIM(probe.sell-price:SCREEN-VALUE IN BROWSE {&browse-name})   EQ "?" OR
       /*TRIM(reftable.val[3]:SCREEN-VALUE IN BROWSE {&browse-name})    EQ "?" OR
       TRIM(reftable.val[4]:SCREEN-VALUE IN BROWSE {&browse-name})    EQ "?" OR
       TRIM(reftable.val[5]:SCREEN-VALUE IN BROWSE {&browse-name})    EQ "?" OR*/
       ld-price GT 999999.99                                                 THEN DO:

      MESSAGE "Value(s) invalid, please try again" VIEW-AS ALERT-BOX ERROR.

      IF probe.comm:VISIBLE IN BROWSE {&browse-name} THEN
         probe.comm:SCREEN-VALUE IN BROWSE {&browse-name} = lv-comm.

      ASSIGN
       probe.full-cost:SCREEN-VALUE IN BROWSE {&browse-name}    = lv-fullc
       probe.net-profit:SCREEN-VALUE IN BROWSE {&browse-name}   = lv-nprof
       probe.gross-profit:SCREEN-VALUE IN BROWSE {&browse-name} = lv-gprof
       probe.sell-price:SCREEN-VALUE IN BROWSE {&browse-name}   = lv-price
       /*reftable.val[3]:SCREEN-VALUE IN BROWSE {&browse-name}    = lv-brd-%
       reftable.val[4]:SCREEN-VALUE IN BROWSE {&browse-name}    = lv-brdcm
       reftable.val[5]:SCREEN-VALUE IN BROWSE {&browse-name}    = lv-brdc$*/.

      /*IF lv-changed EQ "BC$" THEN
        APPLY "entry" TO reftable.val[5] IN BROWSE {&browse-name}.
      ELSE
      IF lv-changed EQ "BCM" THEN
        APPLY "entry" TO reftable.val[4] IN BROWSE {&browse-name}.
      ELSE
      IF lv-changed EQ "B" THEN
        APPLY "entry" TO reftable.val[3] IN BROWSE {&browse-name}.
      ELSE*/
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
      dMatPctSellPrice:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fDirectMatPctSellPrice()).
      IF lv-changed2 NE "S" THEN 
        probe.gross-profit:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(display-gp (0)).
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
  DEF OUTPUT PARAM op-combo AS LOG NO-UNDO.
  

  op-combo = AVAIL est AND est.est-type EQ 4.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-item B-table-Win 
PROCEDURE copy-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-est-summ FOR est-summ.
  DEF BUFFER bf-reftable FOR reftable.

  DEF VAR v-gprofit-val AS cha NO-UNDO.
  DEF VAR v-gprofit AS DEC EXTENT 5 NO-UNDO.
  DEF BUFFER bf-probe FOR probe.
  DEF BUFFER bf-probeit FOR probeit.
  DEF BUFFER b-probe FOR probe.
  DEF VAR v-probe-rowid AS ROWID NO-UNDO.
  DEF VAR v-rowid AS ROWID NO-UNDO.
  DEF VAR v-probe-line AS INT NO-UNDO.
  def var v as INT NO-UNDO.
  def var li as int NO-UNDO.
  
  def var v-net as dec NO-UNDO.
  def var v-sell as dec extent 2 NO-UNDO.
  def var v-skip-pct as log NO-UNDO.
  DEF VAR v-start LIKE probe.line NO-UNDO.
  DEF VAR ld-price LIKE probe.sell-price NO-UNDO.
  DEF VAR v-est-eqty AS INT NO-UNDO.
  DEF VAR v-bf-probe-format AS CHAR NO-UNDO.
  DEF VAR v-probe-format AS CHAR NO-UNDO.

  v-probe-rowid = ROWID(probe).

  RUN est/d-gmargn.w (probe.net-profit, OUTPUT v-gprofit-val).
  IF v-gprofit-val EQ "" THEN RETURN. /* cancel*/

  SESSION:SET-WAIT-STATE("general").
  FOR EACH bf-probe NO-LOCK WHERE bf-probe.company = probe.company
                              AND bf-probe.est-no = probe.est-no
                              AND bf-probe.probe-date <> ?
                              BY bf-probe.LINE DESC:
      v-probe-line = bf-probe.LINE.
      LEAVE.
  END.
  FIND bf-probe WHERE ROWID(bf-probe) = v-probe-rowid NO-LOCK.
  find first sys-ctrl
         where sys-ctrl.company eq cocode
           and sys-ctrl.name    eq "CERUNF"
         no-lock no-error.
  IF AVAIL sys-ctrl THEN vmclean = IF LOOKUP(sys-ctrl.char-fld,"McLean,CERunF 2") NE 0 THEN TRUE ELSE FALSE .

  EMPTY TEMP-TABLE tt-bqty.

  DO i = 1 TO EXTENT(v-gprofit):
   CREATE tt-bqty.
   tt-bqty = DEC(ENTRY(i,v-gprofit-val)).
  END.

  i = 0.
  FOR EACH tt-bqty BY tt-bqty:
    ASSIGN
     i            = i + 1
     v-gprofit[i] = tt-bqty.
  END.

  EMPTY TEMP-TABLE tt-bqty.

  DO i = 1 TO EXTENT(v-gprofit):
     IF v-gprofit[i] EQ 0 THEN NEXT.

     CREATE probe.
     BUFFER-COPY bf-probe EXCEPT bf-probe.LINE bf-probe.probe-time bf-probe.probe-date bf-probe.net-profit TO probe.
     ASSIGN probe.probe-date = TODAY
            probe.probe-time = TIME
            probe.LINE = v-probe-line + 1
            v-probe-line = v-probe-line + 1.

     FOR EACH probeit {&where-probeit} NO-LOCK:
       CREATE bf-probeit.
       BUFFER-COPY probeit EXCEPT probeit.LINE TO bf-probeit.
       ASSIGN bf-probeit.LINE = probe.LINE.

       FOR EACH reftable NO-LOCK
           WHERE reftable.reftable EQ "ce/com/probemk.p"
             AND reftable.company  EQ probeit.company
             AND reftable.loc      EQ probeit.est-no
             AND reftable.code     EQ STRING(probeit.line,"9999999999")
             AND reftable.code2    EQ probeit.part-no:
         CREATE bf-reftable.
         BUFFER-COPY reftable EXCEPT rec_key TO bf-reftable
         ASSIGN
          reftable.code = STRING(probe.line,"9999999999").
       END.
     END.     

     v-est-eqty = 0.
     FOR EACH est-summ NO-LOCK
          WHERE est-summ.company EQ bf-probe.company
            AND est-summ.est-no  EQ bf-probe.est-no           
            BY est-summ.eqty DESC:
       v-est-eqty = est-summ.eqty.
       LEAVE.
     END.

     FOR EACH est-summ NO-LOCK
         WHERE est-summ.company EQ bf-probe.company
           AND est-summ.est-no  EQ bf-probe.est-no
           AND est-summ.e-num   EQ bf-probe.line
         USE-INDEX est-qty:
         CREATE bf-est-summ.
         BUFFER-COPY est-summ EXCEPT est-summ.eqty est-summ.e-num TO bf-est-summ.
         v-est-eqty = v-est-eqty + 1.
         ASSIGN bf-est-summ.eqty = v-est-eqty
                bf-est-summ.e-num = probe.LINE.
     END.

     FOR EACH reftable
         WHERE reftable.reftable EQ "probe.per-msf"
           AND reftable.company  EQ bf-probe.company
           AND reftable.loc      EQ ""
           AND reftable.code     EQ bf-probe.est-no
           AND reftable.code2    EQ STRING(bf-probe.line,"9999999999"):
       CREATE bf-reftable.
       BUFFER-COPY reftable EXCEPT rec_key TO bf-reftable
       ASSIGN
        reftable.code2 = STRING(probe.line,"9999999999").
     END.

     FOR EACH reftable
         WHERE reftable.reftable EQ "probe.per-ref"
           AND reftable.company  EQ bf-probe.company
           AND reftable.loc      EQ ""
           AND reftable.code     EQ bf-probe.est-no
           AND reftable.code2    EQ STRING(bf-probe.line,"9999999999"):
       CREATE bf-reftable.
       BUFFER-COPY reftable EXCEPT rec_key TO bf-reftable
       ASSIGN
        reftable.code2 = STRING(probe.line,"9999999999").
     END.

     FOR EACH reftable
         WHERE reftable.reftable EQ "probe.per-board"
           AND reftable.company  EQ bf-probe.company
           AND reftable.loc      EQ ""
           AND reftable.code     EQ bf-probe.est-no
           AND reftable.code2    EQ STRING(bf-probe.line,"9999999999"):
       CREATE bf-reftable.
       BUFFER-COPY reftable EXCEPT rec_key TO bf-reftable
       ASSIGN
        reftable.code2 = STRING(probe.line,"9999999999").
     END.

     FOR EACH reftable
        WHERE reftable.reftable EQ "probe-ref"
          AND reftable.company  EQ bf-probe.company
          AND reftable.loc      EQ ""
          AND reftable.code     EQ bf-probe.est-no
          AND reftable.code2    EQ STRING(bf-probe.line,"9999999999"):
       CREATE bf-reftable.
       BUFFER-COPY reftable EXCEPT rec_key TO bf-reftable
       ASSIGN
        reftable.code2 = STRING(probe.line,"9999999999").
     END.

     FOR EACH reftable
        WHERE reftable.reftable EQ "ce/com/probemk.p"
          AND reftable.company  EQ bf-probe.company
          AND reftable.loc      EQ bf-probe.est-no
          AND reftable.code     EQ STRING(bf-probe.line,"9999999999"):
       CREATE bf-reftable.
       BUFFER-COPY reftable EXCEPT rec_key TO bf-reftable
       ASSIGN
        reftable.code = STRING(probe.line,"9999999999").
     END.
     
     IF bf-probe.LINE LT 100 THEN
        v-bf-probe-format = "99".
     ELSE
        v-bf-probe-format = "999".

     IF probe.LINE LT 100 THEN
        v-probe-format = "99".
     ELSE
        v-probe-format = "999".

     RUN get-dir-proc(INPUT trim(est.est-no) + ".a" + string(bf-probe.line,v-bf-probe-format),
                      OUTPUT tmp-dir).
     
     OS-COPY value(tmp-dir + trim(est.est-no) + ".a" + string(bf-probe.line,v-bf-probe-format))
             value(lv-cebrowse-dir + trim(est.est-no) + ".a" + string(probe.line,v-probe-format)).
     
     RUN get-dir-proc(INPUT trim(est.est-no) + ".p" + string(bf-probe.line,v-bf-probe-format),
                      OUTPUT tmp-dir).
     
     OS-COPY value(tmp-dir + trim(est.est-no) + ".p" + string(bf-probe.line,v-bf-probe-format))
             value(lv-cebrowse-dir + trim(est.est-no) + ".p" + string(probe.line,v-probe-format)).
     
     RUN get-dir-proc(INPUT trim(est.est-no) + ".s" + string(bf-probe.line,v-bf-probe-format),
                      OUTPUT tmp-dir).
     
     OS-COPY value(tmp-dir + trim(est.est-no) + ".s" + string(bf-probe.line,v-bf-probe-format))
             value(lv-cebrowse-dir + trim(est.est-no) + ".s" + string(probe.line,v-probe-format)).
     
     RUN get-dir-proc(INPUT trim(est.est-no) + ".v" + string(bf-probe.line,v-bf-probe-format),
                      OUTPUT tmp-dir).
     
     OS-COPY value(tmp-dir + trim(est.est-no) + ".v" + string(bf-probe.line,v-bf-probe-format))
             value(lv-cebrowse-dir + trim(est.est-no) + ".v" + string(probe.line,v-probe-format)).

     v-rowid = ROWID(probe).
     RUN dispatch ("open-query").
     REPOSITION {&browse-name} TO ROWID v-rowid NO-ERROR.
     RUN dispatch ("row-changed").

     probe.net-profit:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(v-gprofit[i]).
     lv-changed = "N".
     RUN calc-fields.
     RUN dispatch ("update-record").
  END.  /* do i = 1 to 5*/

  REPOSITION {&browse-name} TO ROWID v-probe-rowid NO-ERROR.
  SESSION:SET-WAIT-STATE("").

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
DEF buffer xprobe for probe.
DEF BUFFER bf-qhd FOR quotehd.
DEF BUFFER bf-notes FOR notes.

  /* generating quote record - main code from  quote.a with new table */

DEF var j as int no-undo.
DEF var i as int no-undo.
DEF buffer bf-eb for eb.
DEF buffer bf-ef for ef.

DEF var cocode as cha no-undo.
DEF var locode as cha no-undo.
DEF var li-q-no LIKE quotehd.q-no NO-UNDO.
DEF var li-line as int no-undo .  /* for quoteitm.line */
DEF var ll-new-quote as log no-undo.
DEF var ll-first as log no-undo.
DEF var li-first-qty as int no-undo.  /* first qty for quoteitm */
DEF VAR li-prep-qty LIKE quotechg.prep-qty NO-UNDO.
DEF var li-cnt as int no-undo.
DEF var ld-cost as dec no-undo.
DEF var li-value as int no-undo.
DEF VAR v-tot-mat AS DEC NO-UNDO.
DEF VAR v-tot-lab AS DEC NO-UNDO.
DEFINE VARIABLE cNotes LIKE quotehd.comment  NO-UNDO.
 
{est/checkuse.i}

SESSION:SET-WAIT-STATE("general").

DISABLE TRIGGERS FOR LOAD OF quoteqty.

IF CAN-FIND(FIRST xprobe
            WHERE xprobe.company EQ probe.company
              AND xprobe.est-no  EQ probe.est-no
              AND xprobe.do-quote) THEN DO:

  IF est.est-type EQ 4 THEN DO:
    FIND FIRST xjob NO-ERROR.
    IF NOT AVAIL xjob THEN DO:
      /*MESSAGE "You must calculate a combo estimate before creating a quote..."
              VIEW-AS ALERT-BOX ERROR.
      */
        MESSAGE "Must press Calculate Button to generate New Quote"
                VIEW-AS ALERT-BOX ERROR.
      RETURN.
    END.
  END.

  assign
   cocode     = est.company
   locode     = est.loc.

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
  
  EMPTY TEMP-TABLE w-probeit.

  ASSIGN
     li-q-no = if avail quotehd then quotehd.q-no else 1
     i = 0.

  IF est.est-type EQ 4 THEN
  FOR EACH xprobe
      WHERE xprobe.company EQ probe.company
        AND xprobe.est-no  EQ probe.est-no
        AND xprobe.do-quote,
      EACH probeit
      WHERE probeit.company  EQ xprobe.company
        AND probeit.est-no   EQ xprobe.est-no
        AND probeit.line     EQ xprobe.line
      NO-LOCK 
      BREAK BY xprobe.line
            BY probeit.part-no:

    IF LAST-OF(probeit.part-no) THEN xprobe.do-quote = NO.

    CREATE w-probeit.
    BUFFER-COPY probeit TO w-probeit.

    FOR EACH xjob WHERE xjob.i-no EQ probeit.part-no,
        FIRST bf-eb
        WHERE bf-eb.company  EQ est.company
          AND bf-eb.est-no   EQ est.est-no
          AND bf-eb.form-no  EQ xjob.form-no
          AND bf-eb.blank-no EQ xjob.blank-no
        NO-LOCK:
  
      ASSIGN
       w-probeit.mat-cost = w-probeit.mat-cost +
                            (xjob.mat * (bf-eb.bl-qty / 1000))
       w-probeit.lab-cost = w-probeit.lab-cost +
                            (xjob.lab * (bf-eb.bl-qty / 1000))
       w-probeit.fo-cost  = w-probeit.fo-cost  +
                            (xjob.foh * (bf-eb.bl-qty / 1000))
       w-probeit.vo-cost  = w-probeit.vo-cost  +
                            (xjob.voh * (bf-eb.bl-qty / 1000)).
    END.

    ASSIGN
     w-probeit.prof-on    = xprobe.prof-on
     w-probeit.mat-cost   = w-probeit.mat-cost / (w-probeit.bl-qty / 1000)
     w-probeit.lab-cost   = w-probeit.lab-cost / (w-probeit.bl-qty / 1000)
     w-probeit.vo-cost    = w-probeit.vo-cost  / (w-probeit.bl-qty / 1000)
     w-probeit.fo-cost    = w-probeit.fo-cost  / (w-probeit.bl-qty / 1000)
     w-probeit.tot-lbs    = xprobe.tot-lbs 
     w-probeit.probe-date = xprobe.probe-date.

    FIND FIRST reftable NO-LOCK
        WHERE reftable.reftable EQ "ce/com/probemk.p"
          AND reftable.company  EQ probeit.company
          AND reftable.loc      EQ probeit.est-no
          AND reftable.code     EQ STRING(probeit.line,"9999999999")
          AND reftable.code2    EQ probeit.part-no
        NO-ERROR.
     w-probeit.freight = IF AVAIL reftable THEN reftable.val[1]
                                           ELSE xprobe.freight.
  END.

  ELSE
  FOR EACH xprobe
      WHERE xprobe.company  EQ est.company
        AND xprobe.est-no   EQ est.est-no
        AND xprobe.do-quote:

    ASSIGN
     xprobe.do-quote = NO
     i               = i + 1.

    FIND FIRST bf-eb
        WHERE bf-eb.company   EQ est.company 
          AND bf-eb.est-no    EQ est.est-no
          AND (bf-eb.form-no  NE 0 OR
               (bf-eb.form-no EQ 0 AND est.est-type EQ 2))
          AND bf-eb.blank-no  EQ IF est.est-type EQ 1 THEN 1
                                 ELSE
                                 IF est.est-type EQ 2 THEN 0
                                 ELSE i
        NO-LOCK NO-ERROR.

    IF AVAIL bf-eb THEN DO:
      CREATE w-probeit.
      ASSIGN
       w-probeit.company      = bf-eb.company
       w-probeit.est-no       = bf-eb.est-no
       w-probeit.cust-no      = bf-eb.cust-no
       w-probeit.part-no      = bf-eb.part-no
       w-probeit.bl-qty       = xprobe.est-qty
       w-probeit.sell-price   = xprobe.sell-price
       w-probeit.prof-on      = xprobe.prof-on
       w-probeit.net-profit   = xprobe.net-profit
       w-probeit.gross-profit = xprobe.gross-profit
       w-probeit.mat-cost     = xprobe.mat-cost
       w-probeit.lab-cost     = xprobe.lab-cost
       w-probeit.vo-cost      = xprobe.vo-cost
       w-probeit.fo-cost      = xprobe.fo-cost
       w-probeit.tot-lbs      = xprobe.tot-lbs 
       w-probeit.freight      = xprobe.freight
       w-probeit.probe-date   = xprobe.probe-date.
    END.
  END.

  RUN delete-old-part (li-q-no).

  li-line = 1.

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
        /*FIND LAST quotehd
            WHERE quotehd.company EQ cocode
              AND quotehd.loc     EQ locode
            USE-INDEX q-no NO-LOCK NO-ERROR.
        li-q-no = (IF AVAIL quotehd THEN quotehd.q-no ELSE 0) + 1.*/
        CREATE quotehd.
        quotehd.quo-date = TODAY.
      END.

      ASSIGN
       li-q-no            = quotehd.q-no  /* from create trigger */
       /*quotehd.company    = cocode
       quotehd.loc        = locode
       quotehd.q-no       = li-q-no*/
       quotehd.e-num      = est.e-num
       quotehd.est-no     = est.est-no
       quotehd.cust-no    = bf-eb.cust-no
       quotehd.ship-no    = bf-eb.ship-no
       quotehd.ship-id    = bf-eb.ship-id
       quotehd.sold-no    = 1
       quotehd.part-dscr1 = bf-eb.part-dscr1
       quotehd.upd-date   = TODAY
       quotehd.quo-date = TODAY.
       quotehd.upd-user   = USERID("nosweat").
      
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
       quotehd.sman      = bf-eb.sman
       quotehd.carrier   = bf-eb.carrier
       quotehd.del-zone  = bf-eb.dest-code
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
/*         FIND FIRST bf-qhd NO-LOCK NO-ERROR. */
/*         IF AVAIL bf-qhd THEN                */
          RUN est/GetQuoteDefNotes.p (INPUT quotehd.company,
                                      OUTPUT cNotes).
            ASSIGN  quotehd.comment[1] = cNotes[1]
                    quotehd.comment[2] = cNotes[2]
                    quotehd.comment[3] = cNotes[3]
                    quotehd.comment[4] = cNotes[4]
                    quotehd.comment[5] = cNotes[5].
      END. /* ll-new-quote */

      IF est.est-type GE 3 THEN DO:
        /*FOR EACH quoteitm OF quotehd:
          DELETE quoteitm.
        END.*/

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
          WHERE bf-ef.company EQ est.company
            AND bf-ef.est-no  EQ est.est-no
            AND (bf-ef.form-no EQ bf-eb.form-no OR est.est-type EQ 2)
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
       quoteitm.i-coldscr  = IF est.est-type EQ 2 THEN eb.i-coldscr
                                                  ELSE bf-eb.i-coldscr
       quoteitm.i-dscr     = bf-ef.brd-dscr
       quoteitm.qty        = w-probeit.bl-qty
       quoteitm.uom        = "M"
       quoteitm.price      = w-probeit.sell-price
       quoteitm.upd-date   = TODAY
       quoteitm.upd-user   = USERID("nosweat")
       /*RCO400 only */
       quoteitm.i-no    = bf-eb.stock-no.

      RUN sys/inc/calcsize.p (ROWID(bf-eb), OUTPUT quoteitm.size).
  
      IF bf-ef.brd-dscr EQ '' THEN DO:
        FIND FIRST item
            WHERE item.company EQ cocode
              AND item.i-no    EQ bf-ef.board
            NO-LOCK NO-ERROR.
        IF AVAIL ITEM THEN
          quoteitm.i-dscr = IF item.i-name   GT "" THEN item.i-name   ELSE
                            IF item.est-dscr GT "" THEN item.est-dscr ELSE
                            item.i-dscr.
      END. /* if bf-ef.brd-dscr */
    END.

    FIND FIRST quoteqty
        WHERE quoteqty.company EQ quoteitm.company
          AND quoteqty.loc     EQ quoteitm.loc
          AND quoteqty.q-no    EQ quoteitm.q-no
          AND quoteqty.line    EQ quoteitm.line
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
     quoteqty.rel        = w-probeit.freight
     quoteqty.uom        = "M"
     quoteqty.price      = w-probeit.sell-price
     quoteqty.rels       = w-probeit.freight /* 1*/
     quoteqty.quote-date = /*IF ll-new-quote THEN TODAY ELSE*/ w-probeit.probe-date
     quoteqty.quote-user = USERID("nosweat")
     quoteqty.tot-lbs    = w-probeit.tot-lbs
     quoteqty.prof-on    = w-probeit.prof-on
     quoteqty.mat-cost   = w-probeit.mat-cost
     quoteqty.lab-cost   = w-probeit.lab-cost
     quoteqty.vo-cost    = w-probeit.vo-cost
     quoteqty.fo-cost    = w-probeit.fo-cost
     quoteqty.profit     = IF w-probeit.prof-on EQ "Net" THEN w-probeit.net-profit
                                                         ELSE w-probeit.gross-profit.

    /* update rfqitem qty - start */
    &SCOPED-DEFINE getrfq
    {custom/rfq-qty.i}
    /* update rfqitem qty - end */

    IF LAST-OF(w-probeit.cust-no) OR est.est-type LT 3 THEN DO:
      li-first-qty = IF est.est-type GE 3 THEN 0 ELSE w-probeit.bl-qty.

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

        IF est.est-type GE 3 THEN
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
           quotechg.code     = est-prep.code
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
           quotechg.cost     = est-prep.cost
           quotechg.spare-dec-1 = est-prep.spare-dec-1
           quotechg.amtz     = est-prep.amtz
           quotechg.prep-qty = est-prep.qty
           quotechg.s-num    = est-prep.s-num
           quotechg.b-num    = est-prep.b-num
           quotechg.simon    = est-prep.simon.

          IF ceprep-cha EQ "FiveDollar" THEN DO:
             {sys/inc/roundupfive.i quotechg.amt}
          END.
        END.

        DO j = 1 TO 6:
          IF bf-ef.mis-simon[j] EQ "S" and bf-ef.mis-cost[j] NE "" THEN DO:
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
             quotechg.prep-qty = li-prep-qty
             quotechg.quote-date = quoteqty.quote-date
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

             IF ceprep-cha EQ "FiveDollar" THEN DO:
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
         AND quoteitm.q-no    EQ ip-q-no
      BREAK BY quoteitm.part-no:
    IF NOT CAN-FIND(FIRST w-probeit WHERE w-probeit.part-no EQ quoteitm.part-no)
    THEN DELETE quoteitm.
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
  OS-DELETE VALUE(ls-xfile).
  OS-DELETE VALUE(ls-outfile) .
  
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
  DEFINE BUFFER buff-probe FOR probe .

  FIND LAST est-summ
      WHERE est-summ.company EQ est.company
        AND est-summ.est-no  EQ est.est-no
      USE-INDEX est-qty NO-LOCK NO-ERROR.
  li1 = IF AVAIL est-summ THEN est-summ.eqty ELSE 0.

  FOR EACH mclean:
    DO li = 1 TO 28:
      FOR EACH buff-probe
          WHERE buff-probe.company    EQ est.company
            AND buff-probe.est-no     EQ est.est-no
            AND buff-probe.probe-date EQ TODAY
            AND buff-probe.est-qty    EQ qtty[li]
            AND buff-probe.freight    EQ IF est.est-type LE 6 THEN rels[li] ELSE 1
          NO-LOCK
          BY buff-probe.probe-time DESC:

        CREATE est-summ.
        ASSIGN
         est-summ.company  = buff-probe.company
         est-summ.est-no   = buff-probe.est-no
         li1               = li1 + 1
         est-summ.eqty     = li1
         est-summ.summ-tot = STRING(mclean.rec-type,"x(20)")     +
                             STRING(mclean.form-no,"9999999999") +
                             mclean.descr
         est-summ.e-num    = buff-probe.line
         est-summ.per-m    = mclean.cost[li].

        LEAVE.
      END.
    END.

    DELETE mclean.
  END.

  RELEASE est-summ.

  IF est.est-type EQ 2 AND vmclean THEN
  DO li = 1 TO 28:
    IF qtty[li] EQ 0 THEN NEXT.

    FOR EACH buff-probe
        WHERE buff-probe.company    EQ est.company
          AND buff-probe.est-no     EQ est.est-no
          AND buff-probe.probe-date EQ TODAY
          AND buff-probe.est-qty    EQ qtty[li]
          AND buff-probe.freight    EQ rels[li]
        NO-LOCK
        BY buff-probe.probe-time DESC:
      LEAVE.
    END.

    IF AVAIL buff-probe THEN RUN ce/pr4-mcl1.p (ROWID(buff-probe)).
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
  

  {est/checkuse.i}

  RUN save-fields.

  DO WITH FRAME {&FRAME-NAME}:
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

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF ll-use-margin THEN DO:         /* Get Commission% */
    RUN est/getsmanmtrx.p (ROWID(est), "C",
                           INPUT-OUTPUT probe.comm,
                           INPUT-OUTPUT probe.market-price).

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

  FIND FIRST ce-ctrl {sys/look/ce-ctrlW.i} NO-LOCK.

  FIND FIRST est NO-LOCK
      WHERE est.company EQ probe.company
        AND est.est-no  EQ probe.est-no
      NO-ERROR.

  IF ce-ctrl.sell-by EQ "S" THEN
    probe.gross-profit = (1 - (100 / (100 + probe.gross-profit))) * 100.

  FOR EACH probeit
      WHERE probeit.company EQ probe.company
        AND probeit.est-no  EQ probe.est-no
        AND probeit.line    EQ probe.line:
    ASSIGN
     ld          = (IF est.est-type LT 3 OR
                       probeit.yrprice   THEN probeit.yld-qty
                                         ELSE probeit.bl-qty) / 1000
     ld-tot-fact = ld-tot-fact + (probeit.fact-cost  * ld)
     ld-tot-full = ld-tot-full + (probeit.full-cost  * ld)
     ld-tot-pric = ld-tot-pric + (probeit.sell-price * ld).
  END.

  ld = probe.est-qty / 1000.

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

  FOR EACH probeit
      WHERE probeit.company EQ probe.company
        AND probeit.est-no  EQ probe.est-no
        AND probeit.line    EQ probe.line:
    ASSIGN
     ld          = (IF est.est-type LT 3 OR
                       probeit.yrprice   THEN probeit.yld-qty
                                         ELSE probeit.bl-qty) / 1000
     ld-tot-fact = probeit.fact-cost  / ld
     ld-tot-full = probeit.full-cost  / ld
     ld-tot-pric = probeit.sell-price / ld.
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

  IF NOT adm-new-record THEN DO:
    {methods/template/local/delete.i}
  END.

  IF probe.LINE LT 100 THEN
  DO:
     RUN get-dir-proc(INPUT trim(est.est-no) + ".p" + string(probe.line,"99"),
                      OUTPUT tmp-dir).

     IF OPSYS EQ "unix" THEN
        UNIX SILENT rm VALUE(tmp-dir + TRIM(est.est-no) + ".p" + STRING(probe.LINE,"99")).
     else
        DOS SILENT DEL VALUE(tmp-dir + TRIM(est.est-no) + ".p" + STRING(probe.LINE,"99")).
  END.
  ELSE
  DO:
     RUN get-dir-proc(INPUT trim(est.est-no) + ".p" + string(probe.line,"999"),
                      OUTPUT tmp-dir).

     IF OPSYS EQ "unix" THEN
        UNIX SILENT rm VALUE(tmp-dir + TRIM(est.est-no) + ".p" + STRING(probe.LINE,"999")).
     else
        DOS SILENT DEL VALUE(tmp-dir + TRIM(est.est-no) + ".p" + STRING(probe.LINE,"999")).
  END.

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
  FIND FIRST ce-ctrl {sys/look/ce-ctrlW.i} NO-LOCK.

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
  /* rstark - task 11061210 - when maximized, hinders proper functionality
  DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY "cursor-left" TO {&BROWSE-NAME}.
    END.
  END. */

  {est/checkuse.i}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN save-fields.
  RUN release-shared-buffers.

  DO WITH FRAME {&FRAME-NAME}:
    probe.do-quote:SCREEN-VALUE IN BROWSE {&browse-name} = "Y".
    APPLY "entry" TO probe.market-price IN BROWSE {&browse-name}.
  END.

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
  
  find xest where recid(xest) = recid(est) NO-LOCK NO-ERROR.
  find xef where recid(xef) = recid(ef) NO-LOCK NO-ERROR.
  find xeb where recid(xeb) = recid(eb) NO-LOCK NO-ERROR.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ll-use-margin = NO.

  IF cerunf EQ "Fibre" THEN
    RUN est/usemargin.p (ROWID(est), OUTPUT ll-use-margin).

  ASSIGN
     probe.gross-profit:VISIBLE IN BROWSE {&browse-name} = NOT(ll-use-margin AND cerunf = "Fibre" AND cerunc = "Fibre")
     probe.comm:VISIBLE IN BROWSE {&browse-name} = NOT probe.gross-profit:VISIBLE IN BROWSE {&browse-name}.
  
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
  DEF VAR ld-price LIKE probe.sell-price NO-UNDO.


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

  IF est.est-type EQ 2 THEN DO:
    ld-price = probe.sell-price.

    RUN ce/uprobit2.p (RECID(probe)).

    RUN recalc-fields (ld-price).

    probe.sell-price:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(ld-price).
    
    RUN dispatch ("assign-record").

    RUN dispatch ("end-update"). 
  END.

  ELSE RUN ce/uprobit3.p (BUFFER probe).

  FIND xest WHERE ROWID(xest) EQ ROWID(est) NO-LOCK NO-ERROR.

  IF est.est-type EQ 4 THEN DO:
    {ce/com/probe.u}
    RUN per-1000.
  END.

  ELSE
  IF est.est-type EQ 3 THEN RUN ce/tan/probeu.p (ROWID(probe)).

  ELSE
  IF vmclean THEN RUN ce/pr4-mcl1.p (ROWID(probe)).

  DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY "cursor-left" TO {&BROWSE-NAME}.
    END.
  END.

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
      IF CAN-DO(usergrps.users,USERID("NOSWEAT")) THEN
         g_groups = g_groups + usergrps.usergrps + ",".
  END.

  DO num-groups = 1 TO NUM-ENTRIES(g_groups):
     IF NOT CAN-DO(b-prgrms.can_update,ENTRY(num-groups,g_groups)) THEN
        NEXT.
    
     IF NOT v-can-update AND CAN-DO(b-prgrms.can_update,ENTRY(num-groups,g_groups)) THEN
        v-can-update = YES.
  END.
  
  IF NOT v-can-update AND CAN-DO(b-prgrms.can_update,USERID("NOSWEAT")) THEN
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

    RUN ce/probeu3.p (ROWID(b-probe)).
  END.

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
 DEF INPUT PARAM ip-from-dept AS cha NO-UNDO.
 DEF INPUT PARAM ip-to-dept AS cha NO-UNDO.
 DEF OUTPUT PARAM op-error AS LOG NO-UNDO.


{custom/notesdef.i}
DEF VAR v-inst2 AS cha EXTENT 20 NO-UNDO.    
DEF VAR v-dept-inst AS cha FORM "x(78)" EXTENT 20 NO-UNDO.
DEF VAR v-spec-inst AS cha FORM "x(78)" EXTENT 10 NO-UNDO.
DEF VAR v-note-length AS INT INIT 80 NO-UNDO.
DEF VAR lv-form-note AS cha NO-UNDO.

OS-COPY VALUE(ls-outfile) VALUE(ls-notefile).

OUTPUT TO value(ls-notefile) APPEND.

{custom/notespr3.i est v-inst2 20 "notes.rec_key = est.rec_key and notes.note_code >= ip-from-dept and notes.note_code <= ip-to-dept" }
DO i = 1 TO 20:
             v-dept-inst[i] = v-inst2[i].
END.
        
PUT skip(1)
    "======================================= NOTES ==================================" SKIP
    "Instruction:" skip
    v-dept-inst[1] SKIP
    /*        "            " */
    v-dept-inst[2] SKIP
            v-dept-inst[3] SKIP
            v-dept-inst[4] SKIP
             v-dept-inst[5] SKIP
             v-dept-inst[6] SKIP
             v-dept-inst[7] SKIP
            v-dept-inst[8] SKIP
             v-dept-inst[9] SKIP
             v-dept-inst[10] SKIP
             v-dept-inst[11] SKIP
             v-dept-inst[12] SKIP
             v-dept-inst[13] SKIP
             v-dept-inst[14] SKIP
             v-dept-inst[15] SKIP
             v-dept-inst[16] SKIP
             v-dept-inst[17] SKIP
             v-dept-inst[18] SKIP
             v-dept-inst[19] SKIP
             v-dept-inst[20] SKIP
            .

        /* spec note */        
        FIND FIRST itemfg WHERE itemfg.company = job-hdr.company
                            AND itemfg.i-no = job-hdr.i-no NO-LOCK NO-ERROR.
        {custom/notesprt.i itemfg v-inst2 10 }
        DO i = 1 TO 10:
             v-spec-inst[i] = v-inst2[i].
        END.

        
        PUT "Spec Notes: " skip
            v-spec-inst[1] SKIP
            v-spec-inst[2] SKIP
            v-spec-inst[3] SKIP
            v-spec-inst[4] SKIP
            v-spec-inst[5] SKIP
            v-spec-inst[6] SKIP
            v-spec-inst[7] SKIP
            v-spec-inst[8] SKIP
            v-spec-inst[9] SKIP
            v-spec-inst[10] SKIP
            .

    OUTPUT CLOSE.

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
  def var lv-ef-recid as recid no-undo.
 
  ASSIGN
    lv-ef-recid = recid(ef)
    tmp-dir = lv-cebrowse-dir.

  {ce/print4p.i}

  RUN release-shared-buffers.

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
  vprint = YES.
  DEF VAR module AS cha NO-UNDO.
  module = str-tit.

  tmp-dir = ip-tmp-dir.

  
  {ce/print42p.i}

  /*RUN release-shared-buffers.*/

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
  DEFINE VARIABLE is-xprint-form AS LOGICAL NO-UNDO.
  DEF VAR v-error AS LOG NO-UNDO.
  DEF VAR lv-dest AS int NO-UNDO.
  DEF VAR lv-font AS INT NO-UNDO.
  DEF VAR lv-ornt AS CHAR NO-UNDO.
  DEF VAR lv-lines AS INT NO-UNDO.
  DEF VAR list-name AS CHAR NO-UNDO.
  DEF VAR init-dir AS CHAR NO-UNDO.
  DEF VAR ls-fax-file AS CHAR NO-UNDO.
  DEF VAR ret-code AS INT NO-UNDO.
  DEF VAR ls-mail-file2 AS CHAR NO-UNDO.
  DEF VAR lv-pdf-file AS CHAR NO-UNDO.
  DEF VAR ls-outfile2 AS cha NO-UNDO.
  DEF VAR v-cinput AS cha FORM "x(255)" NO-UNDO.
  DEF VAR ls-outfile-saved AS cha NO-UNDO.
  DEF VAR lv-input-vars AS cha EXTENT 3 NO-UNDO.
  DEF VAR ls-outfile3 AS CHAR NO-UNDO.
  DEF VAR v-probe-fmt AS CHAR NO-UNDO.

  {est/checkuse.i}

  PAUSE 2 NO-MESSAGE.

  /* notes print*/
  IF ipPrompt THEN DO:
    RUN est/d-estprt.w (OUTPUT v-prt-note,OUTPUT v-prt-box,OUTPUT v-from-dept,
                        OUTPUT v-to-dept,OUTPUT lv-dest,OUTPUT lv-font,
                        OUTPUT lv-ornt,OUTPUT lv-lines,OUTPUT v-error, OUTPUT v-print-cm).
    IF v-error THEN RETURN ERROR.
  END.
  ELSE
  ASSIGN
    v-prt-note = YES
    v-prt-box = YES
    v-from-dept = ''
    v-to-dept = 'zzzzz'
    lv-dest = 2
    lv-font = 15
    lv-ornt = 'P'
    lv-lines = 100.

  find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name eq "CEPrint" no-lock no-error.
  ASSIGN
     is-xprint-form = avail sys-ctrl and sys-ctrl.char-fld ne 'Text'
     v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999".

  RUN get-dir-proc(INPUT TRIM(est.est-no) + ".p" + STRING(probe.line,v-probe-fmt),
                   OUTPUT tmp-dir).
  
  ASSIGN
    ls-outfile = tmp-dir + trim(est.est-no) + ".p" + string(probe.line,v-probe-fmt)
    ls-outfile3 = lv-cebrowse-dir + trim(est.est-no) + ".p" + string(probe.line,v-probe-fmt)
    ls-xfile = lv-cebrowse-dir + trim(est.est-no) + ".x" + string(probe.line,v-probe-fmt)
    ls-outfile2 = lv-cebrowse-dir + trim(est.est-no) + ".z" + string(probe.line,v-probe-fmt).
  
  RUN get-dir-proc(INPUT TRIM(est.est-no) + ".n" + STRING(probe.line,v-probe-fmt),
                   OUTPUT tmp-dir).
  
  ls-notefile = tmp-dir + trim(est.est-no) + ".n" + string(probe.line,v-probe-fmt).

  FIND xest WHERE ROWID(xest) EQ ROWID(est) NO-ERROR.

  IF xest.est-type LT 3 THEN RUN ce/probeu1.p (RECID(probe)).
            

  FIND xest WHERE ROWID(xest) EQ ROWID(est) NO-LOCK NO-ERROR.

  v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999".

  RUN get-dir-proc(INPUT trim(est.est-no) + ".s" + string(probe.line,v-probe-fmt),
                   OUTPUT tmp-dir).

  if opsys eq "unix" THEN
     unix silent cp  value(tmp-dir + trim(est.est-no) + ".s" + string(probe.line,v-probe-fmt))
                     value(ls-outfile3).
  ELSE DO:
     OUTPUT TO VALUE(ls-outfile3).

     input from value(tmp-dir + trim(est.est-no) + ".s" + string(probe.line,v-probe-fmt)) NO-ECHO.
     repeat:
           v-cinput = "".
           import unformatted v-cinput.
           if v-cinput eq "" then put skip(1).
           else put unformatted v-cinput skip.
     end.

     input close.
     OUTPUT CLOSE.
  END.

  /* have page skip for VIEW */
      IF is-xprint-form THEN lv-lines = 63.
      OUTPUT TO VALUE(ls-outfile2) PAGE-SIZE VALUE(lv-lines). /*actual size is 63*/ /* create .x file with page size */
      lv-input-vars = "".
      i = 0.

      input from value(ls-outfile3) NO-ECHO.
      repeat:
                v-cinput = "".
                import unformatted v-cinput.
                i = i + 1.
                IF i < 4 THEN
                   lv-input-vars[i] = v-cinput.

                IF (LINE-COUNTER >= 51 AND
                   TRIM(v-cinput) BEGINS "E S T I M A T E" AND ceprint-char = "Segment")
                   OR LINE-COUNTER > lv-lines THEN do:
                   PAGE.
                   PUT lv-input-vars[1] FORM "x(100)" SKIP
                       lv-input-vars[2] SKIP
                       lv-input-vars[3] FORM "x(30)" SKIP(1).
                END.
                if v-cinput eq "" then put skip(1).
                else do:
                    /*IF LOOKUP(SUBSTRING(v-cinput,1,2),"01,02,03,04,05,06,07,08,09,10,11,12") > 0 and
                       SUBSTRING(v-cinput,3,1) EQ "/" AND
                       SUBSTRING(v-cinput,6,1) EQ "/"    THEN PAGE. /*seperate page per form*/
                    */   
                    put unformatted v-cinput skip.
                END.
      end.
      input close.
      OUTPUT CLOSE.
  ASSIGN ls-outfile-saved = ls-outfile3
         ls-outfile = ls-outfile2.
  /* end of page skip mods*/
  
  IF is-xprint-form THEN DO:
    OUTPUT TO VALUE(ls-xfile).
    IF lv-dest = 1 THEN PUT "<PRINTER?></PROGRESS>".  /*<REVIEW>*/      
    ELSE IF lv-dest EQ 2 THEN DO:
      IF NOT lBussFormModle THEN
         PUT "<PREVIEW=115><MODAL=NO></PROGRESS>".
      ELSE
         PUT "<PREVIEW=115></PROGRESS>".        
    END.       
    ELSE IF lv-dest = 4 THEN DO:
       ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".tif".
       PUT UNFORMATTED "<PRINTER?><EXPORT=" Ls-fax-file ",BW></PROGRESS>".
    END.        
    ELSE IF lv-dest = 5 THEN DO:
        ASSIGN
           init-dir = v-dir
           lv-pdf-file = init-dir + "Est" + TRIM(est.est-no).
        PUT "<PREVIEW><PDF-LEFT=5mm><PDF-TOP=10mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf></PROGRESS>" FORM "x(180)".
    END.
    OUTPUT CLOSE.
  END.
  
  IF v-prt-note AND SEARCH(ls-outfile) <> ? THEN
  RUN print-notes(v-from-dept,v-to-dept,OUTPUT v-error).
  
  list-name = IF v-prt-note THEN ls-notefile ELSE ls-outfile.
  dos silent type value(list-name) >> value(ls-xfile).
  
  IF is-xprint-form THEN
  list-name = ls-xfile.

  IF v-prt-box THEN DO:
    FIND FIRST box-design-hdr NO-LOCK
         WHERE box-design-hdr.design-no EQ 0
           and box-design-hdr.company EQ eb.company 
           AND box-design-hdr.est-no EQ eb.est-no
           and box-design-hdr.form-no EQ eb.form-no
           and box-design-hdr.blank-no EQ eb.blank-no NO-ERROR.
    IF AVAILABLE box-design-hdr AND
       SEARCH(box-design-hdr.box-image) NE ? THEN DO:
      OUTPUT TO VALUE(list-name) APPEND.
      PUT UNFORMATTED 'Estimate#: ' TRIM(est.est-no) ' - Box Image' SKIP(1)
        '<C1><#30><R+55><C+80><IMAGE#30=' box-design-hdr.box-image '>' SKIP.
      OUTPUT CLOSE.
    END.
  END.
  
  FILE-INFO:FILE-NAME = list-name.

  IF is-xprint-form THEN
  CASE lv-dest:
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
      run custom/xpmail.p ("CUSTOMER",lv-pdf-file + ".pdf","",
                        'Estimate',
                        'Estimate',OUTPUT ret-code).
    END. 
    WHEN 6 THEN RUN custom/d-print.w (list-name).
  END CASE.
  ELSE RUN textFormat (lv-dest,list-name,lv-font,lv-ornt,ls-fax-file,ls-mail-file2).
  
  OS-DELETE VALUE(ls-notefile).
/*  OS-DELETE VALUE(ls-xfile). - This was causing an issue once the business form Modal was introduced.  
Note, this process of deleting files is different than in EC for some reason*/
  OS-DELETE VALUE(ls-outfile).
  ls-outfile = ls-outfile-saved.

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
  DEF INPUT PARAM ip-price LIKE probe.sell-price NO-UNDO.

  DEF VAR ld-commc AS DEC NO-UNDO.
  DEF VAR ld-factc AS DEC NO-UNDO.
  DEF VAR ld-fullc AS DEC NO-UNDO.
  DEF VAR ld-price AS DEC NO-UNDO.


  {cec/combasis.i}

  DO WITH FRAME {&FRAME-NAME}:
    ld-price = 0.
    FOR EACH probeit
        WHERE probeit.company EQ probe.company
          AND probeit.est-no  EQ probe.est-no
          AND probeit.line    EQ probe.line
        NO-LOCK:
    
      ld-price = ld-price +
                 (probeit.sell-price * (probeit.yld-qty / probe.est-qty)).
    END.

    lv-changed = IF ld-price EQ ip-price THEN "" ELSE "S".

    probe.sell-price:SCREEN-VALUE IN BROWSE {&browse-name} =
      STRING(ld-price,probe.sell-price:FORMAT IN BROWSE {&browse-name}).

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
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    RUN dispatch ('open-query').
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN RUN dispatch ("row-changed").
  END.


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
  def var lv-eb-recid as recid no-undo.
  def var lv-ef-recid as recid no-undo.
  DEF VAR tmp-outfile AS cha NO-UNDO.
  DEF VAR viewfile AS cha NO-UNDO.
  DEF VAR li AS INT NO-UNDO.

  {est/checkuse.i}
  EMPTY TEMP-TABLE xprep.

  FIND CURRENT est.
  find xef where recid(xef) = recid(ef).
  find xeb where recid(xeb) = recid(eb).
  vprint = yes.
  lv-eb-recid = recid(eb).
  lv-ef-recid = recid(ef).
  
  FOR EACH mclean:
    DELETE mclean.
  END.

  FOR EACH est-op NO-LOCK
      WHERE est-op.company EQ est.company
        AND est-op.est-no  EQ est.est-no
        AND est-op.line    LT 500,
      FIRST mach NO-LOCK
      {sys/look/machW.i}
        AND mach.m-code EQ est-op.m-code:
   IF mach.obsolete THEN DO:
    MESSAGE "Machine: " + TRIM(mach.m-code) +
            " is obsolete, please replace to complete calculation..."
        VIEW-AS ALERT-BOX ERROR.
    RETURN.
   END.
  END.
  IF est.est-type >= 3 AND est.est-type <= 4 AND cerunf = "HOP" THEN RUN ce/dAskSum.w (OUTPUT gEstSummaryOnly).
  IF est.est-type EQ 4 THEN RUN ce/com/print4.p NO-ERROR.

  ELSE
  IF est.est-type EQ 3 THEN RUN ce/tan/print4.p NO-ERROR.

  ELSE DO:
    FIND FIRST probe WHERE probe.company = est.company
                       AND probe.est-no = est.est-no
                     NO-LOCK NO-ERROR.
    IF AVAIL probe THEN RUN est/d-probeu.w (OUTPUT lv-override).

    IF est.est-type EQ 1 THEN RUN print4 NO-ERROR.
                         ELSE RUN print42 (lv-cebrowse-dir) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN DO:
      SESSION:SET-WAIT-STATE("").
      RETURN.
    END.

    RUN est-summ.
  END.

  for each est-op where est-op.company = est.company and
                        est-op.est-no = est.est-no and est-op.line > 500 
                        exclusive-lock :
           delete est-op.  
  end.

  RUN release-shared-buffers.

  session:set-wait-state("").

  find eb where recid(eb) = lv-eb-recid no-lock.
  find ef where recid(ef) = lv-ef-recid no-lock.
  FIND CURRENT est NO-LOCK NO-ERROR.

  RUN dispatch ("open-query").
  RUN dispatch ("open-query").

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
     /*lv-brd-%   = reftable.val[3]:SCREEN-VALUE IN BROWSE {&browse-name}
     lv-brdcm   = reftable.val[4]:SCREEN-VALUE IN BROWSE {&browse-name}
     lv-brdc$   = reftable.val[3]:SCREEN-VALUE IN BROWSE {&browse-name}*/.

    IF probe.comm:VISIBLE IN BROWSE {&browse-name} THEN
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE textformat B-table-Win 
PROCEDURE textformat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipDest AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipListName AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipFont AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipOrnt AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipFaxFile AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMailFile AS CHARACTER NO-UNDO.

  DEFINE VARIABLE returnCode AS INTEGER NO-UNDO.
  DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
  DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

  list-name = ipListName.
  CASE ipDest:
    WHEN 1 THEN RUN custom/prntproc.p (ipListName,ipFont,ipOrnt).
    WHEN 2 THEN RUN scr-rptest.w (ipListName,"Estimate Analysis",ipFont,ipOrnt). 
    WHEN 3 THEN DO:
      {custom/out2file.i}
    END.
    WHEN 4 THEN DO:
      ipFaxFile = "c:\tmp\fax" + STRING(TIME) + ".txt". 
      OS-COPY VALUE(ipListName) VALUE(ipFaxFile).
      RUN custom/asifax.p ("",ipFaxFile,"",
                        'Estimate',
                        'Estimate',OUTPUT returnCode).
    END.   
    WHEN 5 THEN DO:
      ipMailFile = v-dir + "att" + STRING(TIME) + ".txt". 
      OS-COPY VALUE(ipListName) VALUE(ipMailFile).
      RUN custom/xpmail.p ("CUSTOMER",ipMailFile,"",
                        'Estimate',
                        'Estimate',OUTPUT returnCode).
    END. 
    WHEN 6 THEN RUN custom/d-print.w (ipListName).
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
  DEF VAR ld-price LIKE probe.sell-price NO-UNDO.
  DEF VAR v-probe-fmt AS CHAR NO-UNDO.
  DEF VAR v-old-price AS DEC NO-UNDO.
  DEF VAR v-old-full-cost AS DEC NO-UNDO.
  DEF VAR ld-commc AS DEC NO-UNDO.
  DEF VAR ld-fullc AS DEC NO-UNDO.
  DEF VAR ld-marg% AS DEC NO-UNDO.

  DEF BUFFER probe-ref FOR reftable.

  {est/checkuse.i}

  IF est.est-type NE 1 AND AVAIL probe THEN DO:
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

      IF est.est-type EQ 2 THEN DO:
        ld-price = probe.sell-price.

        RUN ce/uprobit2.p (RECID(probe)). 

        RUN recalc-fields (ld-price).
    
        RUN dispatch ("assign-record").

        RUN dispatch ("end-update").

        IF vmclean THEN RUN ce/pr4-mcl1.p (ROWID(probe)).
      END.

      ELSE DO:
        {cec/combasis.i}
          
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

        ASSIGN
         v-com              = probe.comm
         qm                 = probe.est-qty / 1000
         v-old-price        = probe.sell-price
         v-old-full-cost    = probe.full-cost
         probe.sell-price   = ROUND(ld-tot-pric / qm,2)
         probe.fact-cost    = ROUND(ld-tot-fact / qm,2)
         probe.full-cost    = ROUND(ld-tot-full / qm,2)
         probe.net-profit   = (1 - (probe.full-cost / probe.sell-price)) * 100
         probe.gross-profit = (1 - (probe.fact-cost / probe.sell-price)) * 100
         /*probe.net-profit   = probe.net-profit   - v-com
         probe.gross-profit = probe.gross-profit - v-com*/.

        IF ll-use-margin THEN
        DO:
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

        ASSIGN
         v-prf-s = probe.sell-price - probe.fact-cost
         v-pct-s = v-prf-s / probe.fact-cost * 100.

        RUN ce/uprobit3.p (BUFFER probe).

        {ce/com/probeu.i}

        IF probe.LINE LT 100 THEN
           v-probe-fmt = "99".
        ELSE
           v-probe-fmt = "999".

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


  FIND FIRST ce-ctrl {sys/look/ce-ctrlW.i} NO-LOCK.

  DO WITH FRAME {&FRAME-NAME}:
    lv-gp = IF ce-ctrl.sell-by EQ "S" THEN
              IF ip-type EQ 1 THEN
                (probe.sell-price - probe.fact-cost) / probe.fact-cost * 100
              ELSE
                (DEC(probe.sell-price:SCREEN-VALUE IN BROWSE {&browse-name}) -
                 DEC(probe.fact-cost:SCREEN-VALUE IN BROWSE {&browse-name})) /
                DEC(probe.fact-cost:SCREEN-VALUE IN BROWSE {&browse-name}) * 100
            ELSE
              IF ip-type EQ 1 THEN
                probe.gross-profit
              ELSE
                DEC(gross-profit:SCREEN-VALUE IN BROWSE {&browse-name}).
  END.

  RETURN lv-gp.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fDirectMatPctSellPrice B-table-Win 
FUNCTION fDirectMatPctSellPrice RETURNS DECIMAL
  (  ):
/*------------------------------------------------------------------------------
 Purpose:  Calculates Mat %
 Notes: Ticket 24941 
------------------------------------------------------------------------------*/
       DEFINE VARIABLE dMatPct AS DECIMAL NO-UNDO.
        DEFINE VARIABLE dPrice AS DECIMAL NO-UNDO.
    
    dPrice = DEC(probe.sell-price).
    IF AVAILABLE probe AND dPrice GT 0 THEN 
        dMatPct = probe.spare-dec-1 / dPrice * 100.
    
        RETURN dMatPct.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION voverall B-table-Win 
FUNCTION voverall RETURNS DECIMAL
  ( INPUT ip-type AS INT )  :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR lv-overall AS DEC NO-UNDO.


  IF AVAIL probe THEN
    lv-overall = ROUND((IF ip-type EQ 1 THEN probe.sell-price
                                        ELSE DEC(probe.sell-price:SCREEN-VALUE IN BROWSE {&browse-name}))
                 / probe.bsf,2).

  ELSE lv-overall = 0.

  IF lv-overall = ? then lv-overall = 0.
   
  RETURN lv-overall.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION vtot-lbs B-table-Win 
FUNCTION vtot-lbs RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var lv-tot-lbs as dec no-undo.

  
  if avail probe then lv-tot-lbs = probe.tot-lbs.
  else lv-tot-lbs = 0.

  RETURN lv-tot-lbs.   /* Function return value. */

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

  
  if avail probe then lv-tot-msf = probe.tot-lbs / 1000.
  else lv-tot-msf = 0.

  RETURN lv-tot-msf.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

