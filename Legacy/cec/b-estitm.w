&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: cec\b-estitm.w

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

{custom/gcompany.i}    
{custom/gloc.i}
{custom/persist.i}
{est/ttInputEst.i NEW}
    
DEF VAR ls-add-what AS cha NO-UNDO.
DEF VAR ll-add-set AS LOG NO-UNDO INIT NO.
DEF VAR ll-add-set-part AS LOG NO-UNDO.
DEF VAR ll-add-set-part-2 AS LOG NO-UNDO.
DEF VAR li-new-estnum AS INT NO-UNDO.
DEF VAR ll-new-record AS LOG NO-UNDO.
DEF VAR ll-new-est AS LOG NO-UNDO.
DEF VAR lv-ef-recid AS RECID NO-UNDO.
DEF VAR lv-eb-recid AS RECID NO-UNDO.
DEF VAR ll-is-copy-record AS LOG NO-UNDO.
DEF VAR char-val AS cha NO-UNDO.
DEF NEW SHARED BUFFER xest FOR est.
DEF NEW SHARED BUFFER xef FOR ef.
DEF NEW SHARED BUFFER xeb FOR eb.
DEF NEW SHARED BUFFER xqty FOR est-qty.
DEF NEW SHARED VAR formule AS INT EXTENT 12 NO-UNDO.
DEF NEW SHARED BUFFER xoe-ord FOR oe-ord.
DEF VAR lv-part-no-prev LIKE eb.part-no NO-UNDO.
DEF VAR is-item-copied-from-est AS LOG NO-UNDO.
DEF VAR li-form# LIKE ef.form-no NO-UNDO.
DEF VAR li-est-form-qty LIKE est.form-qty NO-UNDO.
DEF VAR ls-cust-no AS cha NO-UNDO.
DEF VAR ls-ship-id AS cha NO-UNDO.
DEF VAR ls-set-part-no AS cha NO-UNDO.  /* set part-no from local-create-record*/
DEF VAR lv-crt-est-rowid AS ROWID NO-UNDO.  /* refreshing for new record */
DEF VAR lv-eb-copy-frid AS RECID NO-UNDO.
DEF VAR lv-ef-copy-frid AS RECID NO-UNDO.
DEF VAR lv-copy-what AS cha NO-UNDO.   /* Blank or Form */
DEF VAR lv-cad-no LIKE eb.cad-no NO-UNDO.
DEF VAR lv-die-no LIKE eb.die-no NO-UNDO.
DEF VAR v-l-array AS DEC EXTENT 30 NO-UNDO.
DEF VAR li-flen AS INT EXTENT 2 NO-UNDO.
DEF VAR v-board2 AS CHAR NO-UNDO.
DEF VAR v-part-no2 AS CHAR NO-UNDO.
DEF VAR v-assem-grain AS CHAR NO-UNDO.
DEF VAR v-auto-add-item AS LOG NO-UNDO.
DEF VAR v-ds AS LOG VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE cPackCodeOverride AS CHARACTER NO-UNDO.
DEF NEW SHARED VAR s-est-no AS cha NO-UNDO.  /* for fgadd2.p */
DEF NEW SHARED VAR fil_id AS RECID NO-UNDO.
DEF NEW SHARED VAR v-i-item LIKE eb.stock-no NO-UNDO. /* INPUT ITEM */
DEF NEW SHARED VAR v-i-qty LIKE eb.bl-qty NO-UNDO.   /* INPUT QUANTITY */

&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

{sys/inc/var.i "new" "shared"}

DEF NEW SHARED TEMP-TABLE formule FIELD formule AS DEC EXTENT 12.
DEF VAR lv-last-est-cat AS CHAR NO-UNDO INIT "".
DEF VAR ls-prev-val AS cha NO-UNDO.
DEF VAR lv-copy-qty AS INT EXTENT 20 NO-UNDO.
DEF VAR lv-copy-pr AS DEC EXTENT 20 NO-UNDO.
DEF VAR lv-copy-uom AS cha EXTENT 20 NO-UNDO.
DEF VAR lv-copy-date AS DATE EXTENT 20 NO-UNDO.
DEF VAR lv-estqty-recid AS RECID NO-UNDO.
DEF VAR lv-foam AS LOG NO-UNDO.
DEF VAR lv-copied AS ROWID NO-UNDO.
{cec/descalc.i "new"}
DEF NEW SHARED VAR xcal AS DEC NO-UNDO.
DEF NEW SHARED VAR sh-wid AS DEC NO-UNDO.
DEF NEW SHARED VAR sh-len AS DEC NO-UNDO.
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO.
DEF VAR ll-is-add-from-tool AS LOG NO-UNDO.
DEF VAR ll-crt-itemfg AS LOG NO-UNDO.
DEF VAR lv-frst-rowid AS ROWID NO-UNDO.
DEF VAR lv-last-rowid AS ROWID NO-UNDO.
DEF VAR lv-frst-rowid2 AS ROWID NO-UNDO.
DEF VAR lv-last-rowid2 AS ROWID NO-UNDO.
DEF VAR ll-part-no AS LOG NO-UNDO.
DEF VAR ll-new-shipto AS LOG NO-UNDO.
DEF VAR ll-mass-del AS LOG NO-UNDO.
DEF VAR uom-list AS cha NO-UNDO.
DEF VAR lv-hold-flute LIKE eb.flute NO-UNDO.
DEF VAR lv-hold-test LIKE eb.test NO-UNDO.
DEF VAR lv-repo AS CHAR INIT "ON" NO-UNDO.
DEFINE VARIABLE cadcamValue AS CHARACTER NO-UNDO.
DEF VAR lv-Persistent-Handle AS HANDLE NO-UNDO.
DEF VAR lv-label AS CHAR EXTENT 20 NO-UNDO.
DEF VAR ll-form AS LOG NO-UNDO.
DEF VAR ll-warn AS LOG NO-UNDO.
DEF VAR ll-wid-len-warned AS LOG NO-UNDO.
DEF VAR old-bl-qty LIKE eb.bl-qty NO-UNDO.
DEF VAR ll-tandem AS LOG NO-UNDO.

DEF VAR v-style2 AS CHAR NO-UNDO.
DEF VAR v-no-forms AS INT NO-UNDO.
DEF VAR v-in-cell-w AS DEC NO-UNDO.
DEF VAR v-end-cell-w1 AS DEC NO-UNDO.
DEF VAR v-end-cell-w2 AS DEC NO-UNDO.
DEF VAR v-qty-set-2 AS INT NO-UNDO.
DEF VAR v-wid-2 AS DEC DECIMALS 4 NO-UNDO.
DEF VAR v-height-2 AS DEC DECIMALS 4 NO-UNDO.
DEF VAR v-part-desc3 AS CHAR NO-UNDO.
DEF VAR v-part-desc4 AS CHAR NO-UNDO.
DEF VAR v-qty AS INT NO-UNDO.
DEF VAR v-stock-no2 AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEF VAR lv-save-set AS CHAR NO-UNDO.
DEF VAR lv-set-widget AS HANDLE NO-UNDO.
DEF VAR v-rowid-eb AS ROWID NO-UNDO.

DEF NEW SHARED TEMP-TABLE tt-eb-set NO-UNDO LIKE eb.

DEF TEMP-TABLE tt-eb NO-UNDO LIKE eb FIELD row-id AS ROWID INDEX row-id row-id.
DEF TEMP-TABLE tt-est-op NO-UNDO LIKE est-op.
DEF TEMP-TABLE tt-stock-no 
    FIELD eb-row-id AS ROWID
    FIELD stock-no LIKE eb.stock-no.
DEF TEMP-TABLE tt-e-vend 
    FIELD e-vend-row AS ROWID .
RUN Get-Company  (OUTPUT gcompany).
RUN Get-location (OUTPUT gloc).

ASSIGN cocode = gcompany
       locode = gloc.
{oe/oe-sysct1.i NEW}
{sys/ref/CustList.i NEW}
DO TRANSACTION:
  {ce/cecopy.i}
  {sys/inc/cegoto.i}
  {sys/inc/cadcam.i}
  {sys/inc/ceroute.i C}
  {sys/inc/cestyle.i C}
  {sys/inc/cefgitem.i}
  {sys/inc/graphic.i}
  {sys/inc/ecbrowse.i}
  {sys/inc/setprint.i}
  {sys/inc/shiptorep.i}
  {sys/inc/custlistform.i ""EC""}
END.
RUN oe/oe-sysct.p.

{sys/inc/f16to32.i}

{methods/defines/hndldefs.i}

{est/artiosvar.i "new shared"}
{est/frmotvar.i "new shared"}

{cec/tt-eb-set-part.i "new"}

IF v-cecscrn-dec THEN
DO:
   DEF TEMP-TABLE tt-64-dec NO-UNDO
       FIELD DEC AS DEC DECIMALS 6.

   DO v-count = 0 TO 63:
       CREATE tt-64-dec.
       tt-64-dec.DEC = v-count / 64.0.
       RELEASE tt-64-dec.
   END.
END.

DEF VAR viEQtyPrev AS INT NO-UNDO.
DEFINE VARIABLE lCheckPurMan AS LOGICAL NO-UNDO .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target,Navigation-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Corr
&Scoped-define BROWSE-NAME br-estitm

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES est est-qty
&Scoped-define FIRST-EXTERNAL-TABLE est


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR est, est-qty.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ef eb

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-estitm                                     */
&Scoped-define FIELDS-IN-QUERY-br-estitm est.est-no eb.cust-no eb.part-no ~
eb.ship-id eb.part-dscr1 eb.stock-no display-combo-qty () @ est-qty.eqty ~
eb.style display-combo-qty () @ est-qty.eqty est-qty.eqty eb.flute eb.test ~
eb.tab-in ef.board ef.cal eb.procat display-cw-dim(yes,eb.len) @ eb.len ~
eb.len display-cw-dim(yes,eb.len) @ eb.len ~
display-cw-dim(yes,eb.wid) @ eb.wid eb.wid ~
display-cw-dim(yes,eb.wid) @ eb.wid display-cw-dim(yes,eb.dep) @ eb.dep ~
eb.dep display-cw-dim(yes,eb.dep) @ eb.dep eb.form-no eb.blank-no eb.i-col ~
eb.i-pass eb.i-coat eb.i-coat-p eb.yld-qty eb.quantityPerSet ef.f-col ~
ef.f-pass ef.f-coat ef.f-coat-p eb.pur-man est.est-date ~
display-set() @ eb.spare-char-2 eb.spare-char-2 display-tab() @ eb.tab-in ~
eb.spare-char-1 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-estitm est.est-no eb.cust-no ~
eb.part-no eb.ship-id eb.part-dscr1 eb.stock-no eb.style est-qty.eqty ~
eb.flute eb.test eb.tab-in ef.board ef.cal eb.procat eb.len eb.wid eb.dep ~
eb.i-col eb.i-pass eb.i-coat eb.i-coat-p eb.yld-qty eb.quantityPerSet ~
eb.pur-man est.est-date eb.spare-char-2 eb.spare-char-1 
&Scoped-define ENABLED-TABLES-IN-QUERY-br-estitm est eb est-qty ef
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-estitm est
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-br-estitm eb
&Scoped-define THIRD-ENABLED-TABLE-IN-QUERY-br-estitm est-qty
&Scoped-define FOURTH-ENABLED-TABLE-IN-QUERY-br-estitm ef
&Scoped-define QUERY-STRING-br-estitm FOR EACH ef WHERE ef.company = est-qty.company ~
  AND ef.est-no = est-qty.est-no ~
  AND ef.eqty = est-qty.eqty NO-LOCK, ~
      EACH eb WHERE eb.company = ef.company ~
  AND eb.est-no = ef.est-no ~
  AND eb.form-no = ef.form-no NO-LOCK ~
    BY eb.form-no ~
       BY eb.blank-no INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-estitm OPEN QUERY br-estitm FOR EACH ef WHERE ef.company = est-qty.company ~
  AND ef.est-no = est-qty.est-no ~
  AND ef.eqty = est-qty.eqty NO-LOCK, ~
      EACH eb WHERE eb.company = ef.company ~
  AND eb.est-no = ef.est-no ~
  AND eb.form-no = ef.form-no NO-LOCK ~
    BY eb.form-no ~
       BY eb.blank-no INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-estitm ef eb
&Scoped-define FIRST-TABLE-IN-QUERY-br-estitm ef
&Scoped-define SECOND-TABLE-IN-QUERY-br-estitm eb


/* Definitions for FRAME Corr                                           */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-estitm 

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
</FOREIGN-KEYS
><EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-combo-qty B-table-Win 
FUNCTION display-combo-qty RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-cw-dim B-table-Win 
FUNCTION display-cw-dim RETURNS DECIMAL
  ( INPUT ip-is-corr-style AS LOG, INPUT  ip-dim AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-set B-table-Win 
FUNCTION display-set RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-tab B-table-Win 
FUNCTION display-tab RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-estitm FOR 
      ef, 
      eb SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-estitm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-estitm B-table-Win _STRUCTURED
  QUERY br-estitm NO-LOCK DISPLAY
      est.est-no FORMAT "x(8)":U WIDTH 12 COLUMN-FONT 2
      eb.cust-no FORMAT "x(8)":U COLUMN-FONT 2
      eb.part-no FORMAT "x(15)":U COLUMN-FONT 2
      eb.ship-id COLUMN-LABEL "Ship To" FORMAT "x(8)":U WIDTH 12
            COLUMN-FONT 2
      eb.part-dscr1 COLUMN-LABEL "Item Name" FORMAT "x(30)":U COLUMN-FONT 2
      eb.stock-no COLUMN-LABEL "FG Item#" FORMAT "x(15)":U WIDTH 21
            COLUMN-FONT 2
      display-combo-qty () @ est-qty.eqty WIDTH 10.2
      eb.style COLUMN-LABEL "Style" FORMAT "x(6)":U WIDTH 9 COLUMN-FONT 2
      display-combo-qty () @ est-qty.eqty
      est-qty.eqty COLUMN-LABEL "Est Qty" FORMAT ">>>>>>>9":U
      eb.flute FORMAT "XXX":U COLUMN-FONT 2
      eb.test FORMAT "x(6)":U COLUMN-FONT 2
      eb.tab-in FORMAT "In/Out":U
      ef.board FORMAT "x(12)":U COLUMN-FONT 2
      ef.cal FORMAT ">9.99999<":U COLUMN-FONT 2
      eb.procat FORMAT "x(5)":U COLUMN-FONT 2
      display-cw-dim(yes,eb.len) @ eb.len
      eb.len FORMAT ">>9.99":U COLUMN-FONT 2
      display-cw-dim(yes,eb.len) @ eb.len
      display-cw-dim(yes,eb.wid) @ eb.wid
      eb.wid FORMAT ">>9.99":U COLUMN-FONT 2
      display-cw-dim(yes,eb.wid) @ eb.wid
      display-cw-dim(yes,eb.dep) @ eb.dep
      eb.dep FORMAT ">>9.99":U COLUMN-FONT 2
      display-cw-dim(yes,eb.dep) @ eb.dep
      eb.form-no FORMAT ">9":U
      eb.blank-no FORMAT ">9":U
      eb.i-col FORMAT ">9":U
      eb.i-pass FORMAT ">9":U
      eb.i-coat FORMAT ">9":U
      eb.i-coat-p COLUMN-LABEL "Coat Passes" FORMAT ">9":U WIDTH 12
      eb.yld-qty FORMAT "->>>>>>>9":U
      eb.quantityPerSet FORMAT ">>>>9.9<<<<":U
      ef.f-col COLUMN-LABEL "Inks/Form" FORMAT ">>":U
      ef.f-pass COLUMN-LABEL "Passes/Form" FORMAT ">>":U
      ef.f-coat COLUMN-LABEL "Coatings/Form" FORMAT ">>":U
      ef.f-coat-p COLUMN-LABEL "Coat Passes/Form" FORMAT ">>":U
      eb.pur-man COLUMN-LABEL "Purch/Manuf" FORMAT "P/M":U
      est.est-date FORMAT "99/99/9999":U COLUMN-FONT 2
      display-set() @ eb.spare-char-2
      eb.spare-char-2 COLUMN-LABEL "Set?" FORMAT "!":U
      display-tab() @ eb.tab-in
      eb.spare-char-1 COLUMN-LABEL "Designer Name" FORMAT "x(25)":U
  ENABLE
      est.est-no
      eb.cust-no
      eb.part-no
      eb.ship-id
      eb.part-dscr1
      eb.stock-no
      eb.style
      est-qty.eqty
      eb.flute
      eb.test
      eb.tab-in
      ef.board
      ef.cal
      eb.procat
      eb.len
      eb.wid
      eb.dep
      eb.i-col
      eb.i-pass
      eb.i-coat
      eb.i-coat-p
      eb.yld-qty
      eb.quantityPerSet HELP "Enter Quantity Per Set for this component"
      eb.pur-man
      est.est-date
      eb.spare-char-2
      eb.spare-char-1
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 148 BY 16.43
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Corr
     br-estitm AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   External Tables: ASI.est,ASI.est-qty
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
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
         HEIGHT             = 16.48
         WIDTH              = 148.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/navbrows.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME Corr
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br-estitm 1 Corr */
ASSIGN 
       FRAME Corr:SCROLLABLE       = FALSE
       FRAME Corr:HIDDEN           = TRUE.

ASSIGN 
       br-estitm:NUM-LOCKED-COLUMNS IN FRAME Corr     = 3.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-estitm
/* Query rebuild information for BROWSE br-estitm
     _TblList          = "ASI.ef WHERE ASI.est-qty ...,ASI.eb WHERE ASI.ef ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION KEY-PHRASE"
     _TblOptList       = ","
     _OrdList          = "ASI.eb.form-no|yes,ASI.eb.blank-no|yes"
     _JoinCode[1]      = "ASI.ef.company = ASI.est-qty.company
  AND ASI.ef.est-no = ASI.est-qty.est-no
  AND ASI.ef.eqty = ASI.est-qty.eqty"
     _JoinCode[2]      = "ASI.eb.company = ASI.ef.company
  AND ASI.eb.est-no = ASI.ef.est-no
  AND ASI.eb.form-no = ASI.ef.form-no"
     _FldNameList[1]   > ASI.est.est-no
"est.est-no" ? "x(8)" "character" ? ? 2 ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.eb.cust-no
"eb.cust-no" ? ? "character" ? ? 2 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.eb.part-no
"eb.part-no" ? ? "character" ? ? 2 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.eb.ship-id
"eb.ship-id" "Ship To" ? "character" ? ? 2 ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.eb.part-dscr1
"eb.part-dscr1" "Item Name" ? "character" ? ? 2 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.eb.stock-no
"eb.stock-no" "FG Item#" ? "character" ? ? 2 ? ? ? yes ? no no "21" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"display-combo-qty () @ est-qty.eqty" ? ? ? ? ? ? ? ? ? no ? no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.eb.style
"eb.style" "Style" ? "character" ? ? 2 ? ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"display-combo-qty () @ est-qty.eqty" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.est-qty.eqty
"est-qty.eqty" "Est Qty" ">>>>>>>9" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.eb.flute
"eb.flute" ? ? "character" ? ? 2 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.eb.test
"eb.test" ? ? "character" ? ? 2 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.eb.tab-in
"eb.tab-in" ? ? "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.ef.board
"ef.board" ? "x(12)" "character" ? ? 2 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.ef.cal
"ef.cal" ? ">9.99999<" "decimal" ? ? 2 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.eb.procat
"eb.procat" ? ? "character" ? ? 2 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"display-cw-dim(yes,eb.len) @ eb.len" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ASI.eb.len
"eb.len" ? ">>9.99" "decimal" ? ? 2 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"display-cw-dim(yes,eb.len) @ eb.len" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"display-cw-dim(yes,eb.wid) @ eb.wid" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > ASI.eb.wid
"eb.wid" ? ">>9.99" "decimal" ? ? 2 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > "_<CALC>"
"display-cw-dim(yes,eb.wid) @ eb.wid" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > "_<CALC>"
"display-cw-dim(yes,eb.dep) @ eb.dep" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > ASI.eb.dep
"eb.dep" ? ">>9.99" "decimal" ? ? 2 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > "_<CALC>"
"display-cw-dim(yes,eb.dep) @ eb.dep" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   = ASI.eb.form-no
     _FldNameList[27]   = ASI.eb.blank-no
     _FldNameList[28]   > ASI.eb.i-col
"eb.i-col" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[29]   > ASI.eb.i-pass
"eb.i-pass" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[30]   > ASI.eb.i-coat
"eb.i-coat" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[31]   > ASI.eb.i-coat-p
"eb.i-coat-p" "Coat Passes" ? "integer" ? ? ? ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[32]   > ASI.eb.yld-qty
"eb.yld-qty" ? "->>>>>>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[33]   > ASI.eb.quantityPerSet
"eb.quantityPerSet" ? ? "decimal" ? ? ? ? ? ? yes "Enter Quantity Per Set for this component" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[34]   > ASI.ef.f-col
"ef.f-col" "Inks/Form" ">>" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[35]   > ASI.ef.f-pass
"ef.f-pass" "Passes/Form" ">>" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[36]   > ASI.ef.f-coat
"ef.f-coat" "Coatings/Form" ">>" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[37]   > ASI.ef.f-coat-p
"ef.f-coat-p" "Coat Passes/Form" ">>" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[38]   > ASI.eb.pur-man
"eb.pur-man" "Purch/Manuf" ? "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[39]   > ASI.est.est-date
"est.est-date" ? ? "date" ? ? 2 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[40]   > "_<CALC>"
"display-set() @ eb.spare-char-2" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[41]   > ASI.eb.spare-char-2
"eb.spare-char-2" "Set?" "!" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[42]   > "_<CALC>"
"display-tab() @ eb.tab-in" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[43]   > ASI.eb.spare-char-1
"eb.spare-char-1" "Designer Name" "x(25)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE br-estitm */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME Corr
/* Query rebuild information for FRAME Corr
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME Corr */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-estitm
&Scoped-define SELF-NAME br-estitm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-estitm B-table-Win
ON DEFAULT-ACTION OF br-estitm IN FRAME Corr
DO:
   DEF VAR phandle AS WIDGET-HANDLE NO-UNDO.
   DEF VAR char-hdl AS cha NO-UNDO.   
   RUN get-link-handle IN adm-broker-hdl
      (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
   phandle = WIDGET-HANDLE(char-hdl).
   
   RUN new-state IN phandle ('update-begin':U).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-estitm B-table-Win
ON HELP OF br-estitm IN FRAME Corr
DO:
     DEF VAR ls-cur-val AS cha NO-UNDO.
     DEF VAR lv-eb-tmpid AS RECID NO-UNDO.
     DEF VAR lv-handle AS HANDLE NO-UNDO.          
     DEF VAR char-val2 AS cha NO-UNDO.        
     DEF VAR date-val AS cha NO-UNDO.
     DEF VAR date-val2 AS cha NO-UNDO.
     DEF VAR lv-rowid AS ROWID NO-UNDO.

     DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.
     DEF VAR look-recid AS RECID NO-UNDO.

     lw-focus = FOCUS.

     CASE lw-focus:NAME :
     /*when "part-no" then do: 
           run est/l-ebrfqP.w (gcompany, gloc, lw-focus:screen-value, output lv-eb-tmpid) .
           if lv-eb-tmpid = ?  then return no-apply.
           find xeb where recid(xeb) = lv-eb-tmpid no-lock no-error.
           find xef of xeb where xef.company = xeb.company and
                                 xef.est-no = xeb.est-no  
                no-lock no-error.
           run copy-from-est.
           /*run copy-from-est2.*/
           lv-part-no-prev = eb.part-no.          
           return no-apply.           
      end.
      when "stock-no" then do:
        /* run windows/l-itemfg.w  (gcompany, output char-val). */
           run est/l-ebrfq.w (gcompany, gloc,lw-focus:screen-value, output lv-eb-tmpid) .
           if lv-eb-tmpid = ?  then return.
           find xeb where recid(xeb) = lv-eb-tmpid no-lock no-error.
           find xef of xeb where xef.company = xeb.company and
                                 xef.est-no = xeb.est-no
                          no-lock no-error.
   
           run copy-from-est.
           /*run copy-from-est2. */
  
           return no-apply.
      end.*/
      WHEN "part-no" THEN DO:
         RUN blank-cp (YES).
         RETURN NO-APPLY.
      END.
      WHEN "part-dscr1" THEN DO:
         RUN blank-cp (YES).
         RETURN NO-APPLY.
      END.
      WHEN "stock-no" THEN DO:
         RUN blank-cp (YES).
         RETURN NO-APPLY.
      END.
      WHEN "style" THEN DO:
           ls-cur-val = lw-focus:SCREEN-VALUE.
           RUN windows/l-stylec.w (gcompany,ls-cur-val, OUTPUT char-val).
           IF char-val <> "" AND ls-cur-val <> entry(1,char-val) THEN DO:
              eb.style:screen-value IN BROWSE {&browse-name} = entry(1,char-val).
              FIND style WHERE style.company = gcompany AND
                               style.style = eb.style:screen-value IN BROWSE {&browse-name}
                         NO-LOCK NO-ERROR.            
              IF AVAIL style AND ef.board:SCREEN-VALUE EQ "" THEN DO:
                ef.board:SCREEN-VALUE IN BROWSE {&browse-name} = style.material[1].
                RUN new-board.
              END.          
           END.  
           RETURN NO-APPLY.
      END.
      WHEN "procat" THEN DO:
           ls-cur-val = lw-focus:SCREEN-VALUE.
           RUN windows/l-fgcat.w (gcompany,ls-cur-val,OUTPUT char-val).
           IF char-val <> "" THEN
              lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
           RETURN NO-APPLY.

       END.
       WHEN "flute" THEN DO:
           ls-cur-val = lw-focus:SCREEN-VALUE.
           RUN windows/l-flute.w (gcompany,OUTPUT char-val).
           IF char-val <> "" THEN
              lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
           RETURN NO-APPLY.
       END.
       WHEN "test" THEN DO:
           ls-cur-val = eb.flute:screen-value.
           RUN windows/l-test.w (gcompany,gloc,ls-cur-val,OUTPUT char-val).
           IF char-val <> "" THEN
              lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
           RETURN NO-APPLY.       
       END.
       WHEN "Board" THEN DO:
           DEF VAR lv-ind LIKE style.industry NO-UNDO.
           FIND style WHERE style.company = gcompany AND
                            style.style = eb.style:screen-value IN BROWSE {&browse-name}
                            NO-LOCK NO-ERROR.   
           IF AVAIL style THEN lv-ind = style.industry.
           ELSE lv-ind = "".  
           IF AVAIL style AND style.type = "f" THEN  DO: /* foam */
              RUN windows/l-boardf.w (gcompany,lv-ind,lw-focus:SCREEN-VALUE,OUTPUT char-val).
              IF char-val NE "" AND ENTRY(1,char-val) NE lw-focus:SCREEN-VALUE THEN DO:
                ef.board:SCREEN-VALUE IN BROWSE {&browse-name} = ENTRY(1,char-val).
                RUN new-board.
              END.
           END.
           ELSE DO:
              RUN windows/l-board1.w (eb.company,lv-ind,lw-focus:SCREEN-VALUE, OUTPUT lv-rowid).
              FIND FIRST ITEM WHERE ROWID(item) EQ lv-rowid NO-LOCK NO-ERROR.
              IF AVAIL ITEM AND ITEM.i-no NE lw-focus:SCREEN-VALUE THEN DO:
                ef.board:SCREEN-VALUE IN BROWSE {&browse-name} = item.i-no.
                RUN new-board.
              END.
           END.
           RETURN NO-APPLY.   
       END.              
       WHEN "cust-no" THEN DO:
           ls-cur-val = lw-focus:SCREEN-VALUE.
           /*RUN windows/l-custact.w (gcompany,ls-cur-val, OUTPUT char-val, OUTPUT look-recid).*/
           RUN windows/l-cust2.w (INPUT g_company, ls-cur-val,"", OUTPUT char-val).
           IF char-val NE "" AND ls-cur-val NE ENTRY(1,char-val) THEN DO:
              eb.cust-no:SCREEN-VALUE = ENTRY(1,char-val).
              APPLY "value-changed" TO eb.cust-no.
           END.
           RETURN NO-APPLY.
       END.  /* cust-no*/
       WHEN "ship-id" THEN DO:
           ls-cur-val = lw-focus:SCREEN-VALUE.
           RUN windows/l-shipto.w (gcompany,gloc,eb.cust-no:SCREEN-VALUE,ls-cur-val, OUTPUT char-val).
           IF char-val NE "" AND ls-cur-val NE ENTRY(1,char-val) THEN DO:
              lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
              APPLY "value-changed" TO lw-focus.
           END.
           RETURN NO-APPLY.
       END.  /* cust-no*/
       WHEN "eqty" THEN DO:
           IF est.est-type LE 6 THEN DO:
             lv-estqty-recid = IF AVAIL est-qty THEN RECID(est-qty) ELSE ?.
             RUN est/estqtyd.w (lv-estqty-recid, RECID(eb),est-qty.eqty:screen-value IN BROWSE {&browse-name}, OUTPUT char-val, OUTPUT char-val2, OUTPUT date-val, OUTPUT date-val2) .
             IF char-val <> "?" 
                THEN ASSIGN est-qty.eqty:screen-value = ENTRY(1,char-val)
                            lv-copy-qty[2] = INTEGER(ENTRY(2,char-val))
                            lv-copy-qty[3] = INTEGER(ENTRY(3,char-val))
                            lv-copy-qty[4] = INTEGER(ENTRY(4,char-val))
                            lv-copy-qty[5] = INTEGER(ENTRY(5,char-val))
                            lv-copy-qty[6] = INTEGER(ENTRY(6,char-val))
                            lv-copy-qty[7] = INTEGER(ENTRY(7,char-val))
                            lv-copy-qty[8] = INTEGER(ENTRY(8,char-val))
                            lv-copy-qty[9] = INTEGER(ENTRY(9,char-val))
                            lv-copy-qty[10] = INTEGER(ENTRY(10,char-val))
                         /*   lv-copy-pr[1] = decimal(entry(11,char-val))
                            lv-copy-pr[2] = decimal(entry(12,char-val))
                            lv-copy-pr[3] = decimal(entry(13,char-val))
                            lv-copy-pr[4] = decimal(entry(14,char-val))
                            lv-copy-pr[5] = decimal(entry(15,char-val))
                            lv-copy-pr[6] = decimal(entry(16,char-val))
                            lv-copy-pr[7] = decimal(entry(17,char-val))
                            lv-copy-pr[8] = decimal(entry(18,char-val))
                            lv-copy-pr[9] = decimal(entry(19,char-val))
                            lv-copy-pr[10] = decimal(entry(20,char-val))
                            lv-copy-uom[1] = entry(21,char-val)
                            lv-copy-uom[2] = entry(22,char-val)
                            lv-copy-uom[3] = entry(23,char-val)
                            lv-copy-uom[4] = entry(24,char-val)
                            lv-copy-uom[5] = entry(25,char-val)
                            lv-copy-uom[6] = entry(26,char-val)
                            lv-copy-uom[7] = entry(27,char-val)
                            lv-copy-uom[8] = entry(28,char-val)
                            lv-copy-uom[9] = entry(29,char-val)
                            lv-copy-uom[10] = entry(30,char-val)
                            lv-copy-date[1] = date(entry(1,date-val))
                            lv-copy-date[2] = date(entry(2,date-val))
                            lv-copy-date[3] = date(entry(3,date-val))
                            lv-copy-date[4] = date(entry(4,date-val))
                            lv-copy-date[5] = date(entry(5,date-val))
                            lv-copy-date[6] = date(entry(6,date-val))
                            lv-copy-date[7] = date(entry(7,date-val))
                            lv-copy-date[8] = date(entry(8,date-val))
                            lv-copy-date[9] = date(entry(9,date-val))
                            lv-copy-date[10] = date(entry(1,date-val))
                            */
                            .
             IF char-val2 <> "?" 
                THEN ASSIGN lv-copy-qty[11] = INTEGER(ENTRY(1,char-val2))
                            lv-copy-qty[12] = INTEGER(ENTRY(2,char-val2))
                            lv-copy-qty[13] = INTEGER(ENTRY(3,char-val2))
                            lv-copy-qty[14] = INTEGER(ENTRY(4,char-val2))
                            lv-copy-qty[15] = INTEGER(ENTRY(5,char-val2))
                            lv-copy-qty[16] = INTEGER(ENTRY(6,char-val2))
                            lv-copy-qty[17] = INTEGER(ENTRY(7,char-val2))
                            lv-copy-qty[18] = INTEGER(ENTRY(8,char-val2))
                            lv-copy-qty[19] = INTEGER(ENTRY(9,char-val2))
                            lv-copy-qty[20] = INTEGER(ENTRY(10,char-val2))
                                /*lv-copy-pr[11] = decimal(entry(11,char-val2))
                                lv-copy-pr[12] = decimal(entry(12,char-val2))
                                lv-copy-pr[13] = decimal(entry(13,char-val2))
                                lv-copy-pr[14] = decimal(entry(14,char-val2))
                                lv-copy-pr[15] = decimal(entry(15,char-val2))
                                lv-copy-pr[16] = decimal(entry(16,char-val2))
                                lv-copy-pr[17] = decimal(entry(17,char-val2))
                                lv-copy-pr[18] = decimal(entry(18,char-val2))
                                lv-copy-pr[19] = decimal(entry(19,char-val2))
                                lv-copy-pr[20] = decimal(entry(20,char-val2))
                                lv-copy-uom[11] = entry(21,char-val2)
                                lv-copy-uom[12] = entry(22,char-val2)
                                lv-copy-uom[13] = entry(23,char-val2)
                                lv-copy-uom[14] = entry(24,char-val2)
                                lv-copy-uom[15] = entry(25,char-val2)
                                lv-copy-uom[16] = entry(26,char-val2)
                                lv-copy-uom[17] = entry(27,char-val2)
                                lv-copy-uom[18] = entry(28,char-val2)
                                lv-copy-uom[19] = entry(29,char-val2)
                                lv-copy-uom[20] = entry(30,char-val2)
                                lv-copy-date[11] = date(entry(1,date-val2))
                                lv-copy-date[12] = date(entry(2,date-val2))
                                lv-copy-date[13] = date(entry(3,date-val2))
                                lv-copy-date[14] = date(entry(4,date-val2))
                                lv-copy-date[15] = date(entry(5,date-val2))
                                lv-copy-date[16] = date(entry(6,date-val2))
                                lv-copy-date[17] = date(entry(7,date-val2))
                                lv-copy-date[18] = date(entry(8,date-val2))
                                lv-copy-date[19] = date(entry(9,date-val2))
                                lv-copy-date[20] = date(entry(1,date-val2))
                                */.
           END.
           RETURN NO-APPLY.
       END.
       OTHERWISE DO:
        /* ==========================  
           lv-handle = lw-focus:handle.
           run applhelp.p.
             
           if g_lookup-var <> "" then do:
              lv-handle:screen-value = g_lookup-var.
              if lv-handle:name = "cust-no" then do:
                 find cust where cust.company = gcompany and
                              cust.cust-no = lv-handle:screen-value 
                              no-lock no-error.
                 if avail cust then do:
                    find first shipto where shipto.company = gcompany
                                        and shipto.cust-no = cust.cust-no
                                        no-lock no-error.
                    eb.ship-id:screen-value = if avail shipto then shipto.ship-id else "".
                 end.    
              end.  /* cust-no */
           end.   /* g_lookup-var <> "" */
           g_lookup-var = "".
           =======================  */
           RETURN NO-APPLY.
        END.  /* otherwise */
  END CASE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-estitm B-table-Win
ON RETURN OF br-estitm IN FRAME Corr
ANYWHERE
DO:
   APPLY "tab" TO SELF.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-estitm B-table-Win
ON ROW-DISPLAY OF br-estitm IN FRAME Corr
DO:
    DEF VAR lActive AS LOG NO-UNDO.
   IF v-cefgitem-log AND AVAIL eb THEN
   DO:
/*                                                        */
/*       FIND FIRST reftable WHERE                        */
/*            reftable.reftable EQ "FGSTATUS" AND         */
/*            reftable.company  EQ cocode AND             */
/*            reftable.loc      EQ "" AND                 */
/*            reftable.code     EQ eb.stock-no            */
/*            NO-LOCK NO-ERROR.                           */
/*                                                        */
/*       IF AVAIL reftable AND reftable.code2 EQ "I" THEN */
       RUN fg/GetItemfgActInact.p (INPUT cocode,
                                   INPUT eb.stock-no,
                                   OUTPUT lActive).
        IF NOT lActive THEN 
            eb.stock-no:BGCOLOR IN BROWSE {&browse-name} = 11.
        ELSE
            eb.stock-no:BGCOLOR IN BROWSE {&browse-name} = ?.

      RELEASE reftable.
   END.

  IF v-cecscrn-dec AND AVAIL eb THEN
     ASSIGN
        eb.wid:FORMAT IN BROWSE {&browse-name} = ">>9.999999"
        eb.len:FORMAT IN BROWSE {&browse-name} = ">>9.999999"
        eb.dep:FORMAT IN BROWSE {&browse-name} = ">>9.999999".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-estitm B-table-Win
ON ROW-ENTRY OF br-estitm IN FRAME Corr
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  

  /* not to have error no eb record avail when add new set item */
  IF NOT AVAIL eb THEN FIND eb WHERE RECID(eb) = lv-eb-recid NO-LOCK NO-ERROR.
  
  RUN set-hold-values.

  ASSIGN
   ll-part-no        = NO
   lv-copied         = ?
   ll-new-shipto     = NO
   ll-new-est        = NO
   ll-wid-len-warned = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-estitm B-table-Win
ON ROW-LEAVE OF br-estitm IN FRAME Corr
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
    /*{src/adm/template/brsleave.i}    */
  
  
   IF KEYFUNCTION(LASTKEY) = "page-up" OR 
      keyfunction(LASTKEY) = "page-down" OR
      keyfunction(LASTKEY) = "cursor-up" OR
      keyfunction(LASTKEY) = "cursor-down" 
   THEN DO:
  
      RETURN NO-APPLY.
   END.
 
     {est/brsleave.i}   /* same but update will be like add 
                           need to run set-attribute-list ("adm-new-record = 'no' ")
                           in local-update-record  */
                           
  
                           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-estitm B-table-Win
ON VALUE-CHANGED OF br-estitm IN FRAME Corr
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
     
  IF NOT adm-new-record AND AVAIL eb /*and not adm-adding-record */ THEN   
     ASSIGN lv-eb-recid = RECID(eb)
            lv-ef-recid = RECID(ef). 

  {methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header"
     "(est.rec_key,'ESTIMATE:' + eb.rec_key + ' ' + {methods/headers/est.i})"}


  RUN setFarmTab.

  RUN custom-row-changed.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est.est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est.est-no br-estitm _BROWSE-COLUMN B-table-Win
ON ENTRY OF est.est-no IN BROWSE br-estitm /* Estimate # */
DO:
  IF NOT ll-new-est THEN DO:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est.est-no br-estitm _BROWSE-COLUMN B-table-Win
ON LEAVE OF est.est-no IN BROWSE br-estitm /* Estimate # */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-est-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.cust-no br-estitm _BROWSE-COLUMN B-table-Win
ON BACK-TAB OF eb.cust-no IN BROWSE br-estitm /* Cust. # */
DO:
  ll-new-est = YES.
  APPLY "entry" TO est.est-no IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.cust-no br-estitm _BROWSE-COLUMN B-table-Win
ON ENTRY OF eb.cust-no IN BROWSE br-estitm /* Cust. # */
DO:
  DEF BUFFER b-eb FOR eb.


  IF {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN DO:
    FIND FIRST b-eb
        WHERE b-eb.company EQ eb.company
          AND b-eb.est-no  EQ eb.est-no
          AND b-eb.form-no EQ eb.form-no
          AND b-eb.cust-no NE ""
          AND ROWID(b-eb)  NE ROWID(eb)
        NO-LOCK NO-ERROR.
    IF NOT AVAIL b-eb THEN
    FIND FIRST b-eb
        WHERE b-eb.company EQ eb.company
          AND b-eb.est-no  EQ eb.est-no
          AND b-eb.cust-no NE ""
          AND ROWID(b-eb)  NE ROWID(eb)
        NO-LOCK NO-ERROR.
    IF AVAIL b-eb THEN DO:
      {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name} = b-eb.cust-no.

      APPLY "leave" TO {&self-name} IN BROWSE {&browse-name}.
      RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.cust-no br-estitm _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.cust-no IN BROWSE br-estitm /* Cust. # */
DO:
DO WITH FRAME {&FRAME-NAME}:
  IF eb.cust-no:MODIFIED IN BROWSE {&browse-name} THEN DO:

    IF LASTKEY <> -1 AND eb.ord-no NE 0 AND
       eb.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} NE eb.cust-no AND
       eb.cust-no NE "" THEN
    DO:
      MESSAGE "Cannot Change Customer."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN NO-APPLY.
    END.

    FIND cust
        WHERE cust.company EQ gcompany
          AND cust.cust-no BEGINS eb.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.

    IF AVAIL cust THEN DO:
      eb.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} = cust.cust-no.

      FIND FIRST shipto
          WHERE shipto.company EQ cust.company
            AND shipto.cust-no EQ cust.cust-no
            AND shipto.ship-id EQ cust.cust-no
          NO-LOCK NO-ERROR.

      IF NOT AVAIL shipto THEN
      FIND FIRST shipto
          WHERE shipto.company EQ cust.company
            AND shipto.cust-no EQ cust.cust-no
            AND shipto.ship-no EQ 1
          NO-LOCK NO-ERROR.

      IF AVAIL shipto THEN eb.ship-id:SCREEN-VALUE IN BROWSE {&browse-name} = shipto.ship-id.
    END.
  END.

   IF LASTKEY <> -1 AND
      KEYFUNCTION(LASTKEY) NE "BACK-TAB" AND /*eb.cust-no:screen-value in browse {&browse-name} <> "" and */
      NOT CAN-FIND(cust WHERE cust.company = gcompany AND cust.cust-no = eb.cust-no:screen-value IN BROWSE {&browse-name} )
   THEN DO:
       IF eb.cust-no:screen-value = "" THEN DO:
           MESSAGE "Invalid Customer Number. Try Help." VIEW-AS ALERT-BOX ERROR. 
           RETURN NO-APPLY.
       END.
       MESSAGE "Customer " eb.cust-no:screen-value "does not exist. Do you want to add it?"
               VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
       IF NOT ll-ans THEN  RETURN NO-APPLY.
       
       RUN est/custfly.w (eb.cust-no:screen-value). 
       IF NOT CAN-FIND(cust WHERE cust.company = gcompany AND cust.cust-no = eb.cust-no:screen-value IN BROWSE {&browse-name} )
       THEN RETURN NO-APPLY.
       END.

        IF LASTKEY <> -1 THEN DO:
            RUN valid-cust-user NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.part-no br-estitm _BROWSE-COLUMN B-table-Win
ON ENTRY OF eb.part-no IN BROWSE br-estitm /* Cust Part # */
DO:
  IF ll-add-set THEN DO:
     ll-add-set = NO.
     RUN est/crt-set.w (ROWID(est)) NO-ERROR.
     FIND FIRST tt-eb-set NO-LOCK NO-ERROR.
     IF AVAIL tt-eb-set THEN
       ASSIGN
        li-flen[1] = LENGTH(STRING(FILL("X",100),eb.part-no:FORMAT IN BROWSE {&browse-name}))  - 2
        li-flen[2] = LENGTH(STRING(FILL("X",100),eb.stock-no:FORMAT IN BROWSE {&browse-name})) - 2
        eb.part-no:SCREEN-VALUE IN BROWSE {&browse-name}    =
             TRIM(SUBSTR(tt-eb-set.part-no,1,li-flen[1])) + "-1"
        eb.part-dscr1:SCREEN-VALUE IN BROWSE {&browse-name} = tt-eb-set.part-dscr1
        eb.stock-no:SCREEN-VALUE IN BROWSE {&browse-name}   =
             IF tt-eb-set.stock-no NE "" THEN (TRIM(SUBSTR(tt-eb-set.stock-no,1,li-flen[2])) + "-1") ELSE ""
        eb.procat:SCREEN-VALUE = tt-eb-set.procat.
     ELSE RUN dispatch ("cancel-record").
  END.
  ELSE IF ll-add-set-part THEN
  DO:
     RUN est/crt-set-part.w (ROWID(est),lv-last-est-cat) NO-ERROR.
     FIND FIRST tt-eb-set-part NO-ERROR.
     IF AVAIL tt-eb-set-part THEN
     DO:
        ASSIGN
         li-flen[1] = LENGTH(STRING(FILL("X",100),eb.part-no:FORMAT IN BROWSE {&browse-name}))  - 2
         li-flen[2] = LENGTH(STRING(FILL("X",100),eb.stock-no:FORMAT IN BROWSE {&browse-name})) - 2
         eb.part-no:SCREEN-VALUE IN BROWSE {&browse-name}    =
              TRIM(SUBSTR(tt-eb-set-part.part-no,1,li-flen[1])) + "-1"
         eb.part-dscr1:SCREEN-VALUE IN BROWSE {&browse-name} = tt-eb-set-part.part-dscr1
         eb.stock-no:SCREEN-VALUE IN BROWSE {&browse-name}   =
              IF tt-eb-set-part.stock-no NE "" THEN (TRIM(SUBSTR(tt-eb-set-part.stock-no,1,li-flen[2])) + "-1")
              ELSE ""
         eb.procat:SCREEN-VALUE = tt-eb-set-part.procat
         eb.style:SCREEN-VALUE = tt-eb-set-part.style-1
         eb.quantityPerSet:SCREEN-VALUE = STRING(tt-eb-set-part.qty-set-1)
         est-qty.eqty:SCREEN-VALUE = STRING(tt-eb-set-part.est-qty)
         eb.len:SCREEN-VALUE = STRING(tt-eb-set-part.len)
         eb.wid:SCREEN-VALUE = STRING(tt-eb-set-part.dep)
         ef.board:SCREEN-VALUE = STRING(tt-eb-set-part.board)
         v-qty = tt-eb-set-part.est-qty
         v-style2 = tt-eb-set-part.style-2
         v-board2 = tt-eb-set-part.board
         v-no-forms = tt-eb-set-part.num-forms
         v-in-cell-w = tt-eb-set-part.in-cell-width
         v-end-cell-w1 = tt-eb-set-part.end-cell-width-1
         v-end-cell-w2 = tt-eb-set-part.end-cell-width-2
         v-qty-set-2 = tt-eb-set-part.qty-set-2
         v-wid-2 = tt-eb-set-part.wid
         v-height-2 = tt-eb-set-part.dep
         v-part-desc3 = tt-eb-set-part.part-dscr3
         v-part-desc4 = tt-eb-set-part.part-dscr4
         v-part-no2   = TRIM(SUBSTR(tt-eb-set-part.part-no,1,li-flen[1])) + "-2"
         v-stock-no2  = IF tt-eb-set-part.stock-no NE "" THEN (TRIM(SUBSTR(tt-eb-set-part.stock-no,1,li-flen[2])) + "-2")
                        ELSE ""
         v-assem-grain = tt-eb-set-part.grain.
        RUN set-lv-foam.
        RUN new-board.
        RUN dispatch ("update-record").
        ASSIGN
           ll-add-set-part-2 = YES.
           ll-is-add-from-tool = YES.

        IF v-no-forms EQ 1 THEN
           ls-add-what = "blank".
        ELSE
           ls-add-what = "form".
        
        RUN dispatch ("add-record").
     END.
     ELSE
        RUN dispatch ("cancel-record").
  END.
          RUN set-lv-foam.
  ll-part-no = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.part-no br-estitm _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.part-no IN BROWSE br-estitm /* Cust Part # */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF eb.part-dscr1:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" AND
       eb.stock-no:SCREEN-VALUE IN BROWSE {&browse-name}   EQ "" THEN
      RUN blank-cp (NO).

    IF NOT v-fg-copy                                          OR
       eb.part-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
      RUN valid-part-no NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
   RUN set-lv-foam.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.part-no br-estitm _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF eb.part-no IN BROWSE br-estitm /* Cust Part # */
DO:
  lv-copied = ?.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.ship-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.ship-id br-estitm _BROWSE-COLUMN B-table-Win
ON ENTRY OF eb.ship-id IN BROWSE br-estitm /* Ship To */
DO:
  DEF BUFFER b-eb FOR eb.

  IF {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN DO:
    FIND FIRST b-eb
        WHERE b-eb.company EQ eb.company
          AND b-eb.est-no  EQ eb.est-no
          AND b-eb.form-no EQ eb.form-no
          AND b-eb.cust-no EQ eb.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND b-eb.ship-id NE ""
          AND ROWID(b-eb)  NE ROWID(eb)
        NO-LOCK NO-ERROR.
    IF NOT AVAIL b-eb THEN
    FIND FIRST b-eb
        WHERE b-eb.company EQ eb.company
          AND b-eb.est-no  EQ eb.est-no
          AND b-eb.cust-no EQ eb.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND b-eb.ship-id NE ""
          AND ROWID(b-eb)  NE ROWID(eb)
        NO-LOCK NO-ERROR.
    IF AVAIL b-eb THEN DO:
      {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name} = b-eb.ship-id.

      APPLY "leave" TO {&self-name} IN BROWSE {&browse-name}.
      RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.ship-id br-estitm _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.ship-id IN BROWSE br-estitm /* Ship To */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF SELF:MODIFIED OR adm-adding-record THEN RUN new-ship-id.

    RUN valid-ship-id NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.ship-id br-estitm _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF eb.ship-id IN BROWSE br-estitm /* Ship To */
DO:
  ll-new-shipto = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.part-dscr1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.part-dscr1 br-estitm _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.part-dscr1 IN BROWSE br-estitm /* Item Name */
DO:
  IF LASTKEY NE -1                                           AND
     eb.part-no:SCREEN-VALUE IN BROWSE {&browse-name}  EQ "" AND
     eb.stock-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN
    RUN blank-cp (NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.part-dscr1 br-estitm _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF eb.part-dscr1 IN BROWSE br-estitm /* Item Name */
DO:
  lv-copied = ?.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.stock-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.stock-no br-estitm _BROWSE-COLUMN B-table-Win
ON ENTRY OF eb.stock-no IN BROWSE br-estitm /* FG Item# */
DO:
  DEF VAR ll AS LOG NO-UNDO.
DEF VAR lv-i-no AS CHAR NO-UNDO.

  ls-prev-val = SELF:SCREEN-VALUE.

  IF NOT adm-new-record THEN DO:
    FOR EACH job-hdr NO-LOCK
        WHERE job-hdr.company EQ cocode
          AND job-hdr.est-no  EQ eb.est-no
        USE-INDEX est-no:

       ll = job-hdr.i-no EQ {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name} OR
           CAN-FIND(FIRST reftable
                    WHERE reftable.reftable EQ "jc/jc-calc.p"
                      AND reftable.company  EQ job-hdr.company
                      AND reftable.loc      EQ ""
                      AND reftable.code     EQ STRING(job-hdr.job,"999999999")
                      AND reftable.code2    EQ {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}).
      
      IF ll THEN LEAVE.
    END.

    IF NOT ll THEN
      ll = CAN-FIND(FIRST oe-ordl
                    WHERE oe-ordl.company EQ cocode
                      AND oe-ordl.est-no  EQ eb.est-no
                      AND oe-ordl.i-no    EQ {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}
                    USE-INDEX est).
  END.

  IF ll THEN DO:
    APPLY "tab" TO SELF.
    RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.stock-no br-estitm _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.stock-no IN BROWSE br-estitm /* FG Item# */
DO:
  DEF VAR ll-copy-fg AS LOG NO-UNDO.
  DEF VAR lActive AS LOG NO-UNDO.

  IF LASTKEY NE -1 THEN DO:
    /*IF eb.stock-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
       RUN fg/GetItemfgActInact.p (INPUT g_company,
                                   INPUT eb.stock-no:SCREEN-VALUE,
                                   OUTPUT lActive).
       IF NOT lActive THEN DO:
           MESSAGE eb.stock-no:SCREEN-VALUE + " has InActive Status. Order cannot be placed for the Inactive Item."
                   VIEW-AS ALERT-BOX ERROR.
           RETURN NO-APPLY.
        END.        
    END.*/  /* Ticket 22498 */
   
    ll-copy-fg = CAN-FIND(FIRST itemfg
                          WHERE itemfg.company EQ g_company
                            AND itemfg.i-no    EQ SELF:SCREEN-VALUE) AND
                 SELF:SCREEN-VALUE NE ls-prev-val.

    IF (eb.part-no:SCREEN-VALUE IN BROWSE {&browse-name}    EQ "" AND
        eb.part-dscr1:SCREEN-VALUE IN BROWSE {&browse-name} EQ "") OR
       ll-copy-fg                                                 THEN
      RUN blank-cp (NO).

    IF v-fg-copy OR ll-copy-fg THEN DO:
      RUN valid-part-no NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.

    RUN valid-stock-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
  RUN set-lv-foam.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.stock-no br-estitm _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF eb.stock-no IN BROWSE br-estitm /* FG Item# */
DO:
  lv-copied = ?.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.style
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.style br-estitm _BROWSE-COLUMN B-table-Win
ON ENTRY OF eb.style IN BROWSE br-estitm /* Style */
DO:
    ls-prev-val = SELF:screen-value.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.style br-estitm _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.style IN BROWSE br-estitm /* Style */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-style NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY. 

    IF is-item-copied-from-est THEN RETURN.

    IF /*eb.style:modified in browse {&browse-name}*/ 
       SELF:screen-value <> ls-prev-val THEN DO:

      RUN set-lv-foam.
      IF AVAIL style THEN DO:
        /* task 12011101 - to update this even on an update */
        IF style.qty-per-set NE 0 THEN
            eb.quantityPerSet:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(style.qty-per-set).
        IF adm-adding-record THEN DO:
          IF ef.board:SCREEN-VALUE EQ "" THEN DO:
            ef.board:SCREEN-VALUE IN BROWSE {&browse-name} = style.material[1].
            RUN new-board.

          END.
        END.


      END.   
    END.
    SELF:screen-value = CAPS(SELF:screen-value).

    RUN valid-wid-len (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.style br-estitm _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF eb.style IN BROWSE br-estitm /* Style */
DO:
  ll-wid-len-warned = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-qty.eqty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-qty.eqty br-estitm _BROWSE-COLUMN B-table-Win
ON ENTRY OF est-qty.eqty IN BROWSE br-estitm /* Est Qty */
DO:
  IF eb.est-type EQ 6                                               AND
     (INT(eb.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) GT 1 OR
      INT(eb.form-no:SCREEN-VALUE IN BROWSE {&browse-name}) GT 1)   THEN DO:
    APPLY "tab" TO {&self-name}.
    RETURN NO-APPLY.    
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-qty.eqty br-estitm _BROWSE-COLUMN B-table-Win
ON LEAVE OF est-qty.eqty IN BROWSE br-estitm /* Est Qty */
DO:
    IF LASTKEY = -1 THEN RETURN.
    
    IF int(est-qty.eqty:screen-value IN BROWSE {&browse-name} ) <= 0 THEN DO:
       MESSAGE "Quantity must be entered. " VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.flute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.flute br-estitm _BROWSE-COLUMN B-table-Win
ON ENTRY OF eb.flute IN BROWSE br-estitm /* Flute */
DO:
  IF lv-foam THEN DO WITH FRAME {&FRAME-NAME}:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.flute br-estitm _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.flute IN BROWSE br-estitm /* Flute */
DO:
   DEF VAR ls-board AS cha NO-UNDO.

   eb.flute:SCREEN-VALUE IN BROWSE {&browse-name} =
       CAPS(eb.flute:SCREEN-VALUE IN BROWSE {&browse-name}).
   
   IF LASTKEY NE -1 THEN DO: 
     RUN valid-flute NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END.
  
   IF adm-new-record AND eb.flute:modified IN BROWSE {&browse-name}
      /*self:screen-value <> ls-prev-val  */
   THEN DO:
     FIND FIRST reftable WHERE reftable.reftable = "STYFLU"
                           AND reftable.company = eb.style:screen-value
                           AND (reftable.loc = eb.flute:screen-value OR lv-foam)
                           AND reftable.code = "BOARD"
                        NO-LOCK NO-ERROR.
     IF AVAIL reftable AND reftable.dscr <> "" THEN DO:
        FIND FIRST item WHERE item.company = gcompany AND
                              item.i-no = reftable.dscr
                              NO-LOCK NO-ERROR.
        IF AVAIL item THEN
                 ASSIGN ef.board:screen-value = item.i-no
                       /* ef.brd-dscr:screen-value = item.est-dscr */
                        eb.flute:screen-value = item.flute
                        eb.test:screen-value = item.reg-no
                        ef.cal:screen-value = /*if item.s-dep <> 0 then string(item.s-dep)
                                              else*/ STRING(item.cal)
                        ls-board = item.i-no
                        .                      
                          
     END.                                         
  /*   if not avail item or (avail item and item.i-no = "" ) then   */
     IF ls-board = "" THEN RUN new-flute-test.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.test
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.test br-estitm _BROWSE-COLUMN B-table-Win
ON ENTRY OF eb.test IN BROWSE br-estitm /* Test */
DO:
  IF lv-foam THEN DO WITH FRAME {&FRAME-NAME}:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.test br-estitm _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.test IN BROWSE br-estitm /* Test */
DO:
  DEF VAR ls-board AS cha NO-UNDO.

  IF LASTKEY NE -1 THEN DO: 
    RUN valid-test NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
  
   /*if ef.board:screen-value in browse {&browse-name} = "" then */
  IF adm-new-record AND eb.test:modified IN BROWSE {&browse-name} THEN
  DO:  
     FIND FIRST reftable WHERE reftable.reftable = "STYFLU"
                           AND reftable.company = eb.style:screen-value IN BROWSE {&browse-name}
                           AND (reftable.loc = eb.flute:screen-value OR lv-foam)
                           AND reftable.code = "BOARD"
                        NO-LOCK NO-ERROR.
     IF AVAIL reftable AND reftable.dscr <> "" THEN DO:
        FIND FIRST item WHERE item.company = gcompany AND
                              item.i-no = reftable.dscr
                              NO-LOCK NO-ERROR.
        IF AVAIL item THEN
                 ASSIGN ef.board:screen-value = item.i-no
                       /* ef.brd-dscr:screen-value = item.est-dscr 
                        eb.flute:screen-value = item.flute
                        eb.test:screen-value = item.reg-no */
                        ef.cal:screen-value = /*if item.s-dep <> 0 then string(item.s-dep)
                                              else*/  STRING(item.cal)
                        ls-board = item.i-no.
                        .                      
                          
     END.                                         
     /*if not avail item or (avail item and item.i-no = "" ) then   */
     IF ls-board = "" THEN RUN new-flute-test.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.board
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.board br-estitm _BROWSE-COLUMN B-table-Win
ON ENTRY OF ef.board IN BROWSE br-estitm /* Board */
DO:
  IF NOT lv-foam THEN
  RUN check-flute-test-change.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.board br-estitm _BROWSE-COLUMN B-table-Win
ON LEAVE OF ef.board IN BROWSE br-estitm /* Board */
DO:
   IF LASTKEY = -1 THEN RETURN.
   
   FIND item WHERE item.company = gcompany
                AND item.i-no = SELF:screen-value IN BROWSE {&browse-name}
                    NO-LOCK NO-ERROR.
   IF NOT AVAIL item THEN DO:
      MESSAGE "Invalid Board. Try Help. " VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.board br-estitm _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF ef.board IN BROWSE br-estitm /* Board */
DO:
  RUN new-board.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.procat br-estitm _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.procat IN BROWSE br-estitm /* Category */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-procat NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.procat br-estitm _BROWSE-COLUMN B-table-Win
ON PAGE-DOWN OF eb.procat IN BROWSE br-estitm /* Category */
DO:
   FIND FIRST fgcat WHERE fgcat.company = gcompany AND
                          fgcat.procat > eb.procat:screen-value IN BROWSE {&browse-name}
                          NO-LOCK NO-ERROR.
   IF AVAIL fgcat THEN eb.procat:screen-value IN BROWSE {&browse-name} = fgcat.procat.
   
                          
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.procat br-estitm _BROWSE-COLUMN B-table-Win
ON PAGE-UP OF eb.procat IN BROWSE br-estitm /* Category */
DO:
   FIND LAST fgcat WHERE fgcat.company = gcompany AND
                          fgcat.procat < eb.procat:screen-value IN BROWSE {&browse-name}
                          NO-LOCK NO-ERROR.
   IF AVAIL fgcat THEN eb.procat:screen-value IN BROWSE {&browse-name} = fgcat.procat.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.len br-estitm _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.len IN BROWSE br-estitm /* Length */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR len-num AS INT NO-UNDO.

   IF LASTKEY = -1 THEN RETURN.

   IF DECIMAL(eb.len:screen-value IN BROWSE {&browse-name}) = 0 THEN DO:
      MESSAGE "Length can not be 0." VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.
   
   v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
   IF v-dec >= v-16-or-32 THEN
   DO:
      MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's)"
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO SELF.
      RETURN NO-APPLY.
   END.

   IF v-cecscrn-dec THEN
   DO:
      len-num = INT(SELF:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE DO: 
          
          eb.len:screen-value IN BROWSE {&browse-name} = string( len-num +  op-dec) .
      END.
   END.

   RUN valid-wid-len (FOCUS) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.len br-estitm _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF eb.len IN BROWSE br-estitm /* Length */
DO:
  ll-wid-len-warned = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.wid br-estitm _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.wid IN BROWSE br-estitm /* Width */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR wid-num AS INT NO-UNDO.

   IF LASTKEY = -1 THEN RETURN.
   IF DECIMAL(eb.wid:screen-value IN BROWSE {&browse-name}) = 0 THEN DO:
        MESSAGE "Width can not be 0. " VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
   END.

   v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
   IF v-dec >= v-16-or-32 THEN DO:
      MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO SELF.
      RETURN NO-APPLY.
   END.

   IF v-cecscrn-dec THEN
   DO:
      wid-num = INT(SELF:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE DO: 
          eb.wid:screen-value IN BROWSE {&browse-name} = string( wid-num +  op-dec) .
      END.
   END.

   RUN valid-wid-len (FOCUS) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.wid br-estitm _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF eb.wid IN BROWSE br-estitm /* Width */
DO:
  ll-wid-len-warned = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.dep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.dep br-estitm _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.dep IN BROWSE br-estitm /* Depth */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR dep-num AS INT NO-UNDO.

   IF LASTKEY <> -1 THEN
   DO:
      v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
      IF v-dec >= v-16-or-32 THEN DO:
         MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
            VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO SELF.
         RETURN NO-APPLY.
      END.
  
   IF v-cecscrn-dec THEN
       DO:
       dep-num = INT(SELF:screen-value) .
       RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
       IF op-error THEN DO:
           MESSAGE "Invalid Dimension."
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
           APPLY "ENTRY" TO SELF.
           RETURN NO-APPLY.
       END.
       ELSE DO: 
           eb.dep:screen-value IN BROWSE {&browse-name} = string( dep-num +  op-dec) .
       END.
     END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-col
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-col br-estitm _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF eb.i-col IN BROWSE br-estitm /* Colors */
DO:
  IF DEC(eb.i-col:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
    eb.i-pass:SCREEN-VALUE IN BROWSE {&browse-name} = "".
  ELSE
  IF DEC(eb.i-pass:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
    eb.i-pass:SCREEN-VALUE IN BROWSE {&browse-name} = "1".

  IF eb.blank-no EQ 1 THEN
    IF DEC(eb.i-col:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
      ASSIGN
       ef.f-col:SCREEN-VALUE IN BROWSE {&browse-name}  = ""
       ef.f-pass:SCREEN-VALUE IN BROWSE {&browse-name} = "".

    ELSE
    IF DEC(ef.f-col:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
      ASSIGN
       ef.f-col:SCREEN-VALUE IN BROWSE {&browse-name}  =
                                eb.i-col:SCREEN-VALUE IN BROWSE {&browse-name}
       ef.f-pass:SCREEN-VALUE IN BROWSE {&browse-name} = "1".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-coat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-coat br-estitm _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF eb.i-coat IN BROWSE br-estitm /* Coating */
DO:
  IF DEC(eb.i-coat:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
    eb.i-coat-p:SCREEN-VALUE IN BROWSE {&browse-name} = "".
  ELSE
  IF DEC(eb.i-coat-p:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
    eb.i-coat-p:SCREEN-VALUE IN BROWSE {&browse-name} = "1".

  IF eb.blank-no EQ 1 THEN
    IF DEC(eb.i-coat:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
      ASSIGN
       ef.f-coat:SCREEN-VALUE IN BROWSE {&browse-name}  = ""
       ef.f-coat-p:SCREEN-VALUE IN BROWSE {&browse-name} = "".

    ELSE
    IF DEC(ef.f-coat:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
      ASSIGN
       ef.f-coat:SCREEN-VALUE IN BROWSE {&browse-name}  =
                                eb.i-coat:SCREEN-VALUE IN BROWSE {&browse-name}
       ef.f-coat-p:SCREEN-VALUE IN BROWSE {&browse-name} = "1".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.f-col
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.f-col br-estitm _BROWSE-COLUMN B-table-Win
ON ENTRY OF ef.f-col IN BROWSE br-estitm /* Inks/Form */
DO:
  IF est.est-type EQ 5 OR eb.blank-no NE 1 THEN DO:
    /*APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}. geting error 4527 */
    APPLY "leave" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.f-col br-estitm _BROWSE-COLUMN B-table-Win
ON LEAVE OF ef.f-col IN BROWSE br-estitm /* Inks/Form */
DO:
  IF LASTKEY NE -1 THEN DO WITH FRAME {&frame-name}:
    IF INT({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
      ef.f-pass:SCREEN-VALUE IN BROWSE {&browse-name} = "0".
    ELSE
    IF INT(ef.f-pass:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
      ef.f-pass:SCREEN-VALUE IN BROWSE {&browse-name} = "1".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.f-pass
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.f-pass br-estitm _BROWSE-COLUMN B-table-Win
ON ENTRY OF ef.f-pass IN BROWSE br-estitm /* Passes/Form */
DO:
  IF est.est-type EQ 5 OR eb.blank-no NE 1 THEN DO:
    /*APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}. geting error 4527 */
    APPLY "leave" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.f-coat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.f-coat br-estitm _BROWSE-COLUMN B-table-Win
ON ENTRY OF ef.f-coat IN BROWSE br-estitm /* Coatings/Form */
DO:
  IF est.est-type EQ 5 OR eb.blank-no NE 1 THEN DO:
    /*APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}. geting error 4527 */
    APPLY "leave" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.f-coat br-estitm _BROWSE-COLUMN B-table-Win
ON LEAVE OF ef.f-coat IN BROWSE br-estitm /* Coatings/Form */
DO:
  IF LASTKEY NE -1 THEN DO WITH FRAME {&frame-name}:
    IF INT({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
      ef.f-coat-p:SCREEN-VALUE IN BROWSE {&browse-name} = "0".
    ELSE
    IF INT(ef.f-coat-p:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
      ef.f-coat-p:SCREEN-VALUE IN BROWSE {&browse-name} = "1".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.f-coat-p
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.f-coat-p br-estitm _BROWSE-COLUMN B-table-Win
ON ENTRY OF ef.f-coat-p IN BROWSE br-estitm /* Coat Passes/Form */
DO:
  IF est.est-type EQ 5 OR eb.blank-no NE 1 THEN DO:
    /*APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}. geting error 4527 */
    APPLY "leave" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.pur-man
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.pur-man br-estitm _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF eb.pur-man IN BROWSE br-estitm /* Purch/Manuf */
DO:
   DEFINE VARIABLE lChackLog AS LOGICAL NO-UNDO .
   DEFINE BUFFER bf-itemfg FOR itemfg .
    FIND FIRST bf-itemfg NO-LOCK
        WHERE bf-itemfg.company EQ cocode
          AND bf-itemfg.i-no EQ eb.stock:SCREEN-VALUE IN BROWSE br-estitm NO-ERROR .
   
    lChackLog = IF eb.pur-man:SCREEN-VALUE IN BROWSE br-estitm EQ "P" THEN TRUE ELSE FALSE .
    IF AVAIL bf-itemfg AND eb.pur-man:SCREEN-VALUE IN BROWSE br-estitm NE ""
        AND bf-itemfg.pur-man NE lChackLog AND NOT bf-itemfg.isaset THEN do:
        MESSAGE "Purchased / Manufactured Field" SKIP 
                "Estimate Does Not Match Finished Goods." SKIP
                "Reset Both? "
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE lCheckPurMan . 
    END.
    ELSE lCheckPurMan = NO .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est.est-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est.est-date br-estitm _BROWSE-COLUMN B-table-Win
ON ENTRY OF est.est-date IN BROWSE br-estitm /* Est Date */
DO:
  IF eb.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN DO:
    APPLY "entry" TO eb.cust-no IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
  ELSE
  IF ls-add-what NE "" AND NOT ll-part-no THEN DO:
    ll-part-no = YES.
    APPLY "entry" TO eb.part-no IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.spare-char-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.spare-char-2 br-estitm _BROWSE-COLUMN B-table-Win
ON ENTRY OF eb.spare-char-2 IN BROWSE br-estitm /* Set? */
DO:
  lv-set-widget = FOCUS.

  IF INDEX("YN", eb.spare-char-2:SCREEN-VALUE IN BROWSE br-estitm) GT 0 THEN
    lv-save-set = eb.spare-char-2:SCREEN-VALUE IN BROWSE br-estitm.
  ELSE
    lv-save-set = "N".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.spare-char-2 br-estitm _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.spare-char-2 IN BROWSE br-estitm /* Set? */
DO:
  IF INDEX("YN", eb.spare-char-2:SCREEN-VALUE IN BROWSE br-estitm) = 0 THEN DO:
      eb.spare-char-2:SCREEN-VALUE IN BROWSE br-estitm = lv-save-set.
      FOCUS = lv-set-widget.

  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}

DO WITH FRAME {&frame-name}:
  {&browse-name}:set-repositioned-row(1, "conditional").
END.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-estimate B-table-Win 
PROCEDURE add-estimate :
/*------------------------------------------------------------------------------
  Purpose:     run from select_add in w-estc.w and 
               called from button select_add in optionse.w
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.

  ASSIGN                
  ll-is-add-from-tool = YES  /* add from option button not from add button */
  ls-add-what = "est" .   /* new estimate */
  /*run est/d-addwh2.w  (output ls-add-what).*/
  /*if ls-add-what = "est"    /* new estimate */
     then run est/d-addset.w (output ls-add-what). /* one item or set cec/est-add.p */*/
  RUN est/d-addfol.w (INPUT YES, OUTPUT ls-add-what). /* one item or set cec/est-add.p */
  IF ls-add-what = "" THEN RETURN NO-APPLY.  /* cancel */

  IF ls-add-what EQ "est" THEN DO:
      RUN get-link-handle IN adm-broker-hdl  (THIS-PROCEDURE,'Record-source':U,OUTPUT char-hdl).
      RUN clearFilterValues IN WIDGET-HANDLE(char-hdl).
  END.

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
  
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
  {src/adm/template/row-list.i "est-qty"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "est"}
  {src/adm/template/row-find.i "est-qty"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assign-qty B-table-Win 
PROCEDURE assign-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*== Qty assignment ==*/
  DEF BUFFER bf-est FOR est.
  DEF BUFFER bf-est-qty FOR est-qty.
  FIND bf-est-qty WHERE RECID(bf-est-qty) = recid(est-qty).
  ASSIGN      bf-est-qty.qty[1] = est-qty.eqty
              bf-est-qty.qty[2] = lv-copy-qty[2]
              bf-est-qty.qty[3] = lv-copy-qty[3]
              bf-est-qty.qty[4] = lv-copy-qty[4]
              bf-est-qty.qty[5] = lv-copy-qty[5]
              bf-est-qty.qty[6] = lv-copy-qty[6]
              bf-est-qty.qty[7] = lv-copy-qty[7]
              bf-est-qty.qty[8] = lv-copy-qty[8]
              bf-est-qty.qty[9] = lv-copy-qty[9]
              bf-est-qty.qty[10] = lv-copy-qty[10]
              bf-est-qty.qty-price[1] = lv-copy-pr[1]
              bf-est-qty.qty-price[2] = lv-copy-pr[2]
              bf-est-qty.qty-price[3] = lv-copy-pr[3]
              bf-est-qty.qty-price[4] = lv-copy-pr[4]
              bf-est-qty.qty-price[5] = lv-copy-pr[5]
              bf-est-qty.qty-price[6] = lv-copy-pr[6]
              bf-est-qty.qty-price[7] = lv-copy-pr[7]
              bf-est-qty.qty-price[8] = lv-copy-pr[8]
              bf-est-qty.qty-price[9] = lv-copy-pr[9]
              bf-est-qty.qty-price[10] = lv-copy-pr[10]
              bf-est-qty.qty-uom[1] = lv-copy-uom[1]
              bf-est-qty.qty-uom[2] = lv-copy-uom[2]
              bf-est-qty.qty-uom[3] = lv-copy-uom[3]
              bf-est-qty.qty-uom[4] = lv-copy-uom[4]
              bf-est-qty.qty-uom[5] = lv-copy-uom[5]
              bf-est-qty.qty-uom[6] = lv-copy-uom[6]
              bf-est-qty.qty-uom[7] = lv-copy-uom[7]
              bf-est-qty.qty-uom[8] = lv-copy-uom[8]
              bf-est-qty.qty-uom[9] = lv-copy-uom[9]
              bf-est-qty.qty-uom[10] = lv-copy-uom[10]
              bf-est-qty.qty-date[1] = lv-copy-date[1]  
              bf-est-qty.qty-date[2] = lv-copy-date[2]  
              bf-est-qty.qty-date[3] = lv-copy-date[3]  
              bf-est-qty.qty-date[4] = lv-copy-date[4]  
              bf-est-qty.qty-date[5] = lv-copy-date[5]  
              bf-est-qty.qty-date[6] = lv-copy-date[6]  
              bf-est-qty.qty-date[7] = lv-copy-date[7]  
              bf-est-qty.qty-date[8] = lv-copy-date[8]  
              bf-est-qty.qty-date[9] = lv-copy-date[9]  
              bf-est-qty.qty-date[10] = lv-copy-date[10]  
              .  
       ASSIGN bf-est-qty.qty[11] = lv-copy-qty[11]
              bf-est-qty.qty[12] = lv-copy-qty[12]
              bf-est-qty.qty[13] = lv-copy-qty[13]
              bf-est-qty.qty[14] = lv-copy-qty[14]
              bf-est-qty.qty[15] = lv-copy-qty[15]
              bf-est-qty.qty[16] = lv-copy-qty[16]
              bf-est-qty.qty[17] = lv-copy-qty[17]
              bf-est-qty.qty[18] = lv-copy-qty[18]
              bf-est-qty.qty[19] = lv-copy-qty[19]
              bf-est-qty.qty[20] = lv-copy-qty[20]
              bf-est-qty.qty-price[11] = lv-copy-pr[11]
              bf-est-qty.qty-price[12] = lv-copy-pr[12]
              bf-est-qty.qty-price[13] = lv-copy-pr[13]
              bf-est-qty.qty-price[14] = lv-copy-pr[14]
              bf-est-qty.qty-price[15] = lv-copy-pr[15]
              bf-est-qty.qty-price[16] = lv-copy-pr[16]
              bf-est-qty.qty-price[17] = lv-copy-pr[17]
              bf-est-qty.qty-price[18] = lv-copy-pr[18]
              bf-est-qty.qty-price[19] = lv-copy-pr[19]
              bf-est-qty.qty-price[20] = lv-copy-pr[20]
              bf-est-qty.qty-uom[11] = lv-copy-uom[11]
              bf-est-qty.qty-uom[12] = lv-copy-uom[12]
              bf-est-qty.qty-uom[13] = lv-copy-uom[13]
              bf-est-qty.qty-uom[14] = lv-copy-uom[14]
              bf-est-qty.qty-uom[15] = lv-copy-uom[15]
              bf-est-qty.qty-uom[16] = lv-copy-uom[16]
              bf-est-qty.qty-uom[17] = lv-copy-uom[17]
              bf-est-qty.qty-uom[18] = lv-copy-uom[18]
              bf-est-qty.qty-uom[19] = lv-copy-uom[19]
              bf-est-qty.qty-uom[20] = lv-copy-uom[20]
              bf-est-qty.qty-date[11] = lv-copy-date[11]  
              bf-est-qty.qty-date[12] = lv-copy-date[12]  
              bf-est-qty.qty-date[13] = lv-copy-date[13]  
              bf-est-qty.qty-date[14] = lv-copy-date[14]  
              bf-est-qty.qty-date[15] = lv-copy-date[15]  
              bf-est-qty.qty-date[16] = lv-copy-date[16]  
              bf-est-qty.qty-date[17] = lv-copy-date[17]  
              bf-est-qty.qty-date[18] = lv-copy-date[18]  
              bf-est-qty.qty-date[19] = lv-copy-date[19]  
              bf-est-qty.qty-date[20] = lv-copy-date[20]  
              .  
      FIND bf-est WHERE bf-est.company = bf-est-qty.company AND
                        bf-est.est-no = bf-est-qty.est-no.
      ASSIGN bf-est.est-qty[1] = est-qty.eqty
             bf-est.est-qty[2] = bf-est-qty.qty[2]
             bf-est.est-qty[3] = bf-est-qty.qty[3]
             bf-est.est-qty[4] = bf-est-qty.qty[4]
             .                  

      ASSIGN lv-copy-date = ?
             lv-copy-qty = 0
             lv-copy-pr = 0
             lv-copy-uom = "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE auto-create-item B-table-Win 
PROCEDURE auto-create-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT PARAMETER ipc-i-no AS CHAR NO-UNDO.
  DEF VAR lv-i-no              AS CHAR NO-UNDO.
  DEF VAR i                    AS INT  NO-UNDO.
  DEF BUFFER bf-eb FOR eb.

  IF NOT AVAIL xest AND AVAIL(eb) THEN DO:
      FIND FIRST xest WHERE xest.company = eb.company 
                        AND xest.est-no  = eb.est-no
                      NO-LOCK NO-ERROR.
  END.
  fil_id = ?.
  
  IF NOT AVAIL xest THEN
      RETURN.
  IF NOT AVAIL xeb THEN
      RETURN.
  /* Make sure set header is created with a '00' */
  IF xest.est-type EQ 2 OR xest.est-type EQ 6 THEN DO:
      FIND FIRST bf-eb WHERE bf-eb.company = xest.company
                         AND bf-eb.est-no  = xest.est-no
                         AND bf-eb.form-no = 0
                       NO-LOCK NO-ERROR.
      
      IF AVAIL bf-eb AND bf-eb.stock-no = "" THEN DO:
         RUN fg/GetFGItemID.p (ROWID(bf-eb), "", OUTPUT lv-i-no). 
         FIND CURRENT bf-eb EXCLUSIVE-LOCK.
         bf-eb.stock-no = lv-i-no.
         
        FIND xeb WHERE ROWID(xeb) = ROWID(bf-eb) NO-LOCK.
        RUN fg/ce-addfg.p (bf-eb.stock-no).
        FIND itemfg WHERE itemfg.company = xest.company
                      AND itemfg.i-no = bf-eb.stock-no
                    NO-ERROR.
        IF AVAIL itemfg THEN
            itemfg.isaset = TRUE.
        RELEASE itemfg.
         fil_id = RECID(bf-eb).
      END.
  END.
  /* From oe/d-oeitem.w */

  IF xest.est-type EQ 2 OR xest.est-type EQ 6 THEN DO:
       IF fil_id = ? THEN
         fil_id = RECID(xeb).
       s-est-no = xest.est-no.       
       RUN est/fgadd2.p.   /** 2pc box fg create/update routine **/
  END.
  ELSE DO:      
      RUN fg/GetFGItemID.p (ROWID(xeb), "", OUTPUT lv-i-no). 
  END.

  FIND FIRST tt-stock-no WHERE tt-stock-no.eb-row-id = ROWID(xeb)
                         NO-ERROR.

  IF NOT AVAIL tt-stock-no THEN DO:
      CREATE tt-stock-no.
      tt-stock-no.eb-row-id = ROWID(xeb).
      tt-stock-no.stock-no = lv-i-no.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE blank-cp B-table-Win 
PROCEDURE blank-cp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ld-dim AS DEC NO-UNDO.


  {est/blankcp1.i}

  IF AVAIL itemfg OR AVAIL b-eb THEN DO:
    ASSIGN
     ld-dim = DEC(eb.len:SCREEN-VALUE IN BROWSE {&browse-name})
     eb.len:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(display-cw-dim(YES,ld-dim))
     ld-dim = DEC(eb.wid:SCREEN-VALUE IN BROWSE {&browse-name})
     eb.wid:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(display-cw-dim(YES,ld-dim))
     ld-dim = DEC(eb.dep:SCREEN-VALUE IN BROWSE {&browse-name})
     eb.dep:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(display-cw-dim(YES,ld-dim)).

    IF AVAIL b-eb THEN
      ASSIGN
       eb.i-pass:SCREEN-VALUE IN BROWSE {&browse-name}   = STRING(b-eb.i-pass)
       eb.i-coat-p:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(b-eb.i-coat-p)   
       eb.flute:SCREEN-VALUE IN BROWSE {&browse-name}    = b-eb.flute
       eb.test:SCREEN-VALUE IN BROWSE {&browse-name}     = b-eb.test       
       eb.tab-in:SCREEN-VALUE IN BROWSE {&browse-name}   = STRING(b-eb.tab-in).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-blank-size B-table-Win 
PROCEDURE calc-blank-size :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /* calc blank W,L SqIn */

   DEF BUFFER bf-eb FOR eb .
   DEF VAR lv-panels AS LOG NO-UNDO.
   DEF VAR i AS INT NO-UNDO.
   DEF VAR j AS INT NO-UNDO.
   DEF VAR v-score-char LIKE v-lscore-c EXTENT 100.
   DEF VAR ld-total AS DEC NO-UNDO.
   DEF VAR v-index AS INT NO-UNDO.
   DEF VAR v-str AS CHAR NO-UNDO.

   FIND FIRST sys-ctrl  WHERE sys-ctrl.company EQ cocode
                           AND sys-ctrl.name    EQ "PANELS"
        NO-LOCK NO-ERROR.
   IF NOT AVAIL sys-ctrl THEN DO:
      CREATE sys-ctrl.
      ASSIGN  sys-ctrl.company = cocode
              sys-ctrl.name    = "PANELS"
              sys-ctrl.descrip = "CE Lock=Yes Panel Size Popup when Overriding W&L?"
              sys-ctrl.log-fld = YES.
      MESSAGE sys-ctrl.descrip
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE sys-ctrl.log-fld.
   END.
   lv-panels = sys-ctrl.log-fld.

   FIND xest WHERE RECID(xest) = recid(est) NO-LOCK.
   FIND xef WHERE RECID(xef) = recid(ef) NO-LOCK.
   FIND xeb WHERE RECID(xeb) = recid(eb) NO-LOCK.
   
   IF AVAIL eb THEN
   FIND FIRST style WHERE
        style.company = eb.company AND
        style.style = eb.style
        NO-LOCK NO-ERROR.

   IF AVAIL style THEN DO:
      IF style.type <> "F" THEN RUN calc-blank-size2. 

      {cec/msfcalc.i}
      RUN est/u2kinc1c.p (RECID(eb)).
      RUN est/u2kinc2c.p (RECID(eb)).
      FIND FIRST formule NO-LOCK NO-ERROR.
      FIND bf-eb OF eb EXCLUSIVE-LOCK.    
      ASSIGN bf-eb.t-wid = (formule.formule[1])
          bf-eb.t-len = (formule.formule[2])
          bf-eb.t-sqin = (formule.formule[7] * formule.formule[8])
          bf-eb.k-wid-array2 = 0
          bf-eb.k-len-array2 = 0.
      
      IF /*not lv-panels or*/ style.type = "F" THEN 
         ASSIGN bf-eb.k-wid-array2[1] = bf-eb.t-wid
                bf-eb.k-len-array2[1] = bf-eb.t-len
                .
      ELSE DO:
         RUN cec/descalc.p (RECID(xest),RECID(xeb)).
          
         DO i = 1 TO EXTENT(xeb.k-wid-scr-type2):
           ASSIGN
            xeb.k-wid-scr-type2[i] = lv-k-wid-scr-type[i]
            xeb.k-len-scr-type2[i] = lv-k-len-scr-type[i].
         END.
          
         IF v-lscore-c BEGINS "No" THEN
            ASSIGN  xeb.k-wid-array2[1] = xeb.t-wid
                    xeb.k-len-array2[1] = xeb.t-len.
         ELSE IF xest.metric THEN DO:  /*Note - this may work for all*/
            DO i = 1 TO EXTENT(dPanelsLength):
                ASSIGN
                xeb.k-wid-array2[i] = dPanelsWidth[i]
                xeb.k-len-array2[i] = dPanelsLength[i].
            END.
             
          END.
           ELSE DO:
            i = 0.
            FOR EACH w-box-design-line:
              ASSIGN
                 i = i + 1
                 xeb.k-wid-array2[i] = w-box-design-line.wscore-d.
           
              {sys/inc/k16bb.i xeb.k-wid-array2[i]}
            END.
           
            ASSIGN  v-score-char    = ""
                    j               = 1.
            DO i = 1 TO 80:
              IF substr(v-lscore-c,i,1) NE "" THEN DO:
                 v-score-char[j] = v-score-char[j] + substr(v-lscore-c,i,1).
                 IF substr(v-lscore-c,i + 1,1) EQ "" THEN
                    ASSIGN  v-score-char[j] = TRIM(v-score-char[j])
                            j = j + 1.
              END.
              IF j GT 12 THEN LEAVE.
            END.

            IF ll-add-set-part EQ NO AND ll-add-set-part-2 EQ NO THEN
            DO i = 1 TO EXTENT(xeb.k-len-array2):

               IF v-cecscrn-dec AND v-score-char[i] NE "" THEN
                  ASSIGN
                     v-index = INDEX(v-score-char[i],".")
                     v-str = SUBSTRING(v-score-char[i],v-index + 1)
                     v-str = LEFT-TRIM(STRING(INT(v-str) / 64.0,">.999999"))
                     SUBSTRING(v-score-char[i],v-index) = v-str.

               xeb.k-len-array2[i] = dec(v-score-char[i]).
               {sys/inc/k16bb.i xeb.k-len-array2[i]}.
            END.
            ELSE
            DO:
               ld-total = 0.
              IF AVAIL style THEN
               DO i = 1 TO style.dim-df + 1:
              
                  IF i EQ 1 THEN
                  DO:
                     IF ll-add-set-part EQ YES AND ll-add-set-part-2 EQ NO THEN
                        xeb.k-len-array2[i] = tt-eb-set-part.end-cell-length-1.
                     ELSE
                        xeb.k-len-array2[i] = v-end-cell-w1.
                  END.
                  ELSE IF i EQ style.dim-df + 1 THEN
                  DO:
                     IF ll-add-set-part EQ YES AND ll-add-set-part-2 EQ NO THEN
                        xeb.k-len-array2[i] = tt-eb-set-part.end-cell-length-2.
                     ELSE
                        xeb.k-len-array2[i] = v-end-cell-w2.
                  END.
                  ELSE
                  DO:
                     IF ll-add-set-part EQ YES AND ll-add-set-part-2 EQ NO THEN
                        xeb.k-len-array2[i] = tt-eb-set-part.in-cell-length.
                     ELSE
                        xeb.k-len-array2[i] = v-in-cell-w.
                  END.
              
                  {sys/inc/k16bb.i xeb.k-len-array2[i]}.
                  ld-total = ld-total + xeb.k-len-array2[i].
               END.
              
               xeb.t-len = ld-total.

            END.
         END.  /* else v-lscore */
       END. /* panels or not foam */
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-blank-size2 B-table-Win 
PROCEDURE calc-blank-size2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND xeb WHERE RECID(xeb) = recid(eb) NO-LOCK.
   
   {est/u2estc.i eb.gluelap 1}
   {est/u2estc.i eb.k-wid 2}
   FIND FIRST item WHERE item.company = est.company
                    AND item.i-no EQ eb.adhesive
                  NO-LOCK NO-ERROR.
   IF AVAIL item THEN DO:
 
            IF item.mat-type EQ "G" THEN DO:
                    IF eb.tab-in  THEN DO:
                       {est/u2estc.i eb.k-len 3}
                    END.
                    ELSE DO:
                       {est/u2estc.i eb.k-len 4}
                    END.
            END.
            ELSE IF item.mat-type EQ "S" THEN DO:
                    IF eb.tab-in  THEN DO:
                       {est/u2estc.i eb.k-len 5}
                    END.
                    ELSE DO:
                       {est/u2estc.i eb.k-len 6}
                    END.
            END.
            ELSE IF item.mat-type EQ "T" THEN DO:
                    eb.tab-in = NO.
                    {est/u2estc.i eb.k-len 7}
            END. 
    END.
    ELSE DO:
                 eb.tab-in = NO.
                 {est/u2estc.i eb.k-len 7}
    END.

    IF eb.len EQ eb.wid
    THEN DO:
                 {est/u2estc.i eb.k-wid 2 dim-fit}
    END.
    ELSE DO:
                 {est/u2estc.i eb.k-wid 2}
    END.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-layout B-table-Win 
PROCEDURE calc-layout :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-new AS LOG NO-UNDO.

  DEF BUFFER bf-eb FOR eb.

  DEF VAR ll AS LOG NO-UNDO.


  FIND xest WHERE ROWID(xest) = ROWID(est).
  FIND xef  WHERE ROWID(xef)  = ROWID(ef).
  FIND xeb  WHERE ROWID(xeb)  = ROWID(eb).

  ll = ip-new AND
       NOT CAN-FIND(FIRST bf-eb
                    WHERE bf-eb.company EQ eb.company
                      AND bf-eb.est-no  EQ eb.est-no
                      AND bf-eb.form-no EQ eb.form-no
                      AND ROWID(bf-eb)  NE ROWID(eb)).

  IF NOT ll THEN
  DO:
    IF ll-add-set-part-2 EQ NO THEN
       MESSAGE "Do you wish to reset layout screen?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
          UPDATE ll.
    ELSE
       ll = YES.
  END.

  IF ll THEN DO:
    IF NOT lv-foam THEN DO:
      {sys/inc/ceroute1.i w id l en}
    END.

    RUN cec/calc-dim.p.
  END.

  IF ceroute-chr NE "" THEN DO:
    FIND FIRST mach
        WHERE mach.company EQ est.company
          AND mach.loc     EQ est.loc
          AND mach.m-code  EQ ceroute-chr
          AND mach.dept[1] EQ "CR"
        NO-LOCK NO-ERROR.
    IF AVAIL mach THEN DO:
      ASSIGN
       xef.m-code   = ceroute-chr
       xef.lsh-lock = NO
       xeb.num-wid  = 1
       xeb.num-len  = 1.

      RUN cec/calc-dim1.p NO-ERROR.

      ASSIGN
       xef.gsh-len = xef.gsh-len - (xef.nsh-len * xef.n-out-l)
       xef.n-out-l = 1
       xef.gsh-len = xef.gsh-len + (xef.nsh-len * xef.n-out-l).

      IF ceroute-int NE 0 AND ceroute-int LT xef.gsh-wid THEN
        ASSIGN
         xef.n-out   = TRUNC(ceroute-int / xef.nsh-wid,0)
         xef.gsh-wid = xef.n-out * xef.nsh-wid + (mach.min-trimw * 2).
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-layout4Artios B-table-Win 
PROCEDURE calc-layout4Artios :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-new AS LOG NO-UNDO.

  DEF BUFFER bf-eb FOR eb.

  DEF VAR ll AS LOG NO-UNDO.

  FIND xest WHERE ROWID(xest) = ROWID(est).
  FIND xef  WHERE ROWID(xef)  = ROWID(ef).
  FIND xeb  WHERE ROWID(xeb)  = ROWID(eb).

  ll = ip-new.

  IF ll THEN DO:
    IF NOT lv-foam THEN DO:
      {sys/inc/ceroute1.i w id l en}

      RUN cec/calc-dim.p.

      /*
      IF xef.m-code EQ "" THEN xef.m-code = ceroute-chr.

      find first mach {sys/look/mach.w}
            and mach.m-code eq xef.m-code use-index m-code no-lock no-error.
      if avail mach then 
            assign
             xef.m-dscr   = mach.m-dscr
             xef.roll     = mach.p-type eq "R"
             xef.lam-dscr = "S"
             xef.lsh-wid  = mach.max-len
             xef.lsh-len  = mach.max-wid. */
    END.

    /*RUN cec/calc-dimCAD.p.*/
  END.

  IF ceroute-chr NE "" THEN DO:
    FIND FIRST mach
        WHERE mach.company EQ est.company
          AND mach.loc     EQ est.loc
          AND mach.m-code  EQ ceroute-chr
          AND mach.dept[1] EQ "CR"
        NO-LOCK NO-ERROR.
    IF AVAIL mach THEN DO:
      ASSIGN
       xef.m-code   = ceroute-chr
       xef.lsh-lock = NO
       xeb.num-wid  = 1
       xeb.num-len  = 1.

      RUN cec/calc-dimCAD1.p NO-ERROR.

      ASSIGN
       xef.gsh-len = xef.gsh-len - (xef.nsh-len * xef.n-out-l)
       xef.n-out-l = 1
       xef.gsh-len = xef.gsh-len + (xef.nsh-len * xef.n-out-l).

      IF ceroute-int NE 0 AND ceroute-int LT xef.gsh-wid THEN
        ASSIGN
         xef.n-out   = TRUNC(ceroute-int / xef.nsh-wid,0)
         xef.gsh-wid = xef.n-out * xef.nsh-wid + (mach.min-trimw * 2).
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-pass B-table-Win 
PROCEDURE calc-pass :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* *********** copied from uest3.p ***********/

      DEF VAR k AS INT NO-UNDO.
      DEF VAR counter AS INT NO-UNDO.
      DEF VAR i AS INT NO-UNDO.
      DEF VAR j AS INT NO-UNDO.
      DEF VAR save_id AS RECID NO-UNDO.
      DEF VAR save_id2 AS RECID NO-UNDO.
      DEF BUFFER alt-item FOR item .
      DEF VAR choice AS LOG NO-UNDO.
      DEF BUFFER bf-eb FOR eb.
      
      IF AVAIL eb THEN
      FIND FIRST style WHERE style.company = eb.company AND
                 style.style = eb.style NO-LOCK NO-ERROR.
      IF AVAIL style THEN DO:
         IF k = 0 THEN k = INTEGER(style.material[3]).

         RELEASE ITEM.

         IF AVAIL style AND style.material[2] NE "" THEN
            FIND FIRST item WHERE
                 item.company = eb.company AND
                 item.i-no = style.material[2]
                 NO-LOCK NO-ERROR.

         IF AVAIL item THEN k = IF AVAIL style THEN INTEGER(style.material[3]) ELSE 0.

         RELEASE alt-item.

         IF AVAIL style AND style.material[6] NE "" THEN
            FIND FIRST alt-item WHERE
                 alt-item.company  = eb.company  AND
                 alt-item.mat-type = "V" AND
                 alt-item.i-no     = style.material[6]
                 NO-LOCK NO-ERROR.
      END.
      IF NOT AVAIL item OR NOT AVAIL alt-item OR (k = 0) THEN DO:
         FIND FIRST ce-ctrl WHERE ce-ctrl.company = eb.company AND
                                  ce-ctrl.loc = eb.loc
                                   NO-LOCK NO-ERROR.
         IF k = 0 THEN k = ce-ctrl.def-inkcov.
         IF NOT AVAIL item THEN DO:

            FIND FIRST item WHERE item.company = eb.company AND
                       item.i-no = ce-ctrl.def-ink NO-LOCK NO-ERROR.
         END.
         IF NOT AVAIL alt-item THEN
            FIND FIRST alt-item WHERE alt-item.company  = eb.company  AND
                                      alt-item.mat-type = "V"     AND
                                      alt-item.i-no     = ce-ctrl.def-coat
                                      NO-LOCK NO-ERROR.
      END.
 
      ASSIGN
      save_id = RECID(item)
      save_id2 = RECID(alt-item)
      j = (INTEGER(eb.i-col:screen-value IN BROWSE {&browse-name})
          + integer(eb.i-coat:screen-value IN BROWSE {&browse-name}))
      counter = 1
      choice = TRUE.

      {sys/inc/roundup.i j}
  
      FIND bf-eb OF eb EXCLUSIVE-LOCK.    
      IF eb.i-col > 0 THEN ASSIGN bf-eb.i-pass = 1.
      IF eb.i-coat > 0 THEN ASSIGN bf-eb.i-coat-p = 1.
      IF choice THEN DO i = 1 TO 10:
         IF i LE integer(eb.i-col) THEN DO WITH FRAME {&frame-name}:
              FIND item WHERE RECID(item) = save_id NO-LOCK NO-ERROR.

              ASSIGN bf-eb.i-ps[i]   = counter
                     bf-eb.i-code[i] = item.i-no
                     bf-eb.i-dscr[i] = item.i-name
                     bf-eb.i-%[i]    = k.
         END.
         ELSE IF (i > integer(eb.i-col)) AND
                 (i <= (INTEGER(eb.i-col) + 
                       integer(eb.i-coat)) )
         THEN DO:
              FIND alt-item WHERE RECID(alt-item) = save_id2 NO-LOCK NO-ERROR.
              ASSIGN bf-eb.i-ps[i]   = counter
                     bf-eb.i-code[i] = IF AVAIL alt-item THEN alt-item.i-no ELSE ""
                     bf-eb.i-dscr[i] = IF AVAIL alt-item THEN alt-item.i-name ELSE ""
                     bf-eb.i-%[i]    = 100.
         END.
         ELSE IF (i >  (eb.i-col + eb.i-coat) )
         THEN DO:
            ASSIGN bf-eb.i-ps[i]   = 0  
                     bf-eb.i-code[i] = ""
                     bf-eb.i-dscr[i] = "" 
                     bf-eb.i-%[i]    = 0.  
        
         END.
         IF j <> 0 AND i MODULO j = 0 THEN counter = counter + 1.
         IF counter > (eb.i-pass) THEN counter = eb.i-pass.         
      END. 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-delete B-table-Win 
PROCEDURE check-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll AS LOG INIT NO NO-UNDO.

  DEF BUFFER del-eb FOR eb.


  IF AVAIL eb THEN
  FOR EACH del-eb NO-LOCK
      WHERE del-eb.company EQ eb.company
        AND del-eb.est-no  EQ eb.est-no
        AND ((del-eb.form-no EQ eb.form-no AND
              del-eb.blank-no EQ eb.blank-no) OR ll-mass-del):

    IF ((del-eb.ord-no NE 0 AND
         CAN-FIND(FIRST oe-ordl
                  WHERE oe-ordl.company EQ del-eb.company
                    AND oe-ordl.ord-no  EQ del-eb.ord-no
                    AND oe-ordl.est-no  EQ del-eb.est-no)) OR
        CAN-FIND(FIRST job-hdr
                 WHERE job-hdr.company  EQ est.company
                   AND job-hdr.est-no   EQ est.est-no
                   AND job-hdr.opened   EQ YES
                   AND job-hdr.frm      EQ del-eb.form-no
                   AND job-hdr.blank-no EQ del-eb.blank-no))    AND
       (est.est-type NE 8 OR
        (est.form-qty EQ 1 AND ef.blank-qty EQ 1))              THEN DO:
      IF est.est-type EQ 6 THEN
        MESSAGE "An order and/or job exists for this estimate, " +
                "delete component anyway?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE ll.
      ELSE
        MESSAGE "An order and/or job exists for this estimate, cannot delete..."
            VIEW-AS ALERT-BOX ERROR.
      IF ll THEN LEAVE.
      ELSE RETURN ERROR.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-flute-test-change B-table-Win 
PROCEDURE check-flute-test-change :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll AS LOG NO-UNDO.

                                                                           
  DO WITH FRAME {&FRAME-NAME}:
    ll = NO.

    IF (lv-hold-flute NE "" AND
        eb.flute:SCREEN-VALUE IN BROWSE {&browse-name} NE lv-hold-flute) OR
       (lv-hold-test  NE "" AND
        eb.test:SCREEN-VALUE IN BROWSE {&browse-name}  NE lv-hold-test)  THEN
      MESSAGE "Find new Board Material?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll.

    RUN set-hold-values.

    IF ll THEN DO:
      RUN new-flute-test.
      APPLY "entry" TO ef.board IN BROWSE {&browse-name}.
      APPLY "help" TO  {&browse-name}.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-for-set B-table-Win 
PROCEDURE check-for-set :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-eb FOR eb.
  DEF BUFFER bf-ef FOR ef.

  DEF VAR ll-rol AS LOG NO-UNDO.
  DEF VAR ll-set AS LOG NO-UNDO.


  FIND xest WHERE RECID(xest) EQ RECID(est).

  i = 0.
  FOR EACH bf-eb
      WHERE bf-eb.company EQ est.company
        AND bf-eb.est-no  EQ est.est-no
        AND bf-eb.form-no NE 0
      NO-LOCK:

    i = i + 1.
  END.

  ll-set = i GT 1.

  IF NOT ll-set THEN DO:
    ll-rol = eb.len EQ 12 AND eb.dep EQ 0 AND eb.yld-qty GT 1 AND
             ll-add-set-part EQ NO AND ll-add-set-part-2 EQ NO.
  
    IF ll-rol THEN DO ON ENDKEY UNDO, LEAVE:
      MESSAGE "Is this estimate for a Roll?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE ll-rol.
    END.

    ll-set = eb.quantityPerSet GT 1 AND NOT ll-rol.
  END.

  FIND FIRST bf-eb
      WHERE bf-eb.company EQ xest.company
        AND bf-eb.est-no  EQ xest.est-no
        AND bf-eb.form-no EQ 0
      NO-ERROR.

  IF ll-set THEN DO:
    xest.est-type = 6.

    IF NOT AVAIL bf-eb THEN DO:
      {ce/set-info.a 6 "bf-" "x"}

      ASSIGN
       bf-eb.stock-no   = eb.stock-no
       bf-eb.part-no    = eb.part-no
       bf-eb.part-dscr1 = eb.part-dscr1
       bf-eb.part-dscr2 = eb.part-dscr2
       bf-eb.procat     = eb.procat
       bf-eb.len        = eb.len
       bf-eb.wid        = eb.wid
       bf-eb.dep        = eb.dep.
       
      /*RUN cec/d-updset.w (RECID(b-eb),6).*/
    END.

    FIND FIRST fg-set WHERE fg-set.company = eb.company
                        AND fg-set.set-no = bf-eb.stock-no
                        AND fg-set.part-no = eb.stock-no NO-ERROR.
    IF AVAIL fg-set THEN DO:
      fg-set.qtyPerSet = eb.quantityPerSet.
      FIND CURRENT fg-set NO-LOCK NO-ERROR.
    END.
  END.

  ELSE
  IF NOT AVAIL bf-eb OR bf-eb.part-no EQ "" THEN xest.est-type = 5.

  FIND xest WHERE RECID(xest) EQ RECID(est) NO-LOCK.

  FOR EACH bf-ef
      WHERE bf-ef.company EQ est.company
        AND bf-ef.est-no  EQ est.est-no:
    bf-ef.est-type = xest.est-type.
  END.
  
  FOR EACH bf-eb
      WHERE bf-eb.company EQ est.company
        AND bf-eb.est-no  EQ est.est-no
        AND bf-eb.form-no NE 0:
    bf-eb.est-type = xest.est-type.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-2-form-zero B-table-Win 
PROCEDURE copy-2-form-zero :
/*------------------------------------------------------------------------------
  Purpose:     Copy form 1 blank 1 data to the zero record.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE BUFFER bf-eb FOR eb.


  FIND FIRST bf-eb EXCLUSIVE-LOCK
      WHERE bf-eb.company  EQ eb.company
        AND bf-eb.est-no   EQ eb.est-no
        AND bf-eb.form-no  EQ 0
      NO-ERROR.

  IF AVAIL bf-eb THEN
    ASSIGN bf-eb.cas-no    = eb.cas-no
           bf-eb.cas-cost  = eb.cas-cost
           bf-eb.cas-cnt   = eb.cas-cnt
           bf-eb.cas-pal   = eb.cas-pal
           bf-eb.tr-no     = eb.tr-no
           bf-eb.tr-cost   = eb.tr-cost
           bf-eb.tr-cnt    = eb.tr-cnt
           bf-eb.tr-cas    = eb.tr-cas
           bf-eb.stacks    = eb.stacks
           bf-eb.stack-code = eb.stack-code
           bf-eb.chg-method = eb.chg-method
           bf-eb.weight-m  = eb.weight-m
           bf-eb.carrier   = eb.carrier
           bf-eb.dest-code = eb.dest-code
           bf-eb.fr-out-c  = eb.fr-out-c
           bf-eb.fr-out-m  = eb.fr-out-m
           bf-eb.cas-len   = eb.cas-len
           bf-eb.cas-wid   = eb.cas-wid
           bf-eb.cas-dep   = eb.cas-dep
           bf-eb.cas-wt    = eb.cas-wt
           bf-eb.tr-len    = eb.tr-len
           bf-eb.tr-wid    = eb.tr-wid
           bf-eb.tr-dep    = eb.tr-dep.

  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-from-est B-table-Win 
PROCEDURE copy-from-est :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-new-eb FOR eb.
  DEF BUFFER bf-new-ef FOR ef.

  is-item-copied-from-est  = YES.

  FIND bf-new-eb WHERE RECID(bf-new-eb) = IF RECID(eb) <> ? THEN RECID(eb) ELSE lv-eb-recid .
  FIND bf-new-ef WHERE RECID(bf-new-ef) = IF RECID(eb) <> ? THEN RECID(ef) ELSE lv-ef-recid.
  
  BUFFER-COPY xeb EXCEPT xeb.company xeb.est-no xeb.form-no xeb.blank-no TO bf-new-eb .
  BUFFER-COPY xef EXCEPT xef.company xef.est-no xef.form-no TO bf-new-ef.
  
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-from-est2 B-table-Win 
PROCEDURE copy-from-est2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
       ASSIGN eb.len:screen-value IN BROWSE {&browse-name} = STRING(xeb.len)
              eb.wid:screen-value IN BROWSE {&browse-name} = STRING(xeb.wid)
              eb.dep:screen-value IN BROWSE {&browse-name} = STRING(xeb.dep)
              eb.procat:screen-value IN BROWSE {&browse-name} = xeb.procat
              eb.style:screen-value IN BROWSE {&browse-name} = xeb.style
              eb.stock-no:screen-value IN BROWSE {&browse-name} = xeb.stock-no
              eb.part-dscr1:screen-value IN BROWSE {&browse-name} = xeb.part-dscr1
              eb.part-no:screen-value IN BROWSE {&browse-name} = xeb.part-no
              ef.board:screen-value IN BROWSE {&browse-name} = xef.board
              ef.cal:screen-value IN BROWSE {&browse-name} = STRING(xef.cal)
              eb.i-col:screen-value IN BROWSE {&browse-name} = STRING(xeb.i-col)
              eb.i-coat:screen-value IN BROWSE {&browse-name} = STRING(xeb.i-coat)
              .
       RUN set-lv-foam.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-line B-table-Win 
PROCEDURE copy-line :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rowid AS ROWID NO-UNDO.


  ll-new-record = YES.
       
  IF ls-add-what EQ "form" THEN
    RUN est/NewEstimateForm.p ('C', ROWID(est), OUTPUT lv-rowid).
  ELSE
    RUN cec/newblank.p (ROWID(ef), OUTPUT lv-rowid).

  FIND eb WHERE ROWID(eb) EQ lv-rowid NO-LOCK NO-ERROR.
  lv-eb-recid = RECID(eb).
  FIND FIRST ef OF eb NO-LOCK NO-ERROR.
  lv-ef-recid = RECID(ef).

  RUN est/blks-frm.p (ROWID(ef)).
  FIND CURRENT ef NO-LOCK.

  RUN est/frms-est.p (ROWID(est)).
  FIND CURRENT est NO-LOCK.

  ASSIGN
   lv-eb-recid = RECID(eb) 
   lv-ef-recid = RECID(ef).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-e-itemfg-vend B-table-Win 
PROCEDURE create-e-itemfg-vend :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF BUFFER bf-e-itemfg-vend FOR e-itemfg-vend.
   DEF BUFFER bf-e-itemfg FOR e-itemfg.

   IF eb.stock-no EQ "" AND
      NOT CAN-FIND(FIRST e-itemfg-vend
                   WHERE e-itemfg-vend.company EQ eb.company
                     AND e-itemfg-vend.est-no = eb.est-no
                     AND e-itemfg-vend.eqty = eb.eqty
                     AND e-itemfg-vend.form-no = eb.form-no
                     AND e-itemfg-vend.blank-no = eb.blank-no
                     AND e-itemfg-vend.i-no    EQ ""
                     AND e-itemfg-vend.vend-no EQ "") THEN DO TRANSACTION:
      
      CREATE e-itemfg-vend.
      
      ASSIGN e-itemfg-vend.company = eb.company
             e-itemfg-vend.item-type = NO
             e-itemfg-vend.est-no = eb.est-no
             e-itemfg-vend.eqty = eb.eqty
             e-itemfg-vend.form-no = eb.form-no
             e-itemfg-vend.blank-no = eb.blank-no
             e-itemfg-vend.run-qty[1] = 99999999.
      RELEASE e-itemfg-vend.
   END.
   ELSE
   IF eb.stock-no NE "" AND
      NOT CAN-FIND(FIRST e-itemfg-vend
                   WHERE e-itemfg-vend.company EQ eb.company
                     AND e-itemfg-vend.est-no = eb.est-no
                     AND e-itemfg-vend.eqty = eb.eqty
                     AND e-itemfg-vend.form-no = eb.form-no
                     AND e-itemfg-vend.blank-no = eb.blank-no
                     AND e-itemfg-vend.i-no = eb.stock-no) THEN DO:

      FOR EACH bf-e-itemfg-vend WHERE
          bf-e-itemfg-vend.company EQ eb.company AND
          bf-e-itemfg-vend.i-no = eb.stock-no AND
          bf-e-itemfg-vend.est-no EQ ""
          NO-LOCK:
          
          DO TRANSACTION:
            CREATE e-itemfg-vend.
            BUFFER-COPY bf-e-itemfg-vend TO e-itemfg-vend
            ASSIGN e-itemfg-vend.est-no = eb.est-no
                 e-itemfg-vend.eqty = eb.eqty
                 e-itemfg-vend.form-no = eb.form-no
                 e-itemfg-vend.blank-no = eb.blank-no.            

            FIND FIRST bf-e-itemfg WHERE
                 bf-e-itemfg.company EQ bf-e-itemfg-vend.company AND
                 bf-e-itemfg.i-no EQ bf-e-itemfg-vend.i-no
                 NO-LOCK NO-ERROR.

            IF AVAIL bf-e-itemfg THEN
            DO:
               e-itemfg-vend.std-uom = bf-e-itemfg.std-uom.
               RELEASE bf-e-itemfg.
            END.

            RELEASE e-itemfg-vend.
          END.
      END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-inst B-table-Win 
PROCEDURE create-inst :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-notes FOR notes.
  

  FIND FIRST cust
      WHERE cust.company EQ eb.company
        AND cust.cust-no EQ eb.cust-no
      NO-LOCK NO-ERROR.

  IF AVAIL cust THEN
  FOR EACH notes
      WHERE notes.rec_key   EQ cust.rec_key
        AND notes.note_type EQ "D"
        AND notes.note_code NE ""
      NO-LOCK:

    FIND FIRST bf-notes
        WHERE bf-notes.rec_key   EQ est.rec_key
          AND bf-notes.note_type EQ notes.note_type
          AND bf-notes.note_code EQ notes.note_code
      NO-LOCK NO-ERROR.
  
    IF NOT AVAIL bf-notes THEN DO:
      CREATE bf-notes.
      BUFFER-COPY notes EXCEPT notes.note_form_no TO bf-notes
      ASSIGN
       bf-notes.rec_key   = est.rec_key
       bf-notes.note_date = TODAY
       bf-notes.note_time = TIME.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-prep B-table-Win 
PROCEDURE create-prep :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN est/BuildDefaultPreps.p(BUFFER est,
                              BUFFER ef,
                              INPUT eb.form-no,
                              INPUT 0).
/*   def var i as int no-undo.                                                                                                                          */
/*   find last est-prep where est-prep.company = gcompany and                                                                                           */
/*                            est-prep.est-no = est.est-no and                                                                                          */
/*                            est-prep.eqty = est-qty.eqty                                                                                              */
/*                            no-lock no-error.                                                                                                         */
/*   i = if avail est-prep then est-prep.line + 1 else 1.                                                                                               */
/*                                                                                                                                                      */
/*   for each prep where prep.company = gcompany and prep.dfault eq yes no-lock:                                                                        */
/*       create est-prep.                                                                                                                               */
/*       assign est-prep.e-num  = est.e-num                                                                                                             */
/*              est-prep.company = est.company                                                                                                          */
/*              est-prep.est-no = est.est-no                                                                                                            */
/*              est-prep.eqty = est-qty.eqty                                                                                                            */
/*              est-prep.line   = i                                                                                                                     */
/*              est-prep.s-num  = eb.form-no                                                                                                            */
/*              est-prep.b-num  = 0 /*1 */                                                                                                              */
/*              est-prep.qty    = if prep.mat-type eq "r" and avail ef then ef.die-in                                                                   */
/*                                else if prep.mat-type eq "b" and  avail ef then ef.nsh-wid * ef.nsh-len /* ef.adh-sqin is 0 in Corrware - die inch */ */
/*                                else 1  /* mat-type eq "m" */                                                                                         */
/*             est-prep.code   = prep.code                                                                                                              */
/*             est-prep.dscr   = prep.dscr                                                                                                              */
/*             est-prep.cost   = prep.cost                                                                                                              */
/*             est-prep.ml     = prep.ml                                                                                                                */
/*             est-prep.simon  = prep.simon                                                                                                             */
/*             est-prep.mkup   = prep.mkup                                                                                                              */
/*             est-prep.amtz   = prep.amtz                                                                                                              */
/*             est-prep.mat-type = prep.mat-type.                                                                                                       */
/*             if lookup(est-prep.mat-type, "p,f") gt 0 then                                                                                            */
/*                run sys/inc/flm-prep.p(recid(est), est-prep.s-num, output est-prep.qty).                                                              */
/*             i = i + 1.                                                                                                                               */
/* /*message "prep crt: " i est-prep.qty prep.mat-type view-as alert-box.                                                                               */
/* */                                                                                                                                                   */
/*   end.                                                                                                                                               */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createESTFarmOut B-table-Win 
PROCEDURE createESTFarmOut :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /* from crt-new-est procedure */
  DEF BUFFER bf-eb FOR eb.
  DEF VAR iCount AS INT NO-UNDO.
  DEF VAR iArtiosCount AS INT NO-UNDO.
  DEF VAR v-cust-no AS CHAR NO-UNDO.
  DEF VAR v-tmp LIKE eb.t-wid NO-UNDO.
  DEF VAR lv-layers AS DEC NO-UNDO.
  DEF BUFFER b-eb FOR eb.
DEF BUFFER b-ef FOR ef.
    DEF BUFFER bf-est FOR est.
  DEF BUFFER bf-est-qty FOR est-qty.
DEF VAR lv-comm LIKE eb.comm NO-UNDO.
  DEF VAR lv-sman LIKE sman.sman NO-UNDO.
  DEF VAR ld-markup AS DEC NO-UNDO.

  

  FOR EACH tt-frmout NO-LOCK:
      iArtiosCount = iArtiosCount + 1.
  END.
  
  ASSIGN
  ll-new-record = YES
  iCount = 0.
  FOR EACH tt-frmout NO-LOCK BREAK BY tt-frmout.form-no BY tt-frmout.blank-no:
     iCount = iCount + 1.
      
    IF iCount = 1 THEN DO:
        RUN est/NewEstimate.p ('C',
                               IF iArtiosCount = 1 THEN 5 ELSE 6,
                               OUTPUT lv-crt-est-rowid).
     END.
     
     FIND eb WHERE ROWID(eb) EQ lv-crt-est-rowid  NO-ERROR.
     FIND FIRST ef OF eb  NO-ERROR.
     FIND FIRST est OF ef  NO-ERROR.
     FIND est-qty WHERE est-qty.company = ef.company
                    AND est-qty.est-no = ef.est-no
                    AND est-qty.eqty = ef.eqty NO-ERROR.
     
     ASSIGN lv-eb-recid = RECID(eb)
            lv-ef-recid = RECID(ef)
            eb.part-no = tt-frmout.part-no
            eb.stack-code = tt-frmout.stack-no .

    /* if eb.tr-cnt = 0 then*/ eb.tr-cnt = eb.cas-cnt * eb.cas-pal.
     
     ASSIGN 
         eb.i-coldscr = tt-frmout.colr-dscr
         eb.tr-no  = tt-frmout.plat-cod 
         eb.adhesive  = tt-frmout.glu-cod
         eb.cas-no   = tt-frmout.bndl-cod 
         eb.cust-no = tt-frmout.cust-no
         eb.ship-id =  tt-frmout.ship-id
         eb.style = tt-frmout.style
         eb.flute = tt-frmout.flute
         eb.test =  tt-frmout.test
         eb.tab-in = tt-frmout.TAB  
         ef.board = tt-frmout.bord     
         eb.len =   tt-frmout.len
         eb.wid =  tt-frmout.wid
         eb.dep = tt-frmout.dep
         eb.procat =  tt-frmout.cat .
     
     IF AVAIL est-qty THEN
         ASSIGN eb.eqty = tt-frmout.quantity
                ef.eqty = tt-frmout.quantity
          est-qty.eqty = tt-frmout.quantity
          est-qty.qty[1] = tt-frmout.quantity
          est-qty.qty[2] = tt-frmout.copy-qty[2]
          est-qty.qty[3] = tt-frmout.copy-qty[3]
          est-qty.qty[4] = tt-frmout.copy-qty[4]
          est-qty.qty[5] = tt-frmout.copy-qty[5]
          est-qty.qty[6] = tt-frmout.copy-qty[6]
          est-qty.qty[7] = tt-frmout.copy-qty[7]
          est-qty.qty[8] = tt-frmout.copy-qty[8]
          est-qty.qty[9] = tt-frmout.copy-qty[9]
          est-qty.qty[10] = tt-frmout.copy-qty[10]
          est-qty.qty[11] = tt-frmout.copy-qty[11]
          est-qty.qty[12] = tt-frmout.copy-qty[12]
          est-qty.qty[13] = tt-frmout.copy-qty[13]
          est-qty.qty[14] = tt-frmout.copy-qty[14]
          est-qty.qty[15] = tt-frmout.copy-qty[15]
          est-qty.qty[16] = tt-frmout.copy-qty[16]
          est-qty.qty[17] = tt-frmout.copy-qty[17]
          est-qty.qty[18] = tt-frmout.copy-qty[18]
          est-qty.qty[19] = tt-frmout.copy-qty[19]
          est-qty.qty[20] = tt-frmout.copy-qty[20]
          
          est-qty.qty[21] = tt-frmout.copy-rel[1]
          est-qty.qty[22] = tt-frmout.copy-rel[2] 
          est-qty.qty[23] = tt-frmout.copy-rel[3] 
          est-qty.qty[24] = tt-frmout.copy-rel[4] 
          est-qty.qty[25] = tt-frmout.copy-rel[5] 
          est-qty.qty[26] = tt-frmout.copy-rel[6] 
          est-qty.qty[27] = tt-frmout.copy-rel[7] 
          est-qty.qty[28] = tt-frmout.copy-rel[8] 
          est-qty.qty[29] = tt-frmout.copy-rel[9] 
          est-qty.qty[30] = tt-frmout.copy-rel[10]
          est-qty.qty[31] = tt-frmout.copy-rel[11] 
          est-qty.qty[32] = tt-frmout.copy-rel[12] 
          est-qty.qty[33] = tt-frmout.copy-rel[13] 
          est-qty.qty[34] = tt-frmout.copy-rel[14] 
          est-qty.qty[35] = tt-frmout.copy-rel[15] 
          est-qty.qty[36] = tt-frmout.copy-rel[16] 
          est-qty.qty[37] = tt-frmout.copy-rel[17] 
          est-qty.qty[38] = tt-frmout.copy-rel[18] 
          est-qty.qty[39] = tt-frmout.copy-rel[19] 
          est-qty.qty[40] = tt-frmout.copy-rel[20]  .
     


     FIND FIRST itemfg WHERE  itemfg.company = cocode
         AND itemfg.stat = "A" AND itemfg.i-no = tt-frmout.stack-no
         AND ( itemfg.part-no = tt-frmout.part-no OR tt-frmout.part-no = "") NO-LOCK NO-ERROR .
     IF NOT AVAIL itemfg THEN
       FIND FIRST itemfg WHERE  itemfg.company = cocode
         AND itemfg.stat = "A" AND itemfg.i-no = tt-frmout.stack-no NO-LOCK NO-ERROR .

     IF AVAIL itemfg THEN
         ASSIGN
        eb.part-dscr1 = itemfg.i-name
        eb.stock-no   = CAPS(itemfg.i-no)
     /*  eb.style      = itemfg.style*/
        eb.procat     = itemfg.procat
                /*  eb.len        = (itemfg.l-score[50])
                  eb.wid        = (itemfg.w-score[50])
                  eb.dep        = (itemfg.d-score[50]) */ .
        eb.pur-man  = TRUE .

   /*  IF AVAIL itemfg AND eb.part-no = "" THEN
         ASSIGN eb.part-no    = itemfg.part-no. */

      /* convert to decimal from 1/16 */
       {sys/inc/k16bb.i eb.wid  } 
       {sys/inc/k16bb.i eb.len  } 
       {sys/inc/k16bb.i eb.dep  } 
        
        FIND FIRST cust WHERE
            cust.company = gcompany AND
            cust.cust-no = eb.cust-no
            NO-LOCK NO-ERROR.

     IF AVAIL cust THEN DO: 
        ASSIGN 
         /*   eb.cust-no = cust.cust-no*/
            eb.sman = cust.sman  .

        IF AVAIL cust AND cust.sman NE "" THEN DO:
          FIND sman WHERE sman.company = gcompany
                   AND sman.sman = cust.sman
                 NO-LOCK NO-ERROR.
          IF AVAIL sman THEN DO:
              ASSIGN
                  lv-sman = sman.sman.

                RELEASE bf-eb.
                IF eb.est-type EQ 6 THEN
                FIND FIRST bf-eb
                    WHERE bf-eb.company EQ eb.company
                      AND bf-eb.est-no  EQ eb.est-no
                      AND bf-eb.form-no EQ 0
                      AND bf-eb.procat  NE ""
                    NO-LOCK NO-ERROR.
           
                RUN ce/markup.p (eb.company, ROWID(eb), OUTPUT ld-markup).
           
                RUN sys/inc/getsmncm.p (eb.cust-no,
                                        INPUT-OUTPUT lv-sman,
                                        (IF AVAIL bf-eb THEN bf-eb.procat ELSE eb.procat),
                                        ld-markup,
                                        OUTPUT lv-comm).
           
                IF lv-comm NE 0 THEN eb.comm = (lv-comm).
          END.
        END.
            
        FIND FIRST shipto WHERE shipto.company EQ gcompany
            AND shipto.cust-no EQ cust.cust-no
            AND shipto.ship-id EQ tt-frmout.ship-id  NO-LOCK NO-ERROR.
     END.

      IF AVAIL shipto THEN
      ASSIGN
       eb.ship-id      = shipto.ship-id
       eb.ship-name    = shipto.ship-name
       eb.ship-addr[1] = shipto.ship-addr[1]
       eb.ship-addr[2] = shipto.ship-addr[2]
       eb.ship-city    = shipto.ship-city
       eb.ship-state   = shipto.ship-state
       eb.ship-zip     = shipto.ship-zip.

      IF AVAIL shipto AND shipto.spare-char-1 NE "" THEN DO:
          ASSIGN eb.sman = shipto.spare-char-1 .
          FIND sman WHERE sman.company = gcompany
                   AND sman.sman = shipto.spare-char-1
                 NO-LOCK NO-ERROR.
          eb.comm =  IF AVAIL sman THEN sman.scomm ELSE 0. 
      END.

      FIND item WHERE item.company = eb.company AND
                     item.i-no = eb.cas-no
              NO-LOCK NO-ERROR.
     IF AVAIL item THEN ASSIGN eb.cas-cnt = (item.box-case)
                              eb.cas-len = (item.case-l)
                              eb.cas-wid = (item.case-w)
                              eb.cas-dep = (item.case-d)
                              eb.cas-pal = (item.case-pall)
                              eb.cas-wt = (item.avg-w)         
                              .
     FIND FIRST item WHERE item.company = eb.company AND
                     item.i-no = eb.tr-no
              NO-LOCK NO-ERROR.
     IF AVAIL item THEN ASSIGN eb.tr-len = (item.case-l)
                               eb.tr-wid = (item.case-w)
                               eb.tr-dep = (item.case-d)
                               .
   
     FIND FIRST item NO-LOCK
            WHERE item.company EQ gcompany
              AND item.i-no    EQ ef.board
            NO-ERROR.
        IF AVAIL item THEN DO:
            ASSIGN ef.i-code = item.i-code
                  /*ef.flute = item.flute*/
                  ef.test = item.reg-no
                  ef.weight = item.basis-w.
            RUN sys/ref/uom-rm.p (item.mat-type, OUTPUT uom-list).
            IF uom-list NE "" THEN ef.cost-uom = ENTRY(1,uom-list).
            IF item.i-code = "R" THEN ASSIGN ef.lsh-len = item.s-len
                                             ef.lsh-wid = item.s-wid
                                             ef.gsh-wid = item.s-wid.
            IF item.r-wid <> 0 THEN ASSIGN ef.roll = TRUE
                                           ef.roll-wid = item.r-wid
                                           ef.lsh-wid = item.r-wid
                                           ef.gsh-wid = item.r-wid.  
           FIND FIRST e-item OF item NO-LOCK NO-ERROR.
           IF AVAIL e-item THEN ef.cost-uom = e-item.std-uom.                                        
        END.      
        FIND FIRST style WHERE style.company = est.company AND
                      style.style = eb.style
                      NO-LOCK NO-ERROR.
     IF AVAIL style THEN DO:
        ASSIGN eb.adhesive = style.material[7]
               eb.gluelap = style.dim-gl
               eb.fpanel = style.dim-pan5
               eb.lock = style.dim-fit
               eb.tuck = style.dim-tk.
        FIND FIRST ITEM WHERE ITEM.company = eb.company 
                          AND ITEM.i-no = eb.adhesive NO-LOCK NO-ERROR.
        IF AVAIL ITEM AND index("G,S,T",ITEM.mat-type) > 0 AND ITEM.i-no <> "No Joint"
        THEN eb.lin-in = eb.dep.
     END.  /* avail style */ 

      IF AVAIL itemfg AND itemfg.est-no NE "" THEN
    FIND FIRST b-eb
        WHERE b-eb.company  EQ cocode
          AND b-eb.est-no   EQ itemfg.est-no
          AND b-eb.stock-no EQ itemfg.i-no
          AND ROWID(b-eb)   NE ROWID(eb)
      USE-INDEX est-no NO-LOCK NO-ERROR.
  

  IF AVAIL b-eb THEN DO:
      ASSIGN
       eb.procat    = b-eb.procat
       eb.i-col    = (b-eb.i-col)
       eb.i-coat   = (b-eb.i-coat)       
       eb.pur-man  =  TRUE
       eb.eqty = b-eb.eqty
       ef.eqty = b-eb.eqty
       est-qty.eqty = b-eb.eqty 
       eb.i-pass   = (b-eb.i-pass)
       eb.i-coat-p = (b-eb.i-coat-p)   
      . 
        

           FIND FIRST b-ef
          WHERE b-ef.company EQ b-eb.company
            AND b-ef.est-no  EQ b-eb.est-no
            AND b-ef.form-no EQ b-eb.form-no
          NO-LOCK NO-ERROR.
      IF AVAIL b-ef THEN
        ASSIGN
       /*  ef.board  = b-ef.board */
         ef.cal    = (b-ef.cal).

  END.

  FIND xest WHERE ROWID(xest) EQ ROWID(est) NO-LOCK NO-ERROR.
  FIND xef WHERE ROWID(xef)   EQ ROWID(ef)  NO-LOCK NO-ERROR.
  FIND xeb WHERE RECID(xeb) = recid(eb) NO-LOCK NO-ERROR.

  RUN calc-pass.

  /*RUN calc-layout (YES).*/
  
  RUN calc-blank-size.
  
  ASSIGN 
      ef.gsh-wid = eb.t-wid
      ef.gsh-len = eb.t-len
      ef.nsh-wid = eb.t-wid
      ef.nsh-len = eb.t-len
      ef.trim-w = eb.t-wid
      ef.trim-l = eb.t-len .

  /* ink/pack copied from cec/estitm1.i */
     IF eb.stock-no = "" THEN DO:
        FIND FIRST ce-ctrl WHERE ce-ctrl.company = gcompany AND
                                 ce-ctrl.loc = gloc
                                 NO-LOCK NO-ERROR.
        ASSIGN eb.cas-no = ce-ctrl.def-case
               eb.tr-no = ce-ctrl.def-pal.      
     END.
     FIND FIRST cust WHERE cust.company = gcompany AND
                     cust.cust-no = eb.cust-no
                     NO-LOCK NO-ERROR.
     ASSIGN     
       eb.tr-no = IF AVAIL shipto AND shipto.pallet <> "" THEN shipto.pallet ELSE IF AVAIL cust AND cust.pallet <> "" THEN cust.pallet ELSE eb.tr-no.
       
     RUN est/packCodeOverride.p (INPUT eb.company, eb.cust-no, eb.style, OUTPUT cPackCodeOverride).
     IF cPackCodeOverride GT "" THEN 
       eb.cas-no = cPackCodeOverride.             
     FIND item WHERE item.company = eb.company AND
                     item.i-no = eb.cas-no
              NO-LOCK NO-ERROR.
     IF AVAIL item THEN ASSIGN eb.cas-cnt = (item.box-case)
                              eb.cas-len = (item.case-l)
                              eb.cas-wid = (item.case-w)
                              eb.cas-dep = (item.case-d)
                              eb.cas-pal = (item.case-pall)
                              eb.cas-wt = (item.avg-w)         
                              .
     FIND FIRST item WHERE item.company = eb.company AND
                     item.i-no = eb.tr-no
              NO-LOCK NO-ERROR.
     IF AVAIL item THEN ASSIGN eb.tr-len = (item.case-l)
                               eb.tr-wid = (item.case-w)
                               eb.tr-dep = (item.case-d)
                               .
     DEF VAR lv-cas-pal AS DEC NO-UNDO.
     DEF VAR lv-tr-cnt AS INT NO-UNDO.
     DEF VAR lv-numstack AS INT NO-UNDO.
     DEF VAR lv-stackcode AS cha NO-UNDO.
     DEF VAR lv-error AS LOG NO-UNDO.
     RUN cec/kpallet.p (RECID(xeb), OUTPUT lv-cas-pal, OUTPUT lv-tr-cnt,
                        OUTPUT lv-numstack, OUTPUT lv-stackcode, OUTPUT lv-error).

     IF lv-error THEN DO:
/*         message "An error occured while attempting to calculate the number of pallets. " */
/*                 skip                                                                     */
/*                 "Please review any previous error messages for more information."        */
/*                  view-as alert-box error.                                                */
     END.
     ELSE DO:
       lv-layers = lv-cas-pal / lv-numstack.
       {sys/inc/roundup.i lv-layers}

       ASSIGN
        eb.cas-pal    = lv-cas-pal
        eb.tr-cnt     = lv-tr-cnt
        eb.tr-cas     = lv-layers
        eb.stacks     = lv-numstack
        eb.stack-code = lv-stackcode.
     END.


  RUN cec/mach-seq.p (ef.form-no, est-qty.eqty, NO).

     RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-source",OUTPUT char-hdl).
     IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
     RUN init-box-design IN WIDGET-HANDLE(char-hdl) (THIS-PROCEDURE).

     RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"box-calc-target",OUTPUT char-hdl).
     IF VALID-HANDLE(WIDGET-HANDLE(ENTRY(1,char-hdl))) THEN
     RUN build-box IN WIDGET-HANDLE(ENTRY(1,char-hdl)) ("B").

     FIND FIRST box-design-hdr WHERE
          box-design-hdr.design-no EQ 0 AND
          box-design-hdr.company EQ cocode AND
          box-design-hdr.est-no EQ est.est-no AND
          box-design-hdr.form-no EQ tt-artios.form-num AND
          box-design-hdr.blank-no EQ tt-artios.blank-num
          NO-ERROR.
      
        IF AVAIL box-design-hdr THEN
        DO:
           box-design-hdr.box-image = tt-artios.DesignImg.
           FIND CURRENT box-design-hdr NO-LOCK NO-ERROR.
        END. 

  END.  /* for each tt-frmout */

  IF iCount > 0 THEN DO:
     RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
     RUN new_record IN WIDGET-HANDLE(char-hdl)  (lv-crt-est-rowid).
  END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createESTfromArtios B-table-Win 
PROCEDURE createESTfromArtios :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /* from crt-new-est procedure */
  DEF BUFFER bf-eb FOR eb.
  DEF VAR iCount AS INT NO-UNDO.
  DEF VAR iArtiosCount AS INT NO-UNDO.
  DEF VAR v-cust-no AS CHAR NO-UNDO.
  DEF VAR v-tmp LIKE eb.t-wid NO-UNDO.
  DEF VAR lv-layers AS DEC NO-UNDO.
  DEFINE VARIABLE cPackCodeOverride AS CHARACTER NO-UNDO.
  FOR EACH tt-artios NO-LOCK:
      iArtiosCount = iArtiosCount + 1.
  END.
  
  ASSIGN
  ll-new-record = YES
  iCount = 0.
  FOR EACH tt-artios NO-LOCK BREAK BY tt-artios.form-num BY tt-artios.blank-num:
     iCount = iCount + 1.
      
     IF iCount = 1 THEN DO:
        RUN est/NewEstimate.p ('C',
                               IF iArtiosCount = 1 THEN 5 ELSE 6,
                               OUTPUT lv-crt-est-rowid).
     END.
     ELSE DO:
        
        IF FIRST-OF(tt-artios.form-num) THEN DO:
           FIND eb WHERE ROWID(eb) EQ lv-crt-est-rowid NO-LOCK NO-ERROR.
           FIND FIRST est OF ef NO-LOCK NO-ERROR.

           RUN est/NewEstimateForm.p ('C', ROWID(est), OUTPUT lv-crt-est-rowid).
           
        END.
        ELSE IF FIRST-OF(tt-artios.blank-num) THEN DO:
            FIND eb WHERE ROWID(eb) EQ lv-crt-est-rowid NO-LOCK NO-ERROR.
            FIND FIRST ef OF eb NO-LOCK NO-ERROR.
 
            RUN cec/newblank.p (ROWID(ef), OUTPUT lv-crt-est-rowid).
            
        END.
     END.
     
     FIND eb WHERE ROWID(eb) EQ lv-crt-est-rowid  NO-ERROR.
     FIND FIRST ef OF eb  NO-ERROR.
     FIND FIRST est OF ef  NO-ERROR.
     FIND est-qty WHERE est-qty.company = ef.company
                    AND est-qty.est-no = ef.est-no
                    AND est-qty.eqty = ef.eqty NO-ERROR.
     
     ASSIGN lv-eb-recid = RECID(eb)
            lv-ef-recid = RECID(ef)
            eb.cad-no = tt-artios.cadnum 
            eb.cust-no = tt-artios.cust-no
            eb.sman    = tt-artios.sman
            eb.part-no = tt-artios.partnum + "-" + string(tt-artios.blank-num,"99")
            eb.part-dscr1 = tt-artios.partname
            eb.style = tt-artios.style
            ef.board = tt-artios.board
            eb.eqty = tt-artios.setqty
            ef.eqty = tt-artios.setqty
            est-qty.eqty = tt-artios.setqty   
            eb.procat = tt-artios.procat
            eb.quantityPerSet = tt-artios.CompQty   
            eb.die-no = tt-artios.dienum
            ef.die-in = tt-artios.die-in
            est.est-qty[1] = tt-artios.setqty  /* request qty */
            eb.die-in = tt-artios.die-in
            ef.blank-qty = tt-artios.NumOfComponents
            ef.trim-w = tt-artios.t-wid
            ef.trim-l = tt-artios.t-len
            eb.num-up  = tt-artios.CompNumUp
            eb.num-wid = 1
            eb.num-len = tt-artios.CompNumUp
            eb.pur-man = tt-artios.pur-man
            ef.nc = NOT tt-artios.pur-man
            eb.spare-char-1 = tt-artios.DesignerName.

     RUN set-lv-foam.
     IF LOOKUP(eb.style,"5PF,5PFN,COW,FOL,FTB,FTT,HSC,RSC,RSCFOL") GT 0 THEN
        ASSIGN
           eb.len = IF tt-artios.len > 0 THEN tt-artios.len ELSE 0
           eb.wid = IF tt-artios.wid > 0 THEN tt-artios.wid ELSE 0
           eb.dep = IF tt-artios.dep > 0 THEN tt-artios.dep ELSE 0.
           
     ELSE
        eb.dep = 0.

     IF eb.len = 0 AND tt-artios.t-len <> 0 THEN eb.len = tt-artios.t-len * (tt-artios.ratio / 100)  / tt-artios.CompNumUp.           
     IF eb.wid = 0 AND tt-artios.t-wid <> 0 THEN eb.wid = tt-artios.t-wid * tt-artios.ratio / 100.   
     IF eb.tr-cnt = 0 THEN eb.tr-cnt = eb.cas-cnt * eb.cas-pal.
               
     IF tt-artios.grain EQ "Horizontal" THEN
        ASSIGN
           v-tmp = eb.len
           eb.len = eb.wid
           eb.wid = v-tmp.

     v-cust-no = IF tt-artios.cust-no <> "" THEN tt-artios.cust-no
                 ELSE tt-artios.custname.

     FIND FIRST cust WHERE
          cust.company = gcompany AND
          cust.cust-no = v-cust-no
          NO-LOCK NO-ERROR.

     IF AVAIL cust THEN DO: 
        ASSIGN eb.cust-no = cust.cust-no.
        FIND FIRST shipto OF cust NO-LOCK NO-ERROR.
        IF AVAIL shipto THEN ASSIGN eb.ship-id = shipto.ship-id.

     END.

     FIND FIRST item NO-LOCK WHERE
          item.company EQ gcompany AND
          item.i-no  = tt-artios.board
          NO-ERROR.

     IF AVAIL item THEN
        ASSIGN ef.board = item.i-no
               ef.cal = item.cal
               eb.flute = ITEM.flute
              eb.test = ITEM.reg-no.

     RUN calc-blank-size.
     RUN calc-layout4Artios (YES).

     /* create default prep */
     RUN create-prep. 

     /* ink/pack copied from cec/estitm1.i */
     IF eb.stock-no = "" THEN DO:
        FIND FIRST ce-ctrl WHERE ce-ctrl.company = gcompany AND
                                 ce-ctrl.loc = gloc
                                 NO-LOCK NO-ERROR.
        ASSIGN eb.cas-no = ce-ctrl.def-case
               eb.tr-no = ce-ctrl.def-pal.      
     END.
     FIND FIRST cust WHERE cust.company = gcompany AND
                     cust.cust-no = eb.cust-no
                     NO-LOCK NO-ERROR.
     ASSIGN     
       eb.tr-no = IF AVAIL shipto AND shipto.pallet <> "" THEN shipto.pallet ELSE IF AVAIL cust AND cust.pallet <> "" THEN cust.pallet ELSE eb.tr-no.
     
     RUN est/packCodeOverride.p (INPUT eb.company, eb.cust-no, eb.style, OUTPUT cPackCodeOverride).
     IF cPackCodeOverride GT "" THEN 
         eb.cas-no = cPackCodeOverride.           
     FIND item WHERE item.company = eb.company AND
                     item.i-no = eb.cas-no
              NO-LOCK NO-ERROR.
     IF AVAIL item THEN ASSIGN eb.cas-cnt = (item.box-case)
                              eb.cas-len = (item.case-l)
                              eb.cas-wid = (item.case-w)
                              eb.cas-dep = (item.case-d)
                              eb.cas-pal = (item.case-pall)
                              eb.cas-wt = (item.avg-w)         
                              .
     FIND FIRST item WHERE item.company = eb.company AND
                     item.i-no = eb.tr-no
              NO-LOCK NO-ERROR.
     IF AVAIL item THEN ASSIGN eb.tr-len = (item.case-l)
                               eb.tr-wid = (item.case-w)
                               eb.tr-dep = (item.case-d)
                               .
     DEF VAR lv-cas-pal AS DEC NO-UNDO.
     DEF VAR lv-tr-cnt AS INT NO-UNDO.
     DEF VAR lv-numstack AS INT NO-UNDO.
     DEF VAR lv-stackcode AS cha NO-UNDO.
     DEF VAR lv-error AS LOG NO-UNDO.
     RUN cec/kpallet.p (RECID(xeb), OUTPUT lv-cas-pal, OUTPUT lv-tr-cnt,
                        OUTPUT lv-numstack, OUTPUT lv-stackcode, OUTPUT lv-error).

     IF lv-error THEN DO:
/*         message "An error occured while attempting to calculate the number of pallets. " */
/*                 skip                                                                     */
/*                 "Please review any previous error messages for more information."        */
/*                  view-as alert-box error.                                                */
     END.
     ELSE DO:
       lv-layers = lv-cas-pal / lv-numstack.
       {sys/inc/roundup.i lv-layers}

       ASSIGN
        eb.cas-pal    = lv-cas-pal
        eb.tr-cnt     = lv-tr-cnt
        eb.tr-cas     = lv-layers
        eb.stacks     = lv-numstack
        eb.stack-code = lv-stackcode.
     END.

     /* create set header record */
     IF iArtiosCount > 1 THEN DO:
        FIND bf-eb WHERE bf-eb.company = est.company
                          AND bf-eb.est-no = est.est-no
                          AND bf-eb.form-no = 0 NO-ERROR.
        IF NOT AVAIL bf-eb THEN CREATE bf-eb.
        ASSIGN bf-eb.company = est.company
                  bf-eb.est-no = est.est-no
                  bf-eb.form-no = 0
                  bf-eb.part-no = SUBSTRING(tt-artios.cadnum,1,6)
                  /*bf-eb.part-dscr1 =*/
                  bf-eb.procat =   tt-artios.procat
                  bf-eb.est-type = est.est-type
                  bf-eb.eqty = ef.eqty
                  bf-eb.blank-no = 0
                  bf-eb.set-is-assembled = IF v-alloc THEN NO /*assembled */ 
                                           ELSE IF NOT v-alloc THEN YES  /* Unassembled */ 
                                           ELSE ? 
                  bf-eb.pur-man = IF v-alloc THEN NO ELSE YES 
                  .
    /* ???FIND FIRST itemfg
             WHERE itemfg.company EQ eb.company
               AND itemfg.i-no    EQ eb.stock-no
             NO-ERROR.
         IF AVAIL itemfg THEN
           ASSIGN
            itemfg.alloc 
     */

     END.

     RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-source",OUTPUT char-hdl).
     IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
     RUN init-box-design IN WIDGET-HANDLE(char-hdl) (THIS-PROCEDURE).

     RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"box-calc-target",OUTPUT char-hdl).
     IF VALID-HANDLE(WIDGET-HANDLE(ENTRY(1,char-hdl))) THEN
     RUN build-box IN WIDGET-HANDLE(ENTRY(1,char-hdl)) ("B").

     FIND FIRST box-design-hdr WHERE
          box-design-hdr.design-no EQ 0 AND
          box-design-hdr.company EQ cocode AND
          box-design-hdr.est-no EQ est.est-no AND
          box-design-hdr.form-no EQ tt-artios.form-num AND
          box-design-hdr.blank-no EQ tt-artios.blank-num
          NO-ERROR.
      
        IF AVAIL box-design-hdr THEN
        DO:
           box-design-hdr.box-image = tt-artios.DesignImg.
           FIND CURRENT box-design-hdr NO-LOCK NO-ERROR.
        END.
  END.  /* for each tt-artios */

  IF iCount > 0 THEN DO:
     RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
     RUN new_record IN WIDGET-HANDLE(char-hdl)  (lv-crt-est-rowid).
  END. 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createEstFromImpact B-table-Win 
PROCEDURE createEstFromImpact :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /* from crt-new-est procedure */
  DEF BUFFER bf-eb FOR eb.
  DEF VAR iCount AS INT NO-UNDO.
  DEF VAR iArtiosCount AS INT NO-UNDO.
  DEF VAR v-cust-no AS CHAR NO-UNDO.
  DEF VAR v-tmp LIKE eb.t-wid NO-UNDO.
  DEF VAR lv-layers AS DEC NO-UNDO.
  DEFINE VARIABLE cPackCodeOverride AS CHARACTER NO-UNDO.
  FOR EACH tt-artios NO-LOCK:
      iArtiosCount = iArtiosCount + 1.
  END.
  
  ASSIGN
  ll-new-record = YES
  iCount = 0.
  FOR EACH tt-artios NO-LOCK BREAK BY tt-artios.form-num BY tt-artios.blank-num:
     iCount = iCount + 1.
      
     IF iCount = 1 THEN DO:
        RUN est/NewEstimate.p ('C',
                               IF iArtiosCount = 1 THEN 5 ELSE 6,
                               OUTPUT lv-crt-est-rowid).
     END.
     ELSE DO:
        
        IF FIRST-OF(tt-artios.form-num) THEN DO:
           FIND eb WHERE ROWID(eb) EQ lv-crt-est-rowid NO-LOCK NO-ERROR.
           FIND FIRST est OF ef NO-LOCK NO-ERROR.

           RUN est/NewEstimateForm.p ('C', ROWID(est), OUTPUT lv-crt-est-rowid).
           
        END.
        ELSE IF FIRST-OF(tt-artios.blank-num) THEN DO:
            FIND eb WHERE ROWID(eb) EQ lv-crt-est-rowid NO-LOCK NO-ERROR.
            FIND FIRST ef OF eb NO-LOCK NO-ERROR.
 
            RUN cec/newblank.p (ROWID(ef), OUTPUT lv-crt-est-rowid).
            
        END.
     END.
     
     FIND eb WHERE ROWID(eb) EQ lv-crt-est-rowid  NO-ERROR.
     FIND FIRST ef OF eb  NO-ERROR.
     FIND FIRST est OF ef  NO-ERROR.
     FIND est-qty WHERE est-qty.company = ef.company
                    AND est-qty.est-no = ef.est-no
                    AND est-qty.eqty = ef.eqty NO-ERROR.
     
     ASSIGN lv-eb-recid = RECID(eb)
            lv-ef-recid = RECID(ef)
            eb.cad-no = tt-artios.cadnum 
            eb.cust-no = tt-artios.cust-no
            eb.sman    = tt-artios.sman
            eb.part-no = tt-artios.partnum + "-" + string(tt-artios.blank-num,"99")
            eb.part-dscr1 = tt-artios.partname
            eb.style = tt-artios.style
            ef.board = tt-artios.board
            eb.eqty = tt-artios.setqty
            ef.eqty = tt-artios.setqty
            est-qty.eqty = tt-artios.setqty   
            eb.procat = tt-artios.procat
            eb.quantityPerSet = tt-artios.CompQty   
            eb.die-no = tt-artios.dienum
            ef.die-in = tt-artios.die-in
            est.est-qty[1] = tt-artios.setqty  /* request qty */
            eb.die-in = tt-artios.die-in
            ef.blank-qty = tt-artios.NumOfComponents
            ef.trim-w = tt-artios.t-wid
            ef.trim-l = tt-artios.t-len
            eb.num-up  = tt-artios.CompNumUp
            eb.num-wid = 1
            eb.num-len = tt-artios.CompNumUp
            eb.pur-man = tt-artios.pur-man
            ef.nc = NOT tt-artios.pur-man
            eb.spare-char-1 = tt-artios.DesignerName.

     RUN set-lv-foam.
     /*
     IF LOOKUP(eb.style,"5PF,5PFN,COW,FOL,FTB,FTT,HSC,RSC,RSCFOL") GT 0 THEN
        ASSIGN
           eb.len = if tt-artios.len > 0 then tt-artios.len else 0
           eb.wid = if tt-artios.wid > 0 then tt-artios.wid else 0
           eb.dep = if tt-artios.dep > 0 then tt-artios.dep else 0.
           
     ELSE
        eb.dep = 0.
     */
     ASSIGN
           eb.len = IF tt-artios.len > 0 THEN tt-artios.len ELSE 0
           eb.wid = IF tt-artios.wid > 0 THEN tt-artios.wid ELSE 0
           eb.dep = IF tt-artios.dep > 0 THEN tt-artios.dep ELSE 0.

     IF eb.len = 0 AND tt-artios.t-len <> 0 THEN eb.len = tt-artios.t-len * (tt-artios.ratio / 100)  / tt-artios.CompNumUp.           
     IF eb.wid = 0 AND tt-artios.t-wid <> 0 THEN eb.wid = tt-artios.t-wid * tt-artios.ratio / 100.   
     IF eb.tr-cnt = 0 THEN eb.tr-cnt = eb.cas-cnt * eb.cas-pal.
               
     IF tt-artios.grain EQ "Horizontal" THEN
        ASSIGN
           v-tmp = eb.len
           eb.len = eb.wid
           eb.wid = v-tmp.

     v-cust-no = IF tt-artios.cust-no <> "" THEN tt-artios.cust-no
                 ELSE tt-artios.custname.

     FIND FIRST cust WHERE
          cust.company = gcompany AND
          cust.cust-no = v-cust-no
          NO-LOCK NO-ERROR.

     IF AVAIL cust THEN DO: 
        ASSIGN eb.cust-no = cust.cust-no.
        FIND FIRST shipto OF cust NO-LOCK NO-ERROR.
        IF AVAIL shipto THEN ASSIGN eb.ship-id = shipto.ship-id.

     END.

     FIND FIRST item NO-LOCK WHERE
          item.company EQ gcompany AND
          item.i-no  = tt-artios.board
          NO-ERROR.

     IF AVAIL item THEN
        ASSIGN ef.board = item.i-no
               ef.cal = item.cal
               eb.flute = ITEM.flute
               eb.test = ITEM.reg-no.

     RUN calc-blank-size.
     RUN calc-layout4Artios (YES).

     /* create default prep */
     RUN create-prep. 

     /* ink/pack copied from cec/estitm1.i */
     IF eb.stock-no = "" THEN DO:
        FIND FIRST ce-ctrl WHERE ce-ctrl.company = gcompany AND
                                 ce-ctrl.loc = gloc
                                 NO-LOCK NO-ERROR.
        ASSIGN eb.cas-no = ce-ctrl.def-case
               eb.tr-no = ce-ctrl.def-pal.      
     END.
     FIND FIRST cust WHERE cust.company = gcompany AND
                     cust.cust-no = eb.cust-no
                     NO-LOCK NO-ERROR.
     ASSIGN     
     eb.tr-no = IF AVAIL shipto AND shipto.pallet <> "" THEN shipto.pallet ELSE IF AVAIL cust AND cust.pallet <> "" THEN cust.pallet ELSE eb.tr-no.
      
     RUN est/packCodeOverride.p (INPUT eb.company, eb.cust-no, eb.style, OUTPUT cPackCodeOverride).
     IF cPackCodeOverride GT "" THEN 
         eb.cas-no = cPackCodeOverride.
                     
     FIND item WHERE item.company = eb.company AND
                     item.i-no = eb.cas-no
              NO-LOCK NO-ERROR.
     IF AVAIL item THEN ASSIGN eb.cas-cnt = (item.box-case)
                              eb.cas-len = (item.case-l)
                              eb.cas-wid = (item.case-w)
                              eb.cas-dep = (item.case-d)
                              eb.cas-pal = (item.case-pall)
                              eb.cas-wt = (item.avg-w)         
                              .
     FIND FIRST item WHERE item.company = eb.company AND
                     item.i-no = eb.tr-no
              NO-LOCK NO-ERROR.
     IF AVAIL item THEN ASSIGN eb.tr-len = (item.case-l)
                               eb.tr-wid = (item.case-w)
                               eb.tr-dep = (item.case-d)
                               .
     DEF VAR lv-cas-pal AS DEC NO-UNDO.
     DEF VAR lv-tr-cnt AS INT NO-UNDO.
     DEF VAR lv-numstack AS INT NO-UNDO.
     DEF VAR lv-stackcode AS cha NO-UNDO.
     DEF VAR lv-error AS LOG NO-UNDO.
     RUN cec/kpallet.p (RECID(xeb), OUTPUT lv-cas-pal, OUTPUT lv-tr-cnt,
                        OUTPUT lv-numstack, OUTPUT lv-stackcode, OUTPUT lv-error).

     IF lv-error THEN DO:
/*         message "An error occured while attempting to calculate the number of pallets. " */
/*                 skip                                                                     */
/*                 "Please review any previous error messages for more information."        */
/*                  view-as alert-box error.                                                */
     END.
     ELSE DO:
       lv-layers = lv-cas-pal / lv-numstack.
       {sys/inc/roundup.i lv-layers}

       ASSIGN
        eb.cas-pal    = lv-cas-pal
        eb.tr-cnt     = lv-tr-cnt
        eb.tr-cas     = lv-layers
        eb.stacks     = lv-numstack
        eb.stack-code = lv-stackcode.
     END.

     RUN cec/mach-seq.p (ef.form-no, est-qty.eqty, NO).

     /* create set header record */
     IF iArtiosCount > 1 THEN DO:
        FIND bf-eb WHERE bf-eb.company = est.company
                          AND bf-eb.est-no = est.est-no
                          AND bf-eb.form-no = 0 NO-ERROR.
        IF NOT AVAIL bf-eb THEN CREATE bf-eb.
        ASSIGN bf-eb.company = est.company
                  bf-eb.est-no = est.est-no
                  bf-eb.form-no = 0
                  bf-eb.part-no = SUBSTRING(tt-artios.cadnum,1,6)
                  /*bf-eb.part-dscr1 =*/
                  bf-eb.procat =   tt-artios.procat
                  bf-eb.est-type = est.est-type
                  bf-eb.eqty = ef.eqty
                  bf-eb.blank-no = 0
                  bf-eb.set-is-assembled = IF v-alloc THEN NO /*assembled */ 
                                           ELSE IF NOT v-alloc THEN YES  /* Unassembled */ 
                                           ELSE ? 
                  bf-eb.pur-man = IF v-alloc THEN NO ELSE YES 
                  .
    /* ???FIND FIRST itemfg
             WHERE itemfg.company EQ eb.company
               AND itemfg.i-no    EQ eb.stock-no
             NO-ERROR.
         IF AVAIL itemfg THEN
           ASSIGN
            itemfg.alloc 
     */

     END.

     RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-source",OUTPUT char-hdl).
     IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
     RUN init-box-design IN WIDGET-HANDLE(char-hdl) (THIS-PROCEDURE).

     RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"box-calc-target",OUTPUT char-hdl).
     IF VALID-HANDLE(WIDGET-HANDLE(ENTRY(1,char-hdl))) THEN
     RUN build-box IN WIDGET-HANDLE(ENTRY(1,char-hdl)) ("B").

     FIND FIRST box-design-hdr WHERE
          box-design-hdr.design-no EQ 0 AND
          box-design-hdr.company EQ cocode AND
          box-design-hdr.est-no EQ est.est-no AND
          box-design-hdr.form-no EQ tt-artios.form-num AND
          box-design-hdr.blank-no EQ tt-artios.blank-num
          NO-ERROR.
      
        IF AVAIL box-design-hdr THEN
        DO:
           box-design-hdr.box-image = tt-artios.DesignImg.
           FIND CURRENT box-design-hdr NO-LOCK NO-ERROR.
        END.
  END.  /* for each tt-artios */

  IF iCount > 0 THEN DO:
     RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
     RUN new_record IN WIDGET-HANDLE(char-hdl)  (lv-crt-est-rowid).
  END. 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crt-est-childrecord B-table-Win 
PROCEDURE crt-est-childrecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rowid AS ROWID NO-UNDO.
 

  RUN ce/new-form.p (ROWID(est), OUTPUT lv-rowid).

  FIND eb WHERE ROWID(eb) EQ lv-rowid NO-LOCK NO-ERROR.
  lv-eb-recid = RECID(eb).
  FIND FIRST ef OF eb NO-LOCK NO-ERROR.
  lv-ef-recid = RECID(ef).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crt-itemfg B-table-Win 
PROCEDURE crt-itemfg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* -------------------------------------------------- fg/ce-addfg.p 08/98 JLF */
/* Add FG thru estimating                                                     */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAMETER v-item LIKE itemfg.i-no.

DEF BUFFER b-eb FOR eb.

DEF VAR cocode AS cha NO-UNDO.
DEF VAR locode AS cha NO-UNDO.
DEF VAR tmpstore AS cha NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR xeb-rowid AS ROWID NO-UNDO.


ASSIGN cocode = gcompany
       locode = gloc
       xeb-rowid = ROWID(xeb).

{ce/msfcalc.i}
{oe/fgfreight.i}
FIND FIRST cust  WHERE cust.company EQ gcompany
                   AND cust.cust-no EQ xeb.cust-no
    NO-LOCK NO-ERROR.

IF (xest.est-type EQ 2 OR xest.est-type EQ 6)       AND
   CAN-FIND(b-eb OF xest WHERE b-eb.form-no NE 0
                           AND b-eb.quantityPerSet GE 2)   AND
   CAN-FIND(b-eb OF xest WHERE b-eb.form-no  EQ 0
                           AND b-eb.stock-no EQ "") THEN DO:
  FIND FIRST xeb OF xest WHERE xeb.form-no EQ 0.
  xeb.stock-no = v-item.
END.

CREATE itemfg.
ASSIGN
 itemfg.company    = gcompany
 itemfg.loc        = gloc
 itemfg.i-no       = v-item
 itemfg.i-code     = "C"
 itemfg.i-name     = xeb.part-dscr1
 itemfg.part-dscr1 = xeb.part-dscr2
 itemfg.sell-uom   = "M"
 itemfg.part-no    = xeb.part-no
 itemfg.cust-no    = xeb.cust-no
 itemfg.cust-name  = IF AVAIL cust THEN cust.name ELSE ""
 itemfg.pur-uom    = IF xeb.pur-man THEN "EA" ELSE "M"
 itemfg.prod-uom   = IF xeb.pur-man THEN "EA" ELSE "M"
 itemfg.stocked    = YES
 itemfg.die-no     = xeb.die-no
 itemfg.plate-no   = xeb.plate-no
 itemfg.style      = xeb.style
 itemfg.procat     = xeb.procat
 itemfg.cad-no     = xeb.cad-no
 itemfg.upc-no     = xeb.upc-no
 itemfg.spc-no     = xeb.spc-no
 itemfg.isaset     = (xest.est-type EQ 2 OR xest.est-type EQ 6) AND
                     xeb.form-no EQ 0
 itemfg.pur-man    = xeb.pur-man 
 itemfg.alloc      = xeb.set-is-assembled
 itemfg.setupDate  = TODAY.
 RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT "").

 {oe/fgfreighta.i xeb}

 /* gdm - 11190901 */
 IF xeb.ship-id NE "" THEN DO:
  FIND FIRST shipto NO-LOCK
    WHERE shipto.company EQ xeb.company
      AND shipto.cust-no EQ xeb.cust-no
      AND shipto.ship-id EQ xeb.ship-id NO-ERROR.
  IF AVAIL shipto THEN ASSIGN itemfg.ship-meth = shipto.ship-meth.
 END.
 /* gdm - 11190901 end */

 IF itemfg.alloc NE ? THEN itemfg.alloc = NOT itemfg.alloc.

IF v-graphic-char NE "" THEN 
DO:
   IF LOOKUP(SUBSTR(v-graphic-char,LENGTH(v-graphic-char)),"\,/") EQ 0 THEN
      v-graphic-char = v-graphic-char + "\".

   IF SEARCH(v-graphic-char + itemfg.i-no + ".jpg") NE ? THEN
      itemfg.box-image = v-graphic-char + itemfg.i-no + ".jpg".
END.

IF NOT ll-new-record THEN DO:
  {fg/set-inks1.i itemfg xeb}
END.
 
{sys/inc/fgcascnt.i itemfg xeb}

{sys/inc/updfgdim.i "xeb"}
   
/* update taxable info to itemfg */
 {est/fgupdtax.i xeb}
    
FIND FIRST xeb WHERE ROWID(xeb) EQ xeb-rowid.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crt-new-est B-table-Win 
PROCEDURE crt-new-est :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ll-new-record = YES.
  
  RUN est/NewEstimate.p ('C',
                         IF ls-add-what EQ "est" THEN 5 ELSE 6,
                         OUTPUT lv-crt-est-rowid).

  FIND eb WHERE ROWID(eb) EQ lv-crt-est-rowid NO-LOCK NO-ERROR.
  FIND FIRST ef OF eb NO-LOCK NO-ERROR.
  FIND FIRST est OF ef NO-LOCK NO-ERROR.
  ASSIGN
    lv-eb-recid = RECID(eb)
    lv-ef-recid = RECID(ef).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crt-new-set B-table-Win 
PROCEDURE crt-new-set :
/*------------------------------------------------------------------------------
  Purpose:  new item for set   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rowid AS ROWID NO-UNDO.

  ll-new-record = YES.

  RUN set-or-combo.

  IF ls-add-what EQ "form" THEN
     RUN est/NewEstimateForm.p ('C', ROWID(est), OUTPUT lv-rowid).
  ELSE
     RUN cec/newblank.p (ROWID(ef), OUTPUT lv-rowid).

  IF ll-add-set-part-2 THEN
  DO:
     FIND eb WHERE ROWID(eb) EQ lv-rowid NO-ERROR.
    
     IF ls-add-what EQ "form" THEN
     DO:
        FIND FIRST ef WHERE
             ef.company EQ eb.company AND
             ef.est-no EQ eb.est-no AND
             ef.form-no EQ 2
             NO-ERROR.

        IF AVAIL ef THEN
        DO:
           ef.board = v-board2.

           FIND FIRST item WHERE
                item.company EQ ef.company AND
                item.i-no    EQ ef.board
                NO-LOCK NO-ERROR.
           IF AVAIL item THEN
           DO:
              ASSIGN
                 ef.cal = item.cal
                 eb.flute = ITEM.flute
                 eb.test = ITEM.reg-no.

              FIND CURRENT ITEM NO-LOCK NO-ERROR.
              RELEASE ITEM.
           END.

           FIND CURRENT ef NO-LOCK NO-ERROR.
        END.
     END.
    
     ASSIGN
        eb.part-no    = v-part-no2
        eb.part-dscr1 = v-part-desc3
        eb.part-dscr2 = v-part-desc4
        eb.style = v-style2
        eb.stock-no = v-stock-no2
        eb.quantityPerSet = v-qty-set-2
        eb.len = v-wid-2
        eb.wid = v-height-2.
     RUN set-lv-foam.
  END.

  FIND eb WHERE ROWID(eb) EQ lv-rowid NO-LOCK NO-ERROR.
  
  FIND FIRST ef OF eb NO-LOCK NO-ERROR.
  ASSIGN
     lv-eb-recid = RECID(eb)
     lv-ef-recid = RECID(ef).

  DISPLAY est.est-no est-qty.eqty
          eb.style eb.part-dscr1
          eb.cust-no eb.ship-id eb.part-no eb.tab-in 
          eb.len eb.wid eb.dep eb.procat
          eb.quantityPerSet eb.flute ef.board eb.test
      WITH BROWSE {&browse-name}.
  
  RUN est/blks-frm.p (ROWID(ef)).

  FIND CURRENT ef NO-LOCK.

  RUN est/frms-est.p (ROWID(est)).
  FIND CURRENT est NO-LOCK.

  ASSIGN
   lv-eb-recid = RECID(eb) 
   lv-ef-recid = RECID(ef).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE custom-row-changed B-table-Win 
PROCEDURE custom-row-changed :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS CHAR NO-UNDO.

  IF lv-repo EQ "ON" AND AVAIL eb THEN DO:
    
      IF AVAIL eb AND eb.est-type = 8 THEN
       BROWSE {&browse-name}:REFRESH().
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"form-blank-target",OUTPUT char-hdl).

    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
      RUN repo-on-off IN WIDGET-HANDLE(char-hdl) ("OFF").
      RUN repo-query IN WIDGET-HANDLE(char-hdl) (ROWID(eb)).
      RUN repo-on-off IN WIDGET-HANDLE(char-hdl) ("ON").
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-est-childrecord B-table-Win 
PROCEDURE delete-est-childrecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH est-qty WHERE est-qty.company = est.company 
                     AND est-qty.est-no = est.est-no
                     :
     FOR EACH eb WHERE eb.company = est.company 
                   AND eb.est-no = est.est-no
                   AND eb.eqty = est-qty.eqty:
         DELETE eb.             
     END.
     FOR EACH ef WHERE ef.company = est.company 
                   AND ef.est-no = est.est-no
                   AND ef.eqty = est-qty.eqty:
         DELETE ef.             
     END.
     DELETE est-qty.
  END.   
  FOR EACH est-prep WHERE est-prep.company = est.company
                      AND est-prep.est-no = est.est-no:
      DELETE est-prep.
  END.
  FOR EACH est-op WHERE est-op.company = est.company
                    AND est-op.est-no = est.est-no:
      DELETE est-op.
  END.
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
  HIDE FRAME Corr.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE est-from-tandem B-table-Win 
PROCEDURE est-from-tandem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR ll-new-tandem AS LOG NO-UNDO.
  DEF VAR ll-dumb AS LOG NO-UNDO.
  DEF VAR lv-eb-rowid AS ROWID NO-UNDO.

  DEF BUFFER b-est FOR est.
  DEF BUFFER b-eb FOR eb.
  DEF VAR v-log AS LOG NO-UNDO.

  RUN crt-new-est.

  FIND b-eb WHERE RECID(b-eb) EQ lv-eb-recid NO-LOCK NO-ERROR.

  IF AVAIL b-eb THEN DO:
    RUN est/d-selest.w (ROWID(b-eb), NO, "",
                        OUTPUT ll-new-tandem, OUTPUT lv-eb-rowid).

    IF ll-new-tandem THEN DO:
      FIND FIRST xest OF b-eb NO-LOCK NO-ERROR.
      IF AVAIL xest THEN
         v-log = xest.est-type EQ 8.

      RUN cec/mach-seq.p (b-eb.form-no, 0, v-log).

      RUN get-link-handle IN adm-broker-hdl  (THIS-PROCEDURE,'Record-source':U,OUTPUT char-hdl).
      RUN New_Record IN WIDGET-HANDLE(char-hdl) (ROWID(b-eb)).
      ll-dumb = {&browse-name}:REFRESH() IN FRAME {&FRAME-NAME}.

      RUN release-shared-buffers.
    END.

    ELSE DO:
      FIND FIRST b-est OF b-eb EXCLUSIVE NO-ERROR.
      IF AVAIL b-est THEN DELETE b-est.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE estOpDim B-table-Win 
PROCEDURE estOpDim :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Moved from local assign
    Variables and buffers used:
    
    ll-is-copy-record 
    cereoute-log
    li-est-type
    BUFFER est
    BUFFER ef
    BUFFER eb
    BUFFER est-qty
    BUFFER est
    lv-hld-eqty
    ll-add-set-part
    ll-add-set-part-2
    sets xx
    sets BUFFER reftable
    sets BUFFER xest
    sets BUFFER xeb
    sets li
------------------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER li-est-type AS INTEGER     NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER lv-hld-eqty AS DECIMAL     NO-UNDO.
DEF VAR xx AS INT NO-UNDO.
DEF VAR li AS INT NO-UNDO.


   IF NOT ll-is-copy-record AND ceroute-log AND 
      li-est-type GE 5 AND li-est-type LE 6 THEN DO:
      
      FIND xest WHERE RECID(xest) = recid(est).
      FIND xef WHERE RECID(xef) = recid(ef).
      FIND xeb WHERE RECID(xeb) = recid(eb).

      FOR EACH est-op
          WHERE est-op.company EQ xest.company
            AND est-op.est-no  EQ xest.est-no
            AND est-op.qty     EQ lv-hld-eqty:
        est-op.qty = est-qty.eqty.
      END.
     
      FOR EACH est-op
          WHERE est-op.company EQ xest.company
            AND est-op.est-no  EQ xest.est-no
            AND est-op.qty     EQ est-qty.eqty
            AND est-op.line    GE 500:
        DELETE est-op.
      END.
     
      IF CAN-FIND(FIRST est-op WHERE est-op.company EQ xest.company
                                 AND est-op.est-no  EQ xest.est-no
                                 AND est-op.qty     EQ est-qty.eqty
                                 AND est-op.s-num   EQ xef.form-no) THEN
      DO:
         IF ll-add-set-part EQ YES OR ll-add-set-part-2 EQ YES THEN
         DO:
            IF xef.m-code = "" THEN DO:
               ASSIGN
                  eb.num-wid = 1
                  eb.num-len = 1
                  eb.num-up = 1.
               
               RUN cec/calc-dim.p .
            END.
         END.
        
         FOR EACH est-op
             WHERE est-op.company EQ xest.company
               AND est-op.est-no  EQ xest.est-no
               AND est-op.qty     EQ lv-hld-eqty
               AND est-op.s-num   EQ xef.form-no
             NO-LOCK:
         END.
      END.
     
      ELSE DO:
         /* Protect existing est-op records */
         FOR EACH tt-est-op:
             DELETE tt-est-op.
         END.
        
         FOR EACH est-op
             WHERE est-op.company EQ xest.company
               AND est-op.est-no  EQ xest.est-no
               AND est-op.qty     EQ est-qty.eqty:
           CREATE tt-est-op.
           BUFFER-COPY est-op TO tt-est-op.
           DELETE est-op.
         END.
                                   
         xx = DEC(xef.form-no).
        
         RUN cec/mach-seq.p (xef.form-no, est-qty.eqty, NO).
        
         IF ll-add-set-part EQ YES OR ll-add-set-part-2 EQ YES THEN
         DO:
            IF xef.m-code = "" THEN
            DO:
               ASSIGN
                  eb.num-wid = 1
                  eb.num-len = 1
                  eb.num-up = 1.

               /*IF ll-add-set-part-2 THEN
               DO:*/ /* ticket 15488 */
                  FIND xest WHERE RECID(xest) = recid(est).
                  FIND xef WHERE RECID(xef) = recid(ef).
                  FIND xeb WHERE RECID(xeb) = recid(eb).
              /* END.*/
               RUN cec/calc-dim.p .
            END.
         END.
        
         FOR EACH est-op
             WHERE est-op.company EQ xest.company
               AND est-op.est-no  EQ xest.est-no
               AND est-op.qty     EQ est-qty.eqty
               AND est-op.s-num   NE INT(xx):
           DELETE est-op.
         END.
        
         FOR EACH tt-est-op:
             CREATE est-op.
             BUFFER-COPY tt-est-op TO est-op.
             DELETE tt-est-op.
         END.
        
         li = 0.
         FOR EACH est-op
             WHERE est-op.company EQ est.company
               AND est-op.est-no  EQ est.est-no
               AND est-op.line    LT 500
             BY est-op.qty
             BY est-op.s-num
             BY est-op.b-num
             BY est-op.d-seq
             BY est-op.op-pass
             BY est-op.rec_key:
                   
           ASSIGN
            li          = li + 1
            est-op.line = li.        
           
         END.
      END.
   END.
   ELSE
      IF ll-add-set-part EQ YES OR ll-add-set-part-2 EQ YES THEN
      DO:
         IF xef.m-code = "" THEN DO:
            ASSIGN
               eb.num-wid = 1
               eb.num-len = 1
               eb.num-up = 1.
            RUN cec/calc-dim.p.
         END.
      END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE farmNav B-table-Win 
PROCEDURE farmNav :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {est/farmNav.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-eb B-table-Win 
PROCEDURE get-eb :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

  DEF BUFFER b-eb FOR eb.

  DEF VAR li AS INT NO-UNDO.
  

  IF NOT AVAIL eb OR ROWID(eb) NE ip-rowid THEN DO:
    RUN dispatch ('get-first':U).
    IF CAN-FIND(FIRST b-eb WHERE b-eb.company EQ est.company
                             AND b-eb.est-no  EQ est.est-no
                             AND ROWID(b-eb)  EQ ip-rowid) THEN
    DO WHILE AVAIL eb AND ROWID(eb) NE ip-rowid:
      li = li + 1.
      IF li GT 200 THEN LEAVE.
      RUN dispatch ('get-next':U).
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-eb-rowid B-table-Win 
PROCEDURE get-eb-rowid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-rowid AS ROWID NO-UNDO.


  op-rowid = IF AVAIL eb THEN ROWID(eb) ELSE ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-form-blank B-table-Win 
PROCEDURE get-form-blank :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-form AS INT .
  DEF OUTPUT PARAM op-blank AS INT.

  IF AVAIL eb THEN
     ASSIGN op-form = eb.FORM-no
            op-blank = eb.BLANK-no.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-last-est-cat B-table-Win 
PROCEDURE get-last-est-cat :
/*------------------------------------------------------------------------------
  Purpose:     Get the last eb category used.
  Parameters:  <none>
  Notes:       SET:  est.est-type EQ 6 
------------------------------------------------------------------------------*/
DEFINE BUFFER buf-eb FOR eb.

FIND LAST buf-eb WHERE buf-eb.company = gcompany 
                     AND buf-eb.est-type = 6 /*> 4*/ NO-LOCK NO-ERROR.

IF AVAIL buf-eb THEN 
    ASSIGN lv-last-est-cat = buf-eb.procat.

RETURN.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getEstQtyRowID B-table-Win 
PROCEDURE getEstQtyRowID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER opEstQtyRowID AS ROWID NO-UNDO.

  opEstQtyRowID = ROWID(est-qty).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record B-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-eb FOR eb.
  DEFINE VARIABLE lDummy AS LOGICAL NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN
   ll-add-set = NO
   /*ll-add-set-part = NO*/
   lv-cad-no  = ""
   lv-die-no  = "".

  IF NOT ll-is-add-from-tool THEN DO:
    RUN est/d-addfol.w (INPUT YES, OUTPUT ls-add-what).
     /*run est/d-addwh2.w  (output ls-add-what).*/
     /*if ls-add-what = "set"    /* new estimate */
        then run est/d-addset.w (output ls-add-what). /* one item or set cec/est-add.p */*/
   
     IF ls-add-what = "" THEN RETURN NO-APPLY.  /* cancel from dialog box */
  END.

  IF CAN-DO("form,blank",ls-add-what) THEN DO:
    {custom/checkuse.i}

    IF AVAIL eb AND eb.est-type EQ 6 THEN DO:
      IF CAN-FIND(FIRST b-eb WHERE b-eb.company EQ eb.company
                               AND b-eb.est-no  EQ eb.est-no
                               AND b-eb.form-no EQ 0
                               AND b-eb.part-no EQ eb.part-no) THEN DO:
        MESSAGE "Set header part# must be changed to create a set..."
            VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
      END.
    END.
  END.

  /* Get category from last estimate before creating a new one. */
  RUN get-last-est-cat.

  ASSIGN
    ll-is-add-from-tool = NO
    cadcamValue = ''.
  IF ls-add-what EQ "copy-est" THEN DO:
      RUN local-copy-record .
  END.
  ELSE IF ls-add-what EQ "est-from-tandem" THEN RUN est-from-tandem.
  ELSE IF ls-add-what = "estCad" THEN DO:
    EMPTY TEMP-TABLE tt-artios.
    RUN est/d-artioscad.w (cocode).
    RUN createESTfromArtios.
  END.
  ELSE IF ls-add-what = "estCadNew" THEN DO:
    RUN util/impEsko.p.
    RUN dispatch('open-query').
    lDummy = {&browse-name}:REFRESH() IN FRAME {&FRAME-NAME}.
  END.
  ELSE IF ls-add-what = "estCadTest" THEN DO:
    RUN est/d-ArtiosCadSimple.w (cocode).
  END.
  ELSE IF ls-add-what = "estImpact" THEN DO:
     EMPTY TEMP-TABLE tt-artios.
     RUN est/d-impact.w (cocode).
     RUN createESTfromImpact.
  END.
  ELSE IF ls-add-what = "farm" THEN DO:
     EMPTY TEMP-TABLE tt-frmout.
     RUN est/d-frmout.w (cocode).
     RUN createESTFarmOut.
  END.
  ELSE IF ls-add-what = "ImportForm" THEN DO:
      RUN est/dImportEst.w (cocode).
      RUN pCreateFormFromImport.
  END.
  ELSE DO:
    {est/d-cadcamrun.i}

    /* Dispatch standard ADM method. */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
  END.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR is-first-record AS LOG INIT NO NO-UNDO.
  DEF BUFFER bf-eb FOR eb.  
  DEF BUFFER bf-ef FOR ef.
  DEF BUFFER bf-est FOR est.
  DEF BUFFER bf-est-qty FOR est-qty.
  DEF BUFFER b-ef FOR ef.
  DEF BUFFER b-eb FOR eb.
  DEF BUFFER b-ref FOR reftable.
  DEFINE BUFFER bff-itemfg FOR itemfg .

  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR xx AS DEC NO-UNDO.
  DEF VAR lv-hld-cust LIKE eb.cust-no NO-UNDO.
  DEF VAR lv-hld-ship LIKE eb.ship-id NO-UNDO.
  DEF VAR li-est-type AS INT NO-UNDO.
  DEF VAR lv-hld-eqty LIKE est-qty.eqty NO-UNDO.
  DEF VAR lv-hld-fcol LIKE ef.f-col NO-UNDO.
  DEF VAR lv-hld-fpas LIKE ef.f-pass NO-UNDO.
  DEF VAR lv-hld-fcot LIKE ef.f-coat NO-UNDO.
  DEF VAR lv-hld-fctp LIKE ef.f-coat-p NO-UNDO.
  DEF VAR lv-hld-wid LIKE eb.wid NO-UNDO.
  DEF VAR lv-hld-len LIKE eb.len NO-UNDO.
  DEF VAR lv-hld-dep LIKE eb.dep NO-UNDO.
  DEF VAR lv-hld-style LIKE eb.style NO-UNDO.
  DEF VAR lv-hld-board AS cha NO-UNDO.
  DEF VAR lv-layers AS DEC NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lj AS INT NO-UNDO.
  DEF VAR lv-cad-path AS cha NO-UNDO.
  DEF VAR lv-cad-ext AS cha NO-UNDO.
  DEF VAR ll-2pc AS LOG NO-UNDO.
  DEF VAR lv-box-des AS CHAR INIT "S" NO-UNDO.
  DEF VAR v-dec AS DEC NO-UNDO.
  DEF VAR v-dec2 AS DEC NO-UNDO.
  DEF VAR v-count AS INT NO-UNDO.
  DEF VAR v-w-array AS DEC EXTENT 30 NO-UNDO.  
  DEF VAR cNewRep AS CHAR NO-UNDO.
  DEFINE VARIABLE is2PieceBox AS LOG NO-UNDO.
  
  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN
   lv-hld-cust   = eb.cust-no
   lv-hld-ship   = eb.ship-id
   lv-hld-eqty   = est-qty.eqty
   lv-hld-fcol   = ef.f-col
   lv-hld-fpas   = ef.f-pass
   lv-hld-fcot   = ef.f-coat
   lv-hld-fctp   = ef.f-coat-p
   lv-hld-wid    = eb.wid
   lv-hld-len    = eb.len
   lv-hld-dep    = eb.dep
   lv-hld-style  = eb.style
   lv-hld-board  = ef.board
   is2PieceBox = NO.

  {est/blankcp2.i}

  RUN get-attribute IN adm-broker-hdl ('Is-First-Est').
  IF RETURN-VALUE = "Yes" THEN DO:
       is-first-record = YES.
       RUN set-attribute-list IN adm-broker-hdl ('Is-First-Est=No'). /* reset */
  END.     
  ELSE is-first-record = NO.

  /* Change 4th enabled record lock status PROGRESS only does upto 3 records 
     est,est-qty,eb,ef */
  FIND CURRENT est EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
  FIND CURRENT est-qty EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
  FIND CURRENT ef EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
  FIND CURRENT eb EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

  viEQtyPrev = eb.eqty.
  
  IF est.est-type EQ 6 AND CAN-FIND(bf-eb WHERE bf-eb.company = est.company AND bf-eb.est-no = est.est-no AND bf-eb.blank-no > 0)
     AND CAN-FIND(FIRST b-eb WHERE b-eb.company = est.company AND b-eb.est-no = est.est-no AND b-eb.form-no = 0 AND
                           b-eb.part-no = eb.part-no AND b-eb.stock-no = eb.stock-no AND b-eb.part-dscr1 = eb.part-dscr1
                           AND b-eb.len = eb.len AND b-eb.wid = eb.wid AND b-eb.dep = eb.dep)
              /* OR  (est.est-type EQ 5 AND eb.quantityPerSet GE 2) */   
  THEN ASSIGN is2PieceBox = YES.
       
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ll-2pc = eb.quantityPerSet GT 1 AND
           NOT CAN-FIND(FIRST bf-eb
                        WHERE bf-eb.company EQ eb.company
                          AND bf-eb.est-no  EQ eb.est-no
                          AND bf-eb.form-no NE 0
                          AND ROWID(bf-eb)  NE ROWID(eb)).

  IF TRIM(est.est-no) NE TRIM(eb.est-no) THEN DO:
    ASSIGN
     est.est-no     = FILL(" ", 8 - LENGTH(TRIM(est.est-no))) + TRIM(est.est-no)
     est-qty.est-no = est.est-no
     eb.est-no      = est.est-no
     ef.est-no      = est.est-no.

    ce-ctrl-loop:
    REPEAT:
       FIND FIRST ce-ctrl
            WHERE ce-ctrl.company EQ gcompany
              AND ce-ctrl.loc     EQ gloc
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
       IF AVAIL ce-ctrl THEN
       DO:
          IF ce-ctrl.e-num LT INT(est.est-no) THEN
             ce-ctrl.e-num = INT(est.est-no).

          FIND CURRENT ce-ctrl NO-LOCK.
          LEAVE ce-ctrl-loop.
       END.
    END.
  END.

  IF est.est-type EQ 8 AND
     CAN-FIND(FIRST bf-eb
              WHERE bf-eb.company EQ eb.company
                AND bf-eb.est-no  EQ eb.est-no
                AND ROWID(bf-eb)  NE ROWID(eb)) THEN
    ASSIGN
     eb.bl-qty    = est-qty.eqty
     est-qty.eqty = lv-hld-eqty.

  ELSE
  IF lv-hld-eqty NE est-qty.eqty THEN DO:
    FIND FIRST bf-est-qty
        WHERE bf-est-qty.company EQ est-qty.company
          AND bf-est-qty.est-no  EQ est-qty.est-no
          AND bf-est-qty.eqty    EQ est-qty.eqty
          AND ROWID(bf-est-qty)  NE ROWID(est-qty)
        NO-ERROR.
    IF AVAIL bf-est-qty THEN bf-est-qty.eqty = lv-hld-eqty.
  END.

  IF ll-new-shipto THEN DO:
    RUN windows/d-shpfly.w (ROWID(eb)).
    IF eb.ship-id NE "TEMP" THEN
    FIND FIRST shipto
        WHERE shipto.company EQ cocode
          AND shipto.cust-no EQ eb.cust-no
          AND shipto.ship-id EQ eb.ship-id
        NO-LOCK NO-ERROR.
    IF AVAIL shipto THEN
      ASSIGN
       eb.ship-id      = shipto.ship-id
       eb.ship-name    = shipto.ship-name
       eb.ship-addr[1] = shipto.ship-addr[1]
       eb.ship-addr[2] = shipto.ship-addr[2]
       eb.ship-city    = shipto.ship-city
       eb.ship-state   = shipto.ship-state
       eb.ship-zip     = shipto.ship-zip.
  END.
  IF adm-adding-record OR lv-hld-cust NE eb.cust-no THEN DO:
       FIND FIRST cust NO-LOCK
            WHERE cust.company = gcompany
              AND cust.cust-no = eb.cust-no NO-ERROR.
       IF AVAIL cust THEN
           est.csrUser_id = cust.csrUser_id .
  END.

  IF NOT ll-is-copy-record THEN DO:
    {ce/uship-id.i ll-new-record}
  END.

  FIND CURRENT eb.
  FOR EACH tt-eb-set BREAK BY tt-eb-set.company:
      IF FIRST(tt-eb-set.company) THEN DO:
         CREATE bf-eb.
         BUFFER-COPY tt-eb-set TO bf-eb
         ASSIGN
            bf-eb.form-no = 0
            bf-eb.cust-no = eb.cust-no.
       END.
  END.

  FOR EACH tt-eb-set-part BREAK BY tt-eb-set-part.company:
      IF FIRST(tt-eb-set-part.company) THEN DO:
         CREATE bf-eb.
         BUFFER-COPY tt-eb-set-part EXCEPT part-dscr1 part-dscr2 stock-no TO bf-eb
         ASSIGN
            bf-eb.form-no = 0
            bf-eb.cust-no = eb.cust-no
            bf-eb.stock-no = tt-eb-set-part.header-stock-no
            bf-eb.part-dscr1 = tt-eb-set-part.header-part-dscr1
            bf-eb.part-dscr2 = tt-eb-set-part.header-part-dscr2.
       END.
  END.
  
  RUN update-sb-qtys (ROWID(eb)).

  /* convert to decimal from 1/16 */
  {sys/inc/k16bb.i eb.wid  } 
  {sys/inc/k16bb.i eb.len  } 
  {sys/inc/k16bb.i eb.dep  } 

  {cec/slotheit.i}
  /*== update all eb,ef eqty field ==*/
  {cec/estitm2.i}

  IF lv-hld-board NE ef.board THEN DO:
    FIND FIRST item NO-LOCK
        WHERE item.company EQ ef.company
          AND item.i-no    EQ ef.board
        NO-ERROR.
    IF AVAIL item THEN DO:
      ASSIGN
       ef.brd-dscr = item.i-name
       ef.weight   = item.basis-w.  
      FIND FIRST e-item OF item NO-LOCK NO-ERROR.
      IF AVAIL e-item THEN ef.cost-uom = e-item.std-uom.
    END.
  END.

  /* copy logic - size limit */
  {cec/estitm1.i}

  IF ll-add-set-part AND ll-add-set-part-2 EQ NO AND AVAIL tt-eb-set-part THEN
     eb.part-dscr2 = tt-eb-set-part.part-dscr2.

  IF est.est-type NE 8 THEN RUN check-for-set.

  FIND xest WHERE ROWID(xest) EQ ROWID(est) NO-LOCK NO-ERROR.
  FIND xef WHERE ROWID(xef)   EQ ROWID(ef)  NO-LOCK NO-ERROR.
  
  FIND FIRST bf-eb
      WHERE bf-eb.company  EQ eb.company
        AND bf-eb.est-no   EQ eb.est-no
        AND bf-eb.form-no  EQ 0
      NO-ERROR.

  IF AVAIL bf-eb THEN DO:
    IF ll-2pc AND eb.stock-no NE "" AND
       NOT ll-add-set-part AND NOT ll-add-set-part-2 THEN bf-eb.stock-no = eb.stock-no.

    IF bf-eb.stock-no NE ""                                 AND
       (bf-eb.stock-no NE eb.stock-no OR ll-2pc)            AND
       NOT CAN-FIND(FIRST itemfg
                    WHERE itemfg.company EQ bf-eb.company
                      AND itemfg.i-no    EQ bf-eb.stock-no) THEN DO:
      FIND xeb WHERE ROWID(xeb) EQ ROWID(bf-eb) NO-LOCK NO-ERROR.
      
      RUN fg/ce-addfg.p (bf-eb.stock-no).
    END.
  END.

  IF ll-add-set-part AND eb.stock-no NE "" THEN
     ll-crt-itemfg = YES.

  IF ll-crt-itemfg AND
     NOT CAN-FIND(FIRST itemfg
                  WHERE itemfg.company EQ eb.company
                    AND itemfg.i-no    EQ eb.stock-no) THEN DO:
    FIND xeb WHERE ROWID(xeb) EQ ROWID(eb) NO-LOCK NO-ERROR.
    RUN fg/ce-addfg.p (xeb.stock-no).
  END.

  IF ll-add-set-part EQ NO THEN
     ll-crt-itemfg = NO.

  IF eb.stock-no NE "" AND eb.part-dscr2 EQ "" THEN DO:
    FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company    EQ eb.company
          AND itemfg.i-no       EQ eb.stock-no
          AND itemfg.part-dscr1 NE ""
        NO-ERROR.
    IF AVAIL itemfg THEN eb.part-dscr2 = itemfg.part-dscr1.
  END.

   /*== Qty assignment ==*/
   RUN assign-qty. 
   RUN reset-est-type (OUTPUT li-est-type).
   IF est.est-type NE 8 THEN RUN check-for-set.
   FIND CURRENT est.

   IF (li-est-type GE 5 AND li-est-type LE 6 AND eb.i-col + eb.i-coat EQ 0) OR
      (li-est-type GE 7 AND li-est-type LE 8 AND ef.f-col + ef.f-coat EQ 0) THEN DO:
     {ce/delplate.i}
   END.

   IF li-est-type GE 7 AND li-est-type LE 8 THEN
     IF lv-hld-fcol NE ef.f-col    OR
        lv-hld-fpas NE ef.f-pass   OR
        lv-hld-fcot NE ef.f-coat   OR
        lv-hld-fctp NE ef.f-coat-p THEN DO:
       {sys/inc/flm-prep.i}
     END.
 
   IF v-shiptorep-log AND adm-new-record THEN DO:       /* task 05301401 */
     RUN ce/markup.p (eb.company, ROWID(eb), OUTPUT ld-markup).
     RUN sys/inc/getsmncm-2.p (eb.cust-no, INPUT-OUTPUT eb.sman,
                             (IF AVAIL bf-eb THEN bf-eb.procat ELSE eb.procat),
                             ld-markup,
                             OUTPUT eb.comm,eb.ship-id).

     RUN fg/fgSlsRep.p (INPUT eb.company,
                        INPUT eb.cust-no,
                        INPUT eb.part-no,
                        INPUT eb.stock-no,
                        OUTPUT cNewRep).
     IF cNewRep GT "" AND cNewRep NE eb.sman THEN DO:
          eb.sman = cNewRep.
          RUN sys/inc/getsmncm.p (eb.cust-no, INPUT-OUTPUT eb.sman, eb.procat, ld-markup,
                                    OUTPUT eb.comm).
     END.

   END.
    
   IF est.est-type GT 6                           AND
      (adm-new-record OR eb.yld-qty LT eb.bl-qty) THEN
     RUN set-yld-qty (ROWID(eb)).

  RUN ce/com/istandem.p (ROWID(est), OUTPUT ll-tandem).

  IF cegoto-log                     OR
     (est.est-type EQ 8       AND
      old-bl-qty NE eb.bl-qty AND
      NOT ll-new-record       AND
      NOT ll-tandem) THEN RUN run-goto.

  RUN estOpDim (INPUT-OUTPUT li-est-type, INPUT-OUTPUT lv-hld-eqty).

  EMPTY TEMP-TABLE tt-eb-set.
  EMPTY TEMP-TABLE tt-eb-set-part.
  
  IF est.est-type NE 8 THEN
  FOR EACH bf-eb
      WHERE bf-eb.company EQ eb.company
        AND bf-eb.est-no  EQ eb.est-no
        AND ROWID(bf-eb)  NE ROWID(eb):
    ASSIGN
     bf-eb.cust-no      = eb.cust-no
     bf-eb.ship-id      = eb.ship-id
     bf-eb.ship-no      = eb.ship-no
     bf-eb.ship-name    = eb.ship-name
     bf-eb.ship-addr[1] = eb.ship-addr[1]
     bf-eb.ship-addr[2] = eb.ship-addr[2]
     bf-eb.ship-city    = eb.ship-city
     bf-eb.ship-state   = eb.ship-state
     bf-eb.ship-zip     = eb.ship-zip
     bf-eb.sman         = eb.sman
     bf-eb.comm         = eb.comm.
  END.

  IF cestyle-log                    AND
     (adm-adding-record        OR
      lv-hld-wid   NE eb.wid   OR
      lv-hld-len   NE eb.len   OR
      lv-hld-dep   NE eb.dep   OR
      lv-hld-style NE eb.style)     THEN DO:

    IF NOT adm-new-record THEN
       MESSAGE "Do you wish to reset box design?"
          VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE ll-ans2 AS LOG.
    ELSE ll-ans2 = YES.

    IF ll-ans2 THEN lv-box-des = "B".
    ELSE lv-box-des = "N".
    
  END.
  ELSE
     FOR FIRST box-design-hdr WHERE
         box-design-hdr.design-no = 0 AND
         box-design-hdr.company = eb.company AND
         box-design-hdr.est-no = eb.est-no AND
         box-design-hdr.form-no = eb.form-no AND
         box-design-hdr.blank-no = eb.blank-no
         NO-LOCK:

         FOR EACH box-design-line FIELDS(wscore) OF box-design-hdr
             NO-LOCK:
             v-dec = DECIMAL(TRIM(box-design-line.wscore)) NO-ERROR.
             IF NOT ERROR-STATUS:ERROR AND
                TRIM(box-design-line.wscore) NE "" THEN
                ASSIGN
                   v-count = v-count + 1
                   v-w-array[v-count] = v-dec.
         END.

         RUN tokenize-proc(box-design-hdr.lscore).

         DO v-count = 1 TO 30:
            ASSIGN
               v-dec = {sys/inc/k16v.i eb.k-len-array2[v-count]}
               v-dec2 = {sys/inc/k16v.i eb.k-wid-array2[v-count]}.
               
            IF v-l-array[v-count] NE v-dec OR
               v-w-array[v-count] NE v-dec2 THEN
               DO:
                  MESSAGE "Do you wish to reset box design?"
                     VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans2.
                  IF ll-ans2 THEN
                     lv-box-des = "B".
                  ELSE
                     lv-box-des = "N".
                  LEAVE.
               END.
         END.
     END.
  
  DO li = 1 TO 2:
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"box-calc-target",OUTPUT char-hdl).
    IF VALID-HANDLE(WIDGET-HANDLE(ENTRY(1,char-hdl))) THEN DO:
      RUN build-box IN WIDGET-HANDLE(ENTRY(1,char-hdl)) (lv-box-des).
      li = 2.
    END.
    ELSE
    IF li EQ 1 THEN DO:
      RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-source",OUTPUT char-hdl).
      IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
         RUN init-box-design IN WIDGET-HANDLE(char-hdl) (THIS-PROCEDURE).
      ELSE li = 2.
    END.
  END.
      
  IF cestyle-log                    AND
     (adm-adding-record        OR
      lv-hld-wid   NE eb.wid   OR
      lv-hld-len   NE eb.len   OR
      lv-hld-dep   NE eb.dep   OR
      lv-hld-style NE eb.style)     THEN DO:
    FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                        AND sys-ctrl.name    EQ "CADFILE"
                        NO-LOCK NO-ERROR.
    IF NOT AVAIL sys-ctrl THEN DO :
      CREATE sys-ctrl.
      ASSIGN sys-ctrl.company = cocode
             sys-ctrl.name    = "CADFILE"
             sys-ctrl.descrip = "Dictate the location of the cad image to search."
             sys-ctrl.char-fld = "R:\rcode\cadimage\".      

    END.
    lv-cad-path = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "".
    IF lv-cad-path <> "" THEN DO:
      IF lv-cad-ext = "" THEN lv-cad-ext = ".jpg".
      IF SEARCH(lv-cad-path + eb.cad-no + lv-cad-ext) = ? THEN lv-cad-ext = "".
    END.
    IF SEARCH(lv-cad-path + eb.cad-no + lv-cad-ext) <> ? THEN DO:
        FIND FIRST box-design-hdr WHERE box-design-hdr.design-no = 0 AND
                                     box-design-hdr.company = eb.company 
                                 AND box-design-hdr.est-no = eb.est-no     
                                 AND box-design-hdr.form-no   EQ eb.form-no
                                 AND box-design-hdr.blank-no  EQ eb.blank-no NO-ERROR.

        IF AVAIL box-design-hdr THEN 
           ASSIGN box-design-hdr.box-image = lv-cad-path + eb.cad-no + lv-cad-ext. /*".jpg"*/.
     END.
  END.

  IF eb.pur-man THEN ef.nc = NO.
  IF lCheckPurMan THEN DO:
      FIND FIRST bff-itemfg EXCLUSIVE-LOCK
           WHERE bff-itemfg.company EQ cocode
             AND bff-itemfg.i-no EQ eb.stock-no NO-ERROR .
      IF AVAIL bff-itemfg THEN
          ASSIGN bff-itemfg.pur-man = eb.pur-man .
      FIND CURRENT bff-itemfg NO-LOCK NO-ERROR .
  END.
  ASSIGN lCheckPurMan = NO .

  ll-new-shipto = NO.
  RUN valid-eb-reckey.

  IF adm-new-record AND ll-add-set-part = YES OR ll-add-set-part-2 = YES THEN DO:
      IF est.est-type NE 8 THEN
        FOR EACH bf-eb WHERE bf-eb.company EQ eb.company
                         AND bf-eb.est-no  EQ eb.est-no,
           FIRST bf-ef WHERE bf-ef.company EQ eb.company
                         AND bf-ef.est-no  EQ eb.est-no
                         AND bf-ef.eqty EQ eb.eqty
                         AND bf-ef.form-no EQ eb.form-no:
          RUN upd-fg-frt-class (INPUT ROWID(bf-eb), bf-ef.board).
        END.

  END.

  IF adm-new-record AND eb.pur-man THEN RUN create-e-itemfg-vend.
  ELSE IF eb.pur-man AND eb.eqty <> viEQtyPrev THEN RUN update-e-itemfg-vend.

  /* If unitized and form 1, blank 1, copy to form zero record. */
  IF eb.pur-man = NO AND eb.form-no = 1 AND eb.blank-no = 1 THEN
      RUN copy-2-form-zero.

  IF is2PieceBox THEN DO:
     FIND b-eb WHERE b-eb.company = est.company AND b-eb.est-no = est.est-no AND b-eb.form-no = 0 AND b-eb.blank-no = 0 EXCLUSIVE-LOCK.
     ASSIGN b-eb.stock-no = eb.stock-no
            b-eb.part-no = eb.part-no
            b-eb.part-dscr1 = eb.part-dscr1
            b-eb.part-dscr2 = eb.part-dscr2
            b-eb.procat = eb.procat
            b-eb.len = eb.len
            b-eb.wid = eb.wid
            b-eb.dep = eb.dep
            .         
     RELEASE b-eb.       
  END.
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record B-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-eb FOR eb.
  DEF BUFFER bff-eb FOR eb.

  /* Code placed here will execute PRIOR to standard behavior. */  

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN release-shared-buffers.
  
  IF ll-new-record THEN DO:
        RUN get-link-handle IN adm-broker-hdl  (THIS-PROCEDURE,'Record-source':U,OUTPUT char-hdl).
        RUN New_Record IN WIDGET-HANDLE(char-hdl) (v-rowid-eb).
   END.

   RUN set-panel (1).

  ASSIGN
   ll-is-add-from-tool = NO  /* reset */
   adm-new-record      = NO
   adm-adding-record   = NO
   ll-new-record       = NO
   ll-new-est          = NO
   ll-add-set-part-2   = NO
   ll-add-set-part     = NO
   ls-add-what         = ""
   lv-copy-what        = ""
   ll-is-copy-record   = NO.

  IF v-cecscrn-dec THEN
     ASSIGN
        eb.wid:WIDTH IN BROWSE {&browse-name} = 15.2
        eb.wid:FORMAT IN BROWSE {&browse-name} = ">>9.999999"
        eb.len:WIDTH IN BROWSE {&browse-name} = 15.2
        eb.len:FORMAT IN BROWSE {&browse-name} = ">>9.999999"
        eb.dep:WIDTH IN BROWSE {&browse-name} = 15.2
        eb.dep:FORMAT IN BROWSE {&browse-name} = ">>9.999999".

  IF AVAIL est AND est.est-type NE 5 AND
     CAN-FIND(b-eb WHERE b-eb.company EQ est.company
                     AND b-eb.est-no  EQ est.est-no) THEN DO:
      ASSIGN
          est-qty.eqty:LABEL IN BROWSE {&browse-name} = "Est Qty"
          eb.yld-qty:HIDDEN IN BROWSE {&browse-name}   = YES.
     
      FOR EACH bff-eb
          WHERE bff-eb.company EQ est.company
            AND bff-eb.est-no  EQ est.est-no
            AND bff-eb.form-no NE 0:
          bff-eb.quantityPerSet = 1 .
      END.

     FIND CURRENT est.
     est.est-type = 5.
     FIND CURRENT est NO-LOCK.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record B-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE v-neweb-est AS CHARACTER NO-UNDO.
   DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
   DEFINE VARIABLE li AS INT NO-UNDO.
   DEFINE VARIABLE ll-dumb AS LOG NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  {custom/checkuse.i}
  
  RUN ce/d-cpwhat.w (OUTPUT lv-copy-what).
  IF lv-copy-what = "" THEN RETURN.

  IF lv-copy-what EQ "copy" THEN DO:
      RUN ce/copyestN.w (lv-copy-what, est.est-no, OUTPUT v-neweb-est ) .
      IF v-neweb-est NE "" THEN DO:
          FIND FIRST est WHERE est.company EQ cocode
              AND est.est-no EQ FILL(" ",8 - LENGTH(TRIM(v-neweb-est))) + TRIM(v-neweb-est) NO-LOCK NO-ERROR .
          
          IF AVAIL est THEN DO:
              FIND FIRST eb WHERE eb.company = est.company
                  AND eb.est-no = est.est-no NO-LOCK NO-ERROR .

              RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

              /* refresh browser for new record */
              RUN get-link-handle IN adm-broker-hdl  (THIS-PROCEDURE,'Record-source':U,OUTPUT char-hdl).
              RUN New_Record IN WIDGET-HANDLE(char-hdl) (ROWID(eb)).

              ll-dumb = {&browse-name}:REFRESH() IN FRAME {&FRAME-NAME}.
          END.
      END.
  END.
  ELSE DO:
      ASSIGN lv-ef-copy-frid = RECID(ef)
          lv-eb-copy-frid = RECID(eb)
          ll-is-copy-record = YES.

      RUN set-or-combo.

      /* Dispatch standard ADM method.                             */
      RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .
  END.
      /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR ll-dumb AS LOG NO-UNDO.
  DEF VAR lv-copylist AS CHAR NO-UNDO.
  DEF VAR ll-error AS LOG NO-UNDO.

  DEF BUFFER b-eb FOR eb.
  DEF BUFFER b-ef FOR ef.
      IF AVAIL eb THEN
          v-rowid-eb  = ROWID(eb).
  /* Code placed here will execute PRIOR to standard behavior. */
  IF AVAIL est THEN li-form# = est.form-qty. /* for set creation on crt-new-set */
    
  ASSIGN
  ls-cust-no = IF AVAIL eb THEN eb.cust-no ELSE ""  /* for new item record */
  ls-ship-id = IF AVAIL eb THEN eb.ship-id ELSE ""  /* for crt-new-set */

  /*Dispatch standard ADM method.                              */
  /*RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ).*/
  
  /* Code placed here will execute AFTER standard behavior.    */
  
   ls-set-part-no    = ""
   ll-add-set        = NO
        /*ll-add-set-part   = NO*/ .

  IF ll-is-copy-record THEN DO:  /* works like adding new form */
     ls-add-what = lv-copy-what.
     RUN copy-line.
     IF NOT AVAIL eb THEN FIND eb WHERE RECID(eb) EQ lv-eb-recid NO-LOCK NO-ERROR. 

     RUN est/d-copy.w (ls-add-what, lv-ef-copy-frid, lv-eb-copy-frid,
                       BUFFER ef, BUFFER eb, OUTPUT ll-error).

     FIND CURRENT eb NO-ERROR.
     FIND CURRENT ef NO-ERROR.
  
     ASSIGN BROWSE {&BROWSE-NAME} {&adm-tableio-fields} NO-ERROR.
  END.

  ELSE DO:
    IF ls-add-what EQ "form" OR ls-add-what EQ "blank" THEN DO:

       RUN crt-new-set.
       IF NOT AVAIL eb THEN FIND eb WHERE RECID(eb) EQ lv-eb-recid NO-LOCK NO-ERROR.
    END.

    ELSE DO:
      RUN crt-new-est.
      ASSIGN
         ll-add-set = ls-add-what EQ "estset"
         ll-add-set-part = ls-add-what EQ "estsetasspart".
      /* refresh browser for new record */
      FIND b-eb WHERE RECID(b-eb) EQ lv-eb-recid NO-LOCK NO-ERROR.

      RUN get-link-handle IN adm-broker-hdl  (THIS-PROCEDURE,'Record-source':U,OUTPUT char-hdl).
      /*RUN New_Record IN WIDGET-HANDLE(char-hdl) (ROWID(b-eb)).*/
      RUN New_Record-user IN WIDGET-HANDLE(char-hdl) (ROWID(b-eb)).

      ll-dumb = {&browse-name}:REFRESH() IN FRAME {&FRAME-NAME}.
      RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
      RUN new-state IN WIDGET-HANDLE(char-hdl) ('update-begin':U).  /* to have save button */
      DISPLAY est.est-no est.est-date eb.yld-qty eb.quantityPerSet WITH BROWSE {&browse-name}.
    END.

    {est/d-cadcam.i}

    IF ll-add-set-part-2 = YES THEN
    DO:
       RUN dispatch("update-record").
       RUN dispatch("open-query").
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
  DEF BUFFER bb FOR eb.
  DEF BUFFER bf FOR ef.  
  DEF BUFFER bqty FOR est-qty.
  DEF BUFFER best FOR est.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR ll-dum AS LOG NO-UNDO.
  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR li-est-type LIKE est.est-type NO-UNDO.
  DEF VAR lv-num-rec AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  {custom/checkuse.i}

  IF NOT adm-new-record AND NOT ll-mass-del THEN DO:
    RUN check-delete NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.

    {custom/askdel.i}
  END.

  lv-rowid = ROWID(eb).

  /* Dispatch standard ADM method.                             */
  DO TRANSACTION:
    FIND bb WHERE ROWID(bb) EQ ROWID(eb) NO-ERROR.
    FIND bf WHERE ROWID(bf) EQ ROWID(ef) NO-ERROR.
    FIND bqty WHERE ROWID(bqty) EQ ROWID(est-qty) NO-ERROR.
    FIND best WHERE ROWID(best) EQ ROWID(est) NO-ERROR.

    ll-dum = BROWSE {&browse-name}:DELETE-CURRENT-ROW().

    IF ll-dum THEN DO:
      IF AVAIL bb THEN DELETE bb.

      IF AVAIL bf THEN DO:
        RUN est/blks-frm.p (ROWID(bf)).
        FIND CURRENT bf NO-ERROR.

        IF bf.blank-qty EQ 0 THEN DELETE bf.
      END.

      IF AVAIL best THEN DO:
        RUN est/frms-est.p (ROWID(best)).
        FIND CURRENT best NO-ERROR.

        IF best.form-qty EQ 0 THEN DO:
          ASSIGN
           lv-ef-recid = ?
           lv-eb-recid = ?.
   
          DELETE best.
        END.
      END.
    END.

    RELEASE bb.
    RELEASE bf.
    RELEASE bqty.
    RELEASE best.
  END.

  RUN release-shared-buffers.

  FIND CURRENT est NO-LOCK NO-ERROR.

  IF AVAIL est THEN DO:
    RUN est/resetf&b.p (ROWID(est), ll-mass-del).
    RUN reset-est-type (OUTPUT li-est-type).

    IF AVAIL eb THEN RUN dispatch ("open-query").
  END.

  ELSE DO:
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl). 
    DO WHILE TRUE:
      RUN get-num-records IN WIDGET-HANDLE(char-hdl) (lv-rowid, OUTPUT lv-num-rec). /* not to get error 2108 4 times*/
      IF lv-num-rec NE 0 THEN DO:
        IF lv-num-rec EQ ? THEN
          RUN dispatch IN WIDGET-HANDLE(char-hdl) ('get-prev').
        ELSE
          RUN dispatch IN WIDGET-HANDLE(char-hdl) ('get-next').           
        IF NOT AVAIL eb OR eb.part-no NE "" THEN LEAVE.
      END.
      ELSE DO:
          /*LEAVE.       */
          RUN first-run IN WIDGET-HANDLE(char-hdl).
      END.
    END.
  END.

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
                                 
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
     IF NOT AVAIL eb THEN RETURN.

     FIND style WHERE style.company = gcompany AND
                      style.style = eb.style:screen-value IN BROWSE {&browse-name}
                      NO-LOCK NO-ERROR.   
     lv-foam = IF AVAIL style AND style.type = "F" THEN YES ELSE NO.

  DO WITH FRAME {&FRAME-NAME}:
     IF lv-label[01] EQ "" THEN
       ASSIGN
        lv-label[01] = est-qty.eqty:LABEL IN BROWSE {&browse-name}
        lv-label[02] = eb.yld-qty:LABEL IN BROWSE {&browse-name}.
    
     IF AVAIL est AND est.est-type EQ 8 THEN
       ASSIGN
        est-qty.eqty:LABEL IN BROWSE {&browse-name} = "Req Qty"
        eb.yld-qty:LABEL IN BROWSE {&browse-name}   = "Yield Qty".
     ELSE
       ASSIGN
        est-qty.eqty:LABEL IN BROWSE {&browse-name} = lv-label[01]
        eb.yld-qty:LABEL IN BROWSE {&browse-name}   = lv-label[02].
    
     IF AVAIL style AND style.TYPE = 'P' THEN
        eb.dep:LABEL IN BROWSE {&browse-name} = "Height".
     ELSE
        eb.dep:LABEL IN BROWSE {&browse-name} = "Depth".

     IF ecbrowse-chr = "Partitions" THEN
     DO:
        ASSIGN
           eb.wid:LABEL IN BROWSE {&browse-name} = "Height".
           eb.dep:LABEL IN BROWSE {&browse-name} = "Slot".
  
        BROWSE {&BROWSE-NAME}:MOVE-COLUMN(24,19).
     END.

     IF v-cecscrn-dec THEN
        ASSIGN
           eb.wid:WIDTH IN BROWSE {&browse-name} = 13
           eb.len:WIDTH IN BROWSE {&browse-name} = 13
           eb.dep:WIDTH IN BROWSE {&browse-name} = 13.


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
  DEF BUFFER b-eb FOR eb.
  DEF VAR li-cnt AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  IF CAN-DO(",form,blank",ls-add-what) THEN
  DO:
     {custom/checkuse.i}
  END.

  DO WITH FRAME {&FRAME-NAME}:
     /* move cursor to left end */
     DO li-cnt = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
        APPLY "cursor-left" TO {&BROWSE-NAME}.
     END.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN release-shared-buffers.

  DO WITH FRAME {&FRAME-NAME}:
    FOR EACH b-eb
        WHERE b-eb.company EQ est-qty.company
          AND b-eb.est-no  EQ est-qty.est-no
          AND b-eb.eqty    EQ est-qty.eqty
          AND b-eb.form-no NE 0
        NO-LOCK BY b-eb.form-no BY b-eb.blank-no:

        LEAVE.
    END.
    
  /*  IF AVAIL eb  AND adm-brs-in-update  AND NOT adm-new-record  /*update mode*/
      AND eb.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} = ""  THEN
    DO:
      MESSAGE "Estimate being created by another user. Cannot Update."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.       
      RETURN "ADM-ERROR":U.
    END. */

    IF AVAIL eb AND AVAIL b-eb AND ROWID(b-eb) EQ ROWID(eb) THEN
       APPLY "entry" TO eb.cust-no IN BROWSE {&browse-name}.
    ELSE IF AVAIL eb THEN
       APPLY "entry" TO eb.part-no IN BROWSE {&browse-name}.
  END.

  IF AVAIL est-qty THEN
  DO li-cnt = 1 TO 20:
    ASSIGN
     lv-copy-qty[li-cnt]  = est-qty.qty[li-cnt]
     lv-copy-pr[li-cnt]   = est-qty.qty-price[li-cnt]
     lv-copy-uom[li-cnt]  = est-qty.qty-uom[li-cnt]
     lv-copy-date[li-cnt] = est-qty.qty-date[li-cnt].
  END.

  RUN set-panel (0).

  EMPTY TEMP-TABLE tt-eb-set.
  EMPTY TEMP-TABLE tt-eb-set-part.

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
  {methods/winReSizeLocInit.i}

  FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                        AND sys-ctrl.name    EQ "CE W>L"
       NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
     CREATE sys-ctrl.
     ASSIGN sys-ctrl.company = cocode
            sys-ctrl.name    = "CE W>L"
            sys-ctrl.descrip = "Default to display Warning when Carton Width > Length."
            sys-ctrl.log-fld = YES.
     MESSAGE sys-ctrl.descrip
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
         UPDATE sys-ctrl.log-fld.
  END.
  
  ASSIGN
   ll-warn = sys-ctrl.log-fld .
  IF AVAIL eb THEN
      ASSIGN
      lv-eb-recid = RECID(eb)
      lv-ef-recid = RECID(ef).  

  IF AVAIL eb AND ecbrowse-chr = "Partitions" THEN
  DO:
     ASSIGN
        eb.wid:LABEL IN BROWSE {&browse-name} = "Height".
        eb.dep:LABEL IN BROWSE {&browse-name} = "Slot".
  
     BROWSE {&BROWSE-NAME}:MOVE-COLUMN(24,19).
  END.

  IF v-cecscrn-dec AND AVAIL eb THEN
     ASSIGN
        eb.wid:WIDTH IN BROWSE {&browse-name} = 15
        eb.len:WIDTH IN BROWSE {&browse-name} = 15
        eb.dep:WIDTH IN BROWSE {&browse-name} = 15.

  RUN get-last-est-cat.
  IF AVAIL eb THEN
      v-rowid-eb  = ROWID(eb) .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCounter AS INTEGER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  lv-repo = "OFF".

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF NUM-RESULTS("{&BROWSE-NAME}":U) > 0 THEN RUN dispatch ('get-last':U).
  IF AVAIL eb THEN
    ASSIGN
     lv-last-rowid  = ROWID(eb)
     lv-last-rowid2 = ROWID(ef).
    
  IF NUM-RESULTS("{&BROWSE-NAME}":U) > 0 THEN RUN dispatch ('get-first':U).
  IF AVAIL eb THEN
    ASSIGN
     lv-frst-rowid  = ROWID(eb)
     lv-frst-rowid2 = ROWID(ef).

  RUN setFarmTab.

  lv-repo = "ON".
  
  IF v-cecscrn-dec AND AVAIL eb THEN
     ASSIGN
        eb.wid:WIDTH IN BROWSE {&browse-name} = 15
        eb.len:WIDTH IN BROWSE {&browse-name} = 15
        eb.dep:WIDTH IN BROWSE {&browse-name} = 15.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-changed B-table-Win 
PROCEDURE local-row-changed :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-changed':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN custom-row-changed. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR ll-dumb AS LOG NO-UNDO.
  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR li-row-num AS INT NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-ef-rowid AS ROWID NO-UNDO.
  DEF BUFFER bf-e-itemfg-vend FOR e-itemfg-vend.
  DEF VAR lv-new-record AS LOG NO-UNDO.
  DEF VAR lActive AS LOG NO-UNDO.
  DEF VAR old-cat-no LIKE eb.procat NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-est-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-cust-user NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  IF NOT lv-foam THEN
  RUN check-flute-test-change.
  /* == validation ===== */
/*   IF eb.stock-no:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN DO:

       RUN fg/GetItemfgActInact.p (INPUT g_company,
                                   INPUT eb.stock-no:SCREEN-VALUE,
                                   OUTPUT lActive).
       IF NOT lActive THEN DO:
           MESSAGE eb.stock-no:SCREEN-VALUE + " has InActive Status. Order cannot be placed for the Inactive Item."
                VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO eb.stock-no.
           RETURN ERROR.
        END.
    END.*/ /* Ticket 22498  */

     RUN blank-cp (NO).

     RUN valid-part-no NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     IF int(est-qty.eqty:screen-value IN BROWSE {&browse-name}) <= 0 THEN DO:
        MESSAGE "Quantity must be entered. " VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO est-qty.eqty.
        RETURN NO-APPLY.
     END.

     RUN valid-style NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-stock-no NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     IF /* eb.cust-no:screen-value <> "" and */
        NOT CAN-FIND(cust WHERE cust.company = gcompany AND cust.cust-no = eb.cust-no:screen-value)
     THEN DO:
        MESSAGE "Invalid Customer Number. Try Help." VIEW-AS ALERT-BOX ERROR.
        APPLY "Entry" TO eb.cust-no.
        RETURN NO-APPLY.
     END.

     RUN valid-ship-id NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-flute NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-test NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
     
     IF ef.board:screen-value IN BROWSE {&browse-name} <> "" AND
        NOT CAN-FIND(item WHERE item.company = gcompany
                     AND item.i-no = ef.board:screen-value IN BROWSE {&browse-name} )
     THEN DO:
        MESSAGE "Invalid Board. Try Help. " VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO ef.board.
        RETURN NO-APPLY.
     END.

     RUN valid-procat NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-wid-len (eb.wid:HANDLE) NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     IF DECIMAL(eb.len:screen-value) = 0 THEN DO:
        MESSAGE "Length can not be 0. " VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO eb.len.
        RETURN NO-APPLY.
     END.
     IF DECIMAL(eb.wid:screen-value) = 0 THEN DO:
        MESSAGE "Width can not be 0. " VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO eb.wid.
        RETURN NO-APPLY.
     END.

     IF DECIMAL(eb.wid:screen-value) - trunc(DECIMAL(eb.wid:screen-value),0) >= v-16-or-32 
     THEN DO:
          MESSAGE "Cannot have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                   VIEW-AS ALERT-BOX ERROR.
             APPLY "entry" TO eb.wid.
             RETURN NO-APPLY.
     END.
     IF DECIMAL(eb.len:screen-value) - trunc(DECIMAL(eb.len:screen-value),0) >= v-16-or-32 
     THEN DO:
            MESSAGE "Cannot have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                   VIEW-AS ALERT-BOX ERROR.
             APPLY "entry" TO eb.len.
             RETURN NO-APPLY.
     END.
     IF DECIMAL(eb.dep:screen-value) - trunc(DECIMAL(eb.dep:screen-value),0) >= v-16-or-32
     THEN DO:
          MESSAGE "Cannot have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                   VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO eb.dep.
          RETURN NO-APPLY.
     END.
     
  /* ====== end validation =======*/
  ASSIGN
   lv-rowid   = ROWID(est)
   li-row-num = BROWSE {&browse-name}:FOCUSED-ROW
   old-bl-qty = eb.bl-qty.

  /* === check record locked ==== */
  DO TRANSACTION:
    FIND CURRENT est EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    FIND CURRENT est-qty EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    FIND CURRENT ef EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    FIND CURRENT eb EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF NOT AVAIL est OR NOT AVAIL est-qty OR
       NOT AVAIL ef  OR NOT AVAIL eb      THEN DO:
       MESSAGE "Estimate Record is being changed by someone else, wait a moment and try again..." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
    FIND CURRENT est NO-LOCK NO-ERROR.
    FIND CURRENT est-qty NO-LOCK NO-ERROR.
    FIND CURRENT ef NO-LOCK NO-ERROR.
    FIND CURRENT eb NO-LOCK NO-ERROR.
    lv-ef-recid = RECID(ef).
  END.
  ASSIGN lv-ef-rowid = ROWID(ef)
         lv-new-record = adm-new-record 
         old-cat-no    = eb.procat .

  RUN release-shared-buffers.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  IF old-cat-no NE eb.procat THEN DO:
       FIND FIRST itemfg WHERE itemfg.company = cocode AND
                            itemfg.i-no = eb.stock-no NO-LOCK NO-ERROR.
       IF AVAIL itemfg AND itemfg.est-no EQ eb.est-no THEN DO:
           FOR EACH oe-prmtx
               WHERE oe-prmtx.company            EQ itemfg.company
               AND oe-prmtx.i-no               BEGINS itemfg.i-no
               AND SUBSTR(oe-prmtx.i-no,1,100) EQ itemfg.i-no
               EXCLUSIVE-LOCK:
               
               ASSIGN
                   SUBSTR(oe-prmtx.i-no,1,100) = STRING(itemfg.i-no,"X(100)")
                   oe-prmtx.procat             = itemfg.procat.
               
           END. /* Each oe-prmtx */
       END.
  END.

  /* Code placed here will execute AFTER standard behavior.    */
  RUN release-shared-buffers.
    
  RUN get-attribute ('adm-new-record') . 
  IF RETURN-VALUE = "Yes" THEN DO:
      RUN set-attribute-list ("ADM-NEW-RECORD=no":U).
  END.

  /* move cursor to left end */  
  DO WITH FRAME {&FRAME-NAME}:
     DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
        APPLY "cursor-left" TO {&BROWSE-NAME}.
     END.
  END.  

  RUN set-panel (1).

  RUN dispatch('open-query').

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'record-target':U,OUTPUT char-hdl).
  DO li = 1 TO NUM-ENTRIES(char-hdl):
    IF VALID-HANDLE(WIDGET-HANDLE(ENTRY(li,char-hdl))) THEN
      RUN dispatch IN WIDGET-HANDLE(ENTRY(li,char-hdl)) ("open-query"). 
  END.

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'form-blank-target':U,OUTPUT char-hdl).
  DO li = 1 TO NUM-ENTRIES(char-hdl):
     IF VALID-HANDLE(WIDGET-HANDLE(ENTRY(li,char-hdl))) THEN
        RUN dispatch IN WIDGET-HANDLE(ENTRY(li,char-hdl)) ("open-query"). 
  END.

  IF lv-eb-recid <> ? THEN DO:
      
     RUN dispatch ('get-first').
     REPOSITION br-estitm TO ROWID lv-ef-rowid NO-ERROR.
     RUN dispatch ('row-changed') NO-ERROR. 
  END.

  IF v-cecscrn-dec THEN
     ASSIGN
        eb.wid:WIDTH IN BROWSE {&browse-name} = 15
        eb.len:WIDTH IN BROWSE {&browse-name} = 15
        eb.dep:WIDTH IN BROWSE {&browse-name} = 15.

  ASSIGN ll-is-add-from-tool = NO  /* reset */
         adm-new-record = NO
         adm-adding-record = NO
         ll-new-record = NO
         ll-new-est = NO
         lv-copy-what = ""
         ll-is-copy-record = NO
         ll-add-set-part-2 = NO
         ll-add-set-part = NO
         ls-add-what = "".
 
  /* disable/enable FARM tab */
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE, "container-source", OUTPUT char-hdl).
  RUN disable-enable-farm IN WIDGET-HANDLE(char-hdl) (eb.pur-man) NO-ERROR.

  SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view B-table-Win 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN setFarmTab.
  IF AVAIL eb THEN
  v-rowid-eb  = ROWID(eb) .
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lookup-eb B-table-Win 
PROCEDURE lookup-eb :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR rowid-val AS ROWID NO-UNDO.


  rowid-val = ROWID(eb).

  RUN windows/l-esteb.w (cocode, locode, est.est-no, INPUT-OUTPUT rowid-val).

  RUN get-eb (rowid-val).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mass-delete B-table-Win 
PROCEDURE mass-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ls-delete AS CHAR NO-UNDO.
  DEF VAR li-delete AS INT NO-UNDO.

  DEF BUFFER b-eb FOR eb.


  RUN check-delete NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  FOR EACH tt-eb:
    DELETE tt-eb.
  END.
  ll-mass-del = NO.

  IF AVAIL eb THEN RUN est/d-masdel.w (OUTPUT ls-delete).

  li-delete = LOOKUP(ls-delete,"est,form,blank").

  IF li-delete GT 0 THEN DO:
    ll-mass-del = YES.

    FOR EACH b-eb
        WHERE b-eb.company EQ eb.company
          AND b-eb.est-no  EQ eb.est-no
          AND (li-delete   EQ 1                                 OR
               (li-delete  EQ 2 AND b-eb.form-no EQ eb.form-no) OR
               (li-delete  EQ 3 AND ROWID(b-eb) EQ ROWID(eb)))
        NO-LOCK:
      CREATE tt-eb.
      BUFFER-COPY b-eb TO tt-eb
      ASSIGN
       tt-eb.row-id = ROWID(b-eb).
    END.

    FOR EACH tt-eb:
      RUN repo-query (tt-eb.row-id).
      IF AVAIL eb AND eb.est-no EQ tt-eb.est-no            AND
         (eb.form-no EQ tt-eb.form-no OR li-delete LT 2)   AND
         (eb.blank-no EQ tt-eb.blank-no OR li-delete LT 3) THEN
        RUN dispatch ("delete-record").
    END.  
  END.

  ll-mass-del = NO.
  FOR EACH tt-eb:
    DELETE tt-eb.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE navigate-browser B-table-Win 
PROCEDURE navigate-browser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER ip-nav-type AS CHAR.
  DEF OUTPUT PARAMETER op-nav-type AS CHAR.
 
 
  IF ip-nav-type NE "" THEN
  CASE ip-nav-type:
    WHEN "F" THEN RUN dispatch ('get-first':U).
    WHEN "L" THEN RUN dispatch ('get-last':U).
    WHEN "N" THEN RUN dispatch ('get-next':U).
    WHEN "P" THEN RUN dispatch ('get-prev':U).
    WHEN "G" THEN RUN lookup-eb.
  END CASE.
    
  IF ROWID(eb) EQ lv-last-rowid THEN
    op-nav-type = "L".
      
  IF ROWID(eb) EQ lv-frst-rowid THEN
    op-nav-type = IF op-nav-type EQ "L" THEN "B" ELSE "F".
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE navigate-browser2 B-table-Win 
PROCEDURE navigate-browser2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER ip-nav-type AS CHAR.
  DEF OUTPUT PARAMETER op-nav-type AS CHAR.
  
  DEF VAR hld-rowid AS ROWID NO-UNDO.

  hld-rowid = ROWID(ef).

  IF ip-nav-type NE "" THEN
  CASE ip-nav-type:
    WHEN "F" THEN RUN dispatch ('get-first':U).
    WHEN "L" THEN RUN dispatch ('get-last':U).
    WHEN "N" THEN DO WHILE ROWID(ef) EQ hld-rowid:
                    RUN dispatch ('get-next':U).
                  END.
    WHEN "P" THEN DO WHILE ROWID(ef) EQ hld-rowid:
                    RUN dispatch ('get-prev':U).
                  END.
    WHEN "G" THEN RUN lookup-eb.
  END CASE.
    
  IF ROWID(ef) EQ lv-last-rowid2 THEN
    op-nav-type = "L".
      
  IF ROWID(ef) EQ lv-frst-rowid2 THEN
    op-nav-type = IF op-nav-type EQ "L" THEN "B" ELSE "F".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-board B-table-Win 
PROCEDURE new-board :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST item NO-LOCK
        WHERE item.company EQ gcompany
          AND item.i-no    EQ ef.board:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-ERROR.
    IF AVAIL item THEN
       ASSIGN
         ef.cal:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(item.cal)
         eb.flute:SCREEN-VALUE IN BROWSE {&browse-name} = ITEM.flute
         eb.test:SCREEN-VALUE IN BROWSE {&browse-name} = ITEM.reg-no.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-flute-test B-table-Win 
PROCEDURE new-flute-test :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-mat-types AS CHAR INIT "B" NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    IF lv-foam THEN lv-mat-types = "1,2,3,4".

    FOR EACH item
        WHERE item.company   EQ gcompany
          AND CAN-DO(lv-mat-types,item.mat-type)
          AND item.industry  EQ "2"
          AND item.i-code    EQ "E"
          AND item.flute     EQ eb.flute:SCREEN-VALUE IN BROWSE {&browse-name}
          AND item.reg-no    EQ eb.test:SCREEN-VALUE IN BROWSE {&browse-name}
        USE-INDEX mat-type NO-LOCK
        BY item.i-no:
      ef.board:SCREEN-VALUE IN BROWSE {&browse-name} = item.i-no.
      RUN new-board.
      LEAVE.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-ship-id B-table-Win 
PROCEDURE new-ship-id :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&frame-name}:
    FIND shipto
        WHERE shipto.company EQ gcompany
          AND shipto.cust-no EQ eb.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND shipto.ship-id BEGINS eb.ship-id:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF AVAIL shipto THEN eb.ship-id:SCREEN-VALUE IN BROWSE {&browse-name} = shipto.ship-id.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateFormFromImport B-table-Win 
PROCEDURE pCreateFormFromImport :
/*------------------------------------------------------------------------------
 Purpose: Processes ttInputEst temp-table, adding forms to the estimate in context
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
  DEFINE VARIABLE lDummy AS LOGICAL NO-UNDO.
   
  ASSIGN
    ll-new-record = YES
    iCount = 0
    .

  FOR EACH ttInputEst:
      ttInputEst.riParentEst = ROWID(est).
      iCount = iCount + 1.
  END.
  
  RUN est/BuildEstimate.p ("C").
  
  RUN dispatch('open-query').
    lDummy = {&browse-name}:REFRESH() IN FRAME {&FRAME-NAME}.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE redisplay-blanks B-table-Win 
PROCEDURE redisplay-blanks :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.


  RUN dispatch ("open-query").

  DO WHILE ROWID(eb) NE ip-rowid:
    RUN dispatch ("get-next").
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
  RELEASE xqty.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-for-est3 B-table-Win 
PROCEDURE repo-for-est3 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.


  RUN get-eb (ip-rowid).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-on-off B-table-Win 
PROCEDURE repo-on-off :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-on-off AS CHAR NO-UNDO.


  lv-repo = ip-on-off.

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
  

  IF NOT AVAIL eb OR ROWID(eb) NE ip-rowid THEN DO WITH FRAME {&FRAME-NAME}:
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN RUN dispatch ("row-changed").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reset-est-type B-table-Win 
PROCEDURE reset-est-type :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER op-est-type AS INT NO-UNDO.

  DEF BUFFER bf-eb FOR eb.  
  DEF BUFFER bf-ef FOR ef.
  DEF BUFFER bf-est FOR est.
  DEF BUFFER bf-set FOR eb.

  DEF VAR li-form-no AS INT NO-UNDO.
  DEF VAR li-blank-no AS INT NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.
  DEF VAR v-set-header AS LOG NO-UNDO.

  FIND bf-est WHERE ROWID(bf-est) EQ ROWID(est).

  ASSIGN
   op-est-type = bf-est.est-type
   li-form-no  = 0
   li-blank-no = 0.

  FOR EACH bf-ef
      WHERE bf-ef.company EQ bf-est.company 
        AND bf-ef.est-no  EQ bf-est.est-no
      NO-LOCK:
    li-form-no = li-form-no + 1.
    FOR EACH bf-eb OF bf-ef NO-LOCK:
      li-blank-no = li-blank-no + 1.
    END.
  END.

  IF op-est-type EQ 5 THEN
    IF (li-form-no NE 1 OR li-blank-no NE 1) THEN DO:
      ASSIGN
       op-est-type = 8
       eb.yld-qty  = eb.bl-qty.

      FOR EACH bf-eb OF bf-est WHERE ROWID(eb) NE ROWID(bf-eb):
        RUN set-yld-qty (ROWID(bf-eb)).
      END.
    END.

  IF (op-est-type EQ 6 AND li-form-no EQ 1 AND li-blank-no EQ 1) OR
     (op-est-type EQ 5 AND eb.quantityPerSet GE 2) THEN DO:
    /*MESSAGE "Is this estimate a two piece box set?"
            VIEW-AS ALERT-BOX QUESTION
            BUTTON YES-NO UPDATE ll.
    IF ll THEN DO:
      RUN update-set.*/
      IF op-est-type EQ 5 THEN DO:
        op-est-type = 6.
        FOR EACH bf-eb
            WHERE bf-eb.company  EQ bf-est.company
              AND bf-eb.est-no   EQ bf-est.est-no
              AND bf-eb.blank-no GT 0:
          bf-eb.bl-qty = bf-est.est-qty[1].
        END.
      END.

      ELSE
      IF NOT adm-new-record AND eb.quantityPerSet EQ 1 THEN DO:
        ll = ll-mass-del.
        IF NOT ll THEN
          MESSAGE "Change back to a single item estimate? "
              VIEW-AS ALERT-BOX QUESTION
              BUTTON YES-NO UPDATE ll.
        IF ll THEN op-est-type = 5.
      END.
    /*END.*/
  END.
  
  ELSE
  IF op-est-type GE 7 THEN
  DO:
     v-set-header = CAN-FIND(FIRST bf-set WHERE
                                   bf-set.company EQ bf-est.company AND
                                   bf-set.est-no  EQ bf-est.est-no AND
                                   bf-set.form-no EQ 0).

     FOR EACH bf-eb
         WHERE bf-eb.company EQ bf-est.company
           AND bf-eb.est-no  EQ bf-est.est-no
           AND bf-eb.form-no NE 0:
     
        IF v-set-header THEN
           ASSIGN
              bf-eb.bl-qty  = est-qty.eqty * bf-eb.quantityPerSet
              bf-eb.yld-qty = bf-eb.bl-qty.
        ELSE
           IF bf-eb.yld-qty LE 1 THEN bf-eb.yld-qty = bf-eb.bl-qty.
     END.

     IF v-set-header THEN
     DO:
        FIND FIRST bf-set WHERE
             bf-set.company EQ bf-est.company AND
             bf-set.est-no  EQ bf-est.est-no AND
             bf-set.form-no EQ 0
             NO-ERROR.

        IF AVAIL bf-set THEN
           DELETE bf-set.
     END.
  END.

  ELSE
  IF op-est-type EQ 5 THEN
  FOR EACH bf-eb
      WHERE bf-eb.company EQ bf-est.company
        AND bf-eb.est-no  EQ bf-est.est-no:
    bf-eb.quantityPerSet = 1.
  END.
  
  IF op-est-type <> ? THEN DO:  
    bf-est.est-type = op-est-type.
    FOR EACH bf-ef
        WHERE bf-ef.company EQ bf-est.company 
          AND bf-ef.est-no  EQ bf-est.est-no:        
      FOR EACH bf-eb OF bf-ef:                    
        bf-eb.est-type = op-est-type.
      END.
      bf-ef.est-type = op-est-type.
    END.
  END.

  RUN est/resetops.p (ROWID(bf-est)).

  FIND CURRENT bf-est NO-LOCK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-goto B-table-Win 
PROCEDURE run-goto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR ll AS LOG INIT NO NO-UNDO.
  DEF VAR li-est-type AS INT NO-UNDO.
  DEF VAR lv-changed-to-page-six AS LOG NO-UNDO.
  DEF VAR op-changed AS LOG NO-UNDO.

  DEF BUFFER b-eb FOR eb.


  IF AVAIL est THEN DO:
    IF est.est-type EQ 5 THEN DO:
      MESSAGE "Utilize Request/Yield Qty Pricing?" SKIP
              "(Single to Tandem/Combo Estimate Type)"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll.
      IF ll THEN DO TRANSACTION:
        FIND CURRENT est NO-ERROR.
        IF AVAIL est THEN DO:
          est.est-type = 8.
          RUN reset-est-type (OUTPUT li-est-type).
        END.
        FIND CURRENT est NO-LOCK NO-ERROR.
      END.
    END.

    IF est.est-type GT 5 THEN DO:
      lv-rowid = ROWID(eb).

      /*RUN ce/com/istandem.p (lv-rowid, OUTPUT ll).

      IF ll THEN*/
        RUN est/d-multbl.w (INPUT-OUTPUT lv-rowid, OUTPUT op-changed).
      /*ELSE
        RUN est/d-multib.w (INPUT-OUTPUT lv-rowid).*/

      IF est.est-type EQ 8 THEN
      FOR EACH b-eb
          WHERE b-eb.company EQ est-qty.company
            AND b-eb.est-no  EQ est-qty.est-no
            AND b-eb.eqty    EQ est-qty.eqty
          NO-LOCK BREAK BY b-eb.est-no:
        IF FIRST(b-eb.est-no) AND LAST(b-eb.est-no) AND
           b-eb.bl-qty EQ b-eb.yld-qty              THEN DO TRANSACTION:
          MESSAGE "Change this Tandem/Combo " +
                  (IF ll THEN "back " ELSE "") +
                  "to a Single Estimate Type?" SKIP
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE ll.
          IF ll THEN DO:
            FIND CURRENT est NO-ERROR.
            FIND CURRENT eb NO-ERROR.
            IF AVAIL est AND AVAIL eb THEN DO:
              ASSIGN
               est.est-type = 5
               eb.yld-qty   = 1.
              RUN reset-est-type (OUTPUT li-est-type).
            END.
            FIND CURRENT eb NO-LOCK NO-ERROR.
            FIND CURRENT est NO-LOCK NO-ERROR.
          END.
        END.
      END.

      RUN redisplay-blanks (lv-rowid).

      IF op-changed THEN
      DO:
         RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"route-target",OUTPUT char-hdl).
        
         IF NOT VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
            /*change page to 6 to initialize object*/
            lv-changed-to-page-six = YES.
        
            RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"Container-source",OUTPUT char-hdl).
            RUN select-page IN WIDGET-HANDLE(char-hdl) (6).
            RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"route-target",OUTPUT char-hdl).
         END.
        
         RUN build-route IN WIDGET-HANDLE(char-hdl).
        
         IF lv-changed-to-page-six THEN
         DO:
            RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"Container-source",OUTPUT char-hdl).
            RUN select-page IN WIDGET-HANDLE(char-hdl) (2).
         END.
      END.

      RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"form-blank-target",OUTPUT char-hdl).

      IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
        RUN dispatch IN WIDGET-HANDLE(char-hdl) ("open-query").
        RUN repo-on-off IN WIDGET-HANDLE(char-hdl) ("ON").
        RUN custom-row-changed.
      END.
    END.
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
  {src/adm/template/snd-list.i "est"}
  {src/adm/template/snd-list.i "est-qty"}
  {src/adm/template/snd-list.i "ef"}
  {src/adm/template/snd-list.i "eb"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-auto-add-item B-table-Win 
PROCEDURE set-auto-add-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR lv-i-no LIKE eb.stock-no NO-UNDO.
  DEF VAR lv-num-created AS INT NO-UNDO.
  DEF VAR lvr-eb AS ROWID NO-UNDO.
  DEF BUFFER bf-est FOR est.
  DEF VAR l-est-type AS INT NO-UNDO.

  
  GET FIRST {&browse-name}.
  IF AVAIL eb THEN DO:
      FIND FIRST xest WHERE xest.company = eb.company
                        AND xest.est-no = eb.est-no
                      NO-LOCK NO-ERROR.
  END.
  IF NOT AVAIL xeb AND avail(eb) THEN
    FIND xeb WHERE ROWID(xeb) = ROWID(eb) NO-LOCK NO-ERROR.

  IF NOT AVAIL xest THEN
      RETURN.
  IF xest.est-type EQ 2 OR xest.est-type EQ 6 THEN DO:
    lv-num-created = lv-num-created + 1.    
    RUN auto-create-item (INPUT lv-i-no).    
  END.

  /* Process regular items */
  ELSE DO WHILE AVAIL(eb) /* AND eb.stock-no = ""*/:

      FIND bf-est WHERE bf-est.company EQ eb.company
                    AND bf-est.est-no  EQ eb.est-no
                  NO-LOCK NO-ERROR.
       IF AVAIL bf-est THEN
         l-est-type = bf-est.est-type.

       IF eb.stock-no GT "" THEN DO:
          GET NEXT {&browse-name}.
          NEXT.
       END.       

       IF v-est-fg THEN lv-i-no = eb.part-no.
       ELSE
       IF v-est-fg1 NE "Manual" THEN DO:
         FIND FIRST itemfg
             WHERE itemfg.company EQ eb.company
               AND itemfg.part-no EQ eb.part-no
               AND itemfg.cust-no EQ eb.cust-no
             NO-LOCK NO-ERROR.
         IF AVAIL itemfg THEN
           ASSIGN
            lv-i-no                         = itemfg.i-no.
       END.


      FIND xeb WHERE ROWID(xeb) = ROWID(eb) EXCLUSIVE-LOCK.
      FIND xef WHERE ROWID(xef) = ROWID(ef) NO-LOCK.
      FIND FIRST xest WHERE xest.company = xeb.company
                        AND xest.est-no  = xeb.est-no
                      NO-LOCK NO-ERROR.
                                                
      lv-num-created = lv-num-created + 1.   
      RUN auto-create-item (INPUT lv-i-no).      
      FIND FIRST tt-stock-no WHERE tt-stock-no.eb-row-id = ROWID(xeb)
                             NO-ERROR.      
      IF AVAIL tt-stock-no THEN DO:
        xeb.stock-no = tt-stock-no.stock-no.                
        RUN fg/ce-addfg.p (xeb.stock-no).
      END.

      RUN dispatch ("row-changed").
      GET NEXT {&browse-name}.

  END.

  GET FIRST {&browse-name}.
  IF AVAIL eb THEN
      FIND bf-est WHERE bf-est.company EQ eb.company
                    AND bf-est.est-no  EQ eb.est-no
                  NO-LOCK NO-ERROR.
  IF AVAIL bf-est THEN
      l-est-type = bf-est.est-type.

  /* To cover set header */
  FOR EACH xeb WHERE xeb.company = eb.company 
                 AND xeb.est-no  = eb.est-no
                 AND xeb.stock-no = ""
                 AND (xeb.form-no = 0 
        /* OR (l-est-type NE 2 AND l-est-type NE 6) */)
               NO-LOCK.

      IF v-est-fg THEN lv-i-no = xeb.part-no.
      ELSE
      IF v-est-fg1 NE "Manual" THEN DO:
        FIND FIRST itemfg
            WHERE itemfg.company EQ xeb.company
              AND itemfg.part-no EQ xeb.part-no
              AND itemfg.cust-no EQ xeb.cust-no
            NO-LOCK NO-ERROR.
        IF AVAIL itemfg THEN
          ASSIGN
           lv-i-no                         = itemfg.i-no.
      END.
      FIND FIRST xef WHERE xef.company = xeb.company
                       AND xef.est-no  = xef.est-no
                     NO-LOCK NO-ERROR.
      FIND FIRST xest WHERE xest.company = xeb.company
                        AND xest.est-no  = xeb.est-no
                      NO-LOCK NO-ERROR.
      lv-num-created = lv-num-created + 1.
             
      RUN auto-create-item (INPUT lv-i-no).      

      FIND FIRST tt-stock-no WHERE tt-stock-no.eb-row-id = ROWID(xeb)
                             NO-ERROR.      
      IF AVAIL tt-stock-no THEN DO:
        xeb.stock-no = tt-stock-no.stock-no.        
        RUN fg/ce-addfg.p (xeb.stock-no).
      END.

  END.

  /* Create the newly assigned itemfg records */
  IF AVAIL bf-est THEN DO:

      GET FIRST {&browse-name}.
    
      FOR EACH xeb WHERE xeb.company = eb.company 
                     AND xeb.est-no  = eb.est-no
                     AND xeb.stock-no = ""
                   EXCLUSIVE-LOCK.
          FIND FIRST tt-stock-no WHERE tt-stock-no.eb-row-id = ROWID(xeb)
                                 NO-ERROR.      
          IF AVAIL tt-stock-no THEN
            xeb.stock-no = tt-stock-no.stock-no.
          
          FIND FIRST itemfg WHERE itemfg.company EQ xeb.company
                              AND itemfg.i-no EQ xeb.stock-no
                            NO-LOCK NO-ERROR.          
          IF NOT AVAIL itemfg THEN
            RUN fg/ce-addfg.p (xeb.stock-no).
      END.
  END.

  IF lv-num-created GT 0 THEN    

    RUN local-open-query.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-hold-values B-table-Win 
PROCEDURE set-hold-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     lv-hold-flute = eb.flute:SCREEN-VALUE IN BROWSE {&browse-name}
     lv-hold-test  = eb.test:SCREEN-VALUE IN BROWSE {&browse-name}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-lv-foam B-table-Win 
PROCEDURE set-lv-foam :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND style WHERE style.company = gcompany AND
             style.style = eb.style:screen-value IN BROWSE {&browse-name}
           NO-LOCK NO-ERROR.   

lv-foam = IF AVAIL style AND style.type = "F" THEN 
          YES ELSE NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-or-combo B-table-Win 
PROCEDURE set-or-combo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-eb FOR eb.

  DEF VAR ld AS DEC NO-UNDO.
       
  ASSIGN
     ll-add-set = NO
        /*ll-add-set-part = NO*/ .

  IF est.est-type NE 8                               OR
     CAN-FIND(b-eb WHERE b-eb.company EQ est.company
                     AND b-eb.est-no  EQ est.est-no) THEN DO:

    ll-add-set = NOT CAN-FIND(FIRST b-eb WHERE b-eb.company EQ est.company
                                           AND b-eb.est-no  EQ est.est-no
                                           AND b-eb.form-no EQ 0).

    IF ll-add-set AND est.est-type NE 6 THEN DO:
      MESSAGE "Is this estimate becoming a Set?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll-add-set.

      FIND CURRENT est.
      est.est-type = IF ll-add-set THEN 6 ELSE 8.
      FIND CURRENT est NO-LOCK.

      FOR EACH b-eb
          WHERE b-eb.company EQ est.company
            AND b-eb.est-no  EQ est.est-no
            AND b-eb.form-no NE 0:
        IF est.est-type EQ 8 THEN DO:
          ASSIGN
           b-eb.bl-qty = est-qty.eqty
           ld          = est-qty.eqty / b-eb.num-up.
          {sys/inc/roundup.i ld}
          b-eb.yld-qty = ld * b-eb.num-up.
        END.
        ELSE
        IF est.est-type EQ 6 THEN b-eb.quantityPerSet = 1.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-panel B-table-Win 
PROCEDURE set-panel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-switch AS INT NO-UNDO.

  DEF VAR char-hdl AS CHAR NO-UNDO.


  RUN get-link-handle IN adm-broker-hdl  (THIS-PROCEDURE,'set-goto-target':U,OUTPUT char-hdl).
  IF ip-switch EQ 0 THEN 
    RUN disable-all IN WIDGET-HANDLE(char-hdl).
  ELSE
    RUN enable-all IN WIDGET-HANDLE(char-hdl).

  RUN get-link-handle IN adm-broker-hdl  (THIS-PROCEDURE,'next-prev-target':U,OUTPUT char-hdl).
  IF ip-switch EQ 0 THEN 
    RUN disable-all IN WIDGET-HANDLE(char-hdl).
  ELSE
    RUN dispatch IN WIDGET-HANDLE(char-hdl) ("initialize").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-yld-qty B-table-Win 
PROCEDURE set-yld-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

  DEF VAR v-qty AS DEC NO-UNDO.

  DEF BUFFER b-eb  FOR eb.
  DEF BUFFER b-eb1 FOR eb.
  DEF BUFFER b-ef  FOR ef.

  FIND b-eb WHERE ROWID(b-eb) EQ ip-rowid EXCLUSIVE NO-ERROR.

  IF AVAIL b-eb THEN
  DO:
     rpt-loop:
     REPEAT:
     
        FIND FIRST b-ef OF b-eb EXCLUSIVE NO-ERROR NO-WAIT.
        
        IF AVAIL b-ef THEN DO:
           IF b-eb.blank-no EQ 1 THEN
             ASSIGN
              b-eb.yld-qty = b-eb.bl-qty
              v-qty        = b-eb.yld-qty / b-eb.num-up.
          
           ELSE
           DO:
           loop2:
           FOR EACH b-eb1 OF b-ef
               WHERE ROWID(b-eb1) NE ROWID(b-eb)
               BY b-eb1.yld-qty / b-eb1.num-up DESC:
             v-qty = b-eb1.yld-qty / b-eb1.num-up.
             LEAVE loop2.
           END.
           END.
          
           {sys/inc/roundup.i v-qty}
          
           ASSIGN
            b-eb.die-in  = b-eb.die-in / b-eb.num-up
            b-eb.num-up  = TRUNC(b-eb.bl-qty / v-qty,0) +
                           INT(b-eb.bl-qty MODULO v-qty GT 0)
            b-eb.die-in  = b-eb.die-in * b-eb.num-up
            b-eb.yld-qty = v-qty * b-eb.num-up
            b-ef.die-in  = 0.
          
           IF b-eb.die-in EQ ? THEN b-eb.die-in = 0.
          
           FOR EACH b-eb1 OF b-ef NO-LOCK:
             b-ef.die-in = b-ef.die-in + b-eb1.die-in.
           END.
          
           IF b-eb.num-wid * b-eb.num-len NE b-eb.num-up THEN
             ASSIGN
              b-eb.num-wid = b-eb.num-up
              b-eb.num-len = 1.

           FIND CURRENT b-ef NO-LOCK.
           /*RELEASE b-ef.*/
           LEAVE rpt-loop.
        END.
     END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setFarmTab B-table-Win 
PROCEDURE setFarmTab :
/*------------------------------------------------------------------------------
  Purpose:     disable/enable FARM tab
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT AVAILABLE eb THEN RETURN.
  {methods/run_link.i "CONTAINER-SOURCE" "get-attribute" "('current-page':U)"}
  IF INTEGER(RETURN-VALUE) EQ 2 THEN
  {methods/run_link.i "CONTAINER-SOURCE" "disable-enable-farm" "(eb.pur-man)"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tokenize-proc B-table-Win 
PROCEDURE tokenize-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ip-string AS CHAR NO-UNDO.

DEF VAR lv-tmp AS CHAR NO-UNDO.
DEF VAR lv-tmp-val AS CHAR NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.

v-l-array = 0.

DO i = 1 TO LENGTH(ip-string):
   lv-tmp-val = SUBSTRING(ip-string,i,1).
   IF lv-tmp-val <> " " THEN
      lv-tmp = lv-tmp + lv-tmp-val.
   ELSE IF lv-tmp <> "" THEN
      ASSIGN
         v-count = v-count + 1
         v-l-array[v-count] = dec(lv-tmp)
         lv-tmp = "".
END.

IF lv-tmp <> "" THEN
   ASSIGN
      v-count = v-count + 1
      v-l-array[v-count] = dec(lv-tmp).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE upd-fg-frt-class B-table-Win 
PROCEDURE upd-fg-frt-class :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER bf-eb FOR eb.
DEF INPUT PARAMETER v-eb-rowid AS ROWID NO-UNDO.
DEF INPUT PARAMETER v-board LIKE ef.board NO-UNDO.
FIND bf-eb WHERE ROWID(bf-eb) = v-eb-rowid NO-LOCK.


IF (est.est-type NE 4 AND est.est-type NE 8) THEN DO:
          
    /* Indicates to always copy estimate freight class from item */
    FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                          AND sys-ctrl.name    EQ "CEDeliveryZone" 
                        NO-LOCK NO-ERROR.
    IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "FreightClass" THEN DO:
      FIND FIRST ITEM WHERE ITEM.company = bf-eb.company
                        AND ITEM.i-no = v-board
                      NO-LOCK NO-ERROR. 
      IF AVAIL(ITEM) AND item.mat-type = "B" AND ITEM.spare-char-1 GT "" THEN DO:
        FIND FIRST carr-mtx WHERE carr-mtx.company  EQ bf-eb.company
                              AND carr-mtx.loc      EQ bf-eb.loc
                              AND carr-mtx.carrier  EQ bf-eb.carrier
                              AND carr-mtx.del-zone EQ ITEM.spare-char-1
                             NO-LOCK NO-ERROR.
        IF AVAIL carr-mtx THEN DO:
          ASSIGN bf-eb.dest-code = ITEM.spare-char-1.    
        END.
      END.
    END.
END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-e-itemfg-vend B-table-Win 
PROCEDURE update-e-itemfg-vend :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF BUFFER bf-e-itemfg-vend FOR e-itemfg-vend.
   DEF BUFFER bf-e-itemfg FOR e-itemfg.
   DEF VAR vcUOM AS cha NO-UNDO.
   DEF VAR i AS INT NO-UNDO.

   IF est.est-type > 5 THEN DO: /* set est - copy for all forms*/

    EMPTY TEMP-TABLE tt-e-vend .
    FOR EACH e-itemfg-vend NO-LOCK
                WHERE e-itemfg-vend.company EQ eb.company
                  AND e-itemfg-vend.est-no = eb.est-no
                  AND e-itemfg-vend.eqty = viEQtyPrev
            /* AND e-itemfg-vend.form-no = eb.form-no
             AND e-itemfg-vend.blank-no = eb.blank-no
             AND e-itemfg-vend.i-no    EQ eb.stock-no*/
            /*AND e-itemfg-vend.vend-no EQ ""*/  :

      CREATE tt-e-vend .
      e-vend-row = ROWID(e-itemfg-vend) .
    END.


    FOR EACH tt-e-vend:
       FIND e-itemfg-vend
           WHERE ROWID(e-itemfg-vend) EQ tt-e-vend.e-vend-row
           EXCLUSIVE-LOCK.

       CREATE bf-e-itemfg-vend.
       BUFFER-COPY e-itemfg-vend TO bf-e-itemfg-vend.
       ASSIGN bf-e-itemfg-vend.eqty = eb.eqty.       

       DELETE e-itemfg-vend.       

       /*FIND FIRST bf-e-itemfg WHERE
                 bf-e-itemfg.company EQ bf-e-itemfg-vend.company AND
                 bf-e-itemfg.i-no EQ bf-e-itemfg-vend.i-no
                 NO-LOCK NO-ERROR.

       IF AVAIL bf-e-itemfg THEN reftable.code2 = bf-e-itemfg.std-uom.*/
       FIND CURRENT bf-e-itemfg-vend NO-LOCK.
       RELEASE bf-e-itemfg-vend.
      END.
   END.   /* end of set est */
   ELSE FOR EACH e-itemfg-vend
                   WHERE e-itemfg-vend.company EQ eb.company
                     AND e-itemfg-vend.est-no = eb.est-no
                     AND e-itemfg-vend.eqty = viEQtyPrev
                     AND e-itemfg-vend.form-no = eb.form-no
                     AND e-itemfg-vend.blank-no = eb.blank-no
            /*AND e-itemfg-vend.i-no    EQ eb.stock-no*/
            /*AND e-itemfg-vend.vend-no EQ ""*/  :

       CREATE bf-e-itemfg-vend.
       BUFFER-COPY e-itemfg-vend TO bf-e-itemfg-vend.
       ASSIGN
            bf-e-itemfg-vend.eqty = eb.eqty
            bf-e-itemfg-vend.i-no = eb.stock-no
            .       

       DELETE e-itemfg-vend.       

       /*FIND FIRST bf-e-itemfg WHERE
                 bf-e-itemfg.company EQ bf-e-itemfg-vend.company AND
                 bf-e-itemfg.i-no EQ bf-e-itemfg-vend.i-no
                 NO-LOCK NO-ERROR.

       IF AVAIL bf-e-itemfg THEN reftable.code2 = bf-e-itemfg.std-uom.*/
       
       FIND CURRENT bf-e-itemfg-vend NO-LOCK.
       RELEASE bf-e-itemfg-vend.
   END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-sb-qtys B-table-Win 
PROCEDURE update-sb-qtys :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.
  
  DEF BUFFER b-est FOR est.
  DEF BUFFER b-ef  FOR ef.
  DEF BUFFER b-eb  FOR eb.
  DEF BUFFER b-ef2 FOR ef.

  FIND b-eb WHERE ROWID(b-eb) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL b-eb THEN DO TRANSACTION:
     FIND FIRST b-est
         WHERE b-est.company EQ b-eb.company
           AND b-est.est-no  EQ b-eb.est-no
         NO-ERROR.
    
     IF AVAIL b-est THEN
     FOR EACH b-ef
         WHERE b-ef.company EQ b-est.company
           AND b-ef.est-no  EQ b-est.est-no
         NO-LOCK
         BREAK BY b-ef.form-no:
    
        IF FIRST(b-ef.form-no) THEN b-est.form-qty = 0.
       
        b-est.form-qty = b-est.form-qty + 1.
       
        repeat-loop:
        REPEAT:
           FIND b-ef2 WHERE ROWID(b-ef2) EQ ROWID(b-ef)
                EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
       
           IF AVAIL b-ef2 THEN
           DO:
              FOR EACH b-eb
                  WHERE b-eb.company EQ b-ef.company
                    AND b-eb.est-no  EQ b-ef.est-no
                    AND b-eb.form-no EQ b-ef.form-no
                  NO-LOCK
                  BREAK BY b-eb.blank-no:
             
                  IF FIRST(b-eb.blank-no) THEN
                     b-ef2.blank-qty = 0.
             
                  b-ef2.blank-qty = b-ef2.blank-qty + 1.
              END.
             
              FIND CURRENT b-ef2 NO-LOCK.
              /*RELEASE b-ef2.*/
              LEAVE repeat-loop.
           END.
        END.
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-set B-table-Win 
PROCEDURE update-set :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-ord FOR oe-ord.
  DEF BUFFER bf-eb FOR eb.
  DEF BUFFER bf-ef FOR ef.
  DEF BUFFER bf-eb-header FOR eb.

  DEF VAR i AS INT NO-UNDO.
  DEF VAR lv-type LIKE est.est-type NO-UNDO.
  DEF VAR lv-old-type LIKE lv-type NO-UNDO.
  DEF VAR lv-yld-qty LIKE eb.yld-qty INIT 999999999 NO-UNDO.
  DEF VAR ld AS DEC NO-UNDO.
  DEF VAR v-assem-partition AS LOG NO-UNDO.

  ASSIGN
   i       = 0
   lv-type = est.est-type.

  FOR EACH bf-eb
      WHERE bf-eb.company EQ est.company
        AND bf-eb.est-no  EQ est.est-no
        AND bf-eb.form-no NE 0
      NO-LOCK:

    i = i + 1.
  END.

  IF est.ord-no GT 0 THEN
  FIND FIRST bf-ord
      WHERE bf-ord.company EQ est.company
        AND bf-ord.ord-no  EQ est.ord-no
        AND INDEX("CDZ",bf-ord.stat) EQ 0
      NO-LOCK NO-ERROR.

  IF NOT AVAIL bf-ord THEN RUN est/d-esttyp.w (ROWID(est), INPUT-OUTPUT lv-type).

  ELSE
  IF AVAIL bf-ord AND i LE 1 AND eb.yld-qty LE 1 THEN DO:
    MESSAGE "Sorry, Order must be closed before changing to a set..." 
        VIEW-AS ALERT-BOX ERROR.
    RETURN.
  END.

  IF lv-type = ? THEN
      RETURN NO-APPLY.
  lv-old-type = est.est-type.

  IF lv-type NE lv-old-type THEN DO:
    FIND CURRENT est.
    est.est-type = lv-type.
    RUN reset-est-type (OUTPUT lv-type).
    FIND CURRENT est NO-LOCK.
  END.
  
  IF est.est-type EQ 6 THEN DO:
     IF lv-old-type GE 7 THEN DO:
        FOR EACH bf-eb FIELDS(yld-qty)
            WHERE bf-eb.company EQ est.company
              AND bf-eb.est-no  EQ est.est-no
              AND bf-eb.form-no NE 0
              NO-LOCK:
          IF bf-eb.yld-qty LT lv-yld-qty THEN lv-yld-qty = bf-eb.yld-qty.
        END.
        FOR EACH bf-eb
            WHERE bf-eb.company EQ est.company
              AND bf-eb.est-no  EQ est.est-no
              AND bf-eb.form-no NE 0:
          ld = bf-eb.yld-qty / lv-yld-qty.
          {sys/inc/roundup.i ld}
          bf-eb.quantityPerSet = ld.
        END.
     END.

     IF i = 2 THEN
     DO:
        v-assem-partition = YES.
        FOR EACH bf-eb FIELDS(style) WHERE
            bf-eb.company EQ est.company AND
            bf-eb.est-no  EQ est.est-no AND
            bf-eb.form-no NE 0
            NO-LOCK,
            FIRST style FIELDS(TYPE) WHERE
                  style.company EQ est.company AND
                  style.style   EQ bf-eb.style
                  NO-LOCK:

            IF LOOKUP(style.TYPE,'P,R') EQ 0 THEN
            DO:
               v-assem-partition = NO.
               LEAVE.
            END.
        END.
     END.

     IF v-assem-partition THEN
        FIND FIRST bf-eb-header WHERE
             bf-eb-header.company EQ est.company AND
             bf-eb-header.est-no  EQ est.est-no AND
             bf-eb-header.form-no EQ 0 AND
             bf-eb-header.blank-no EQ 0
             NO-LOCK NO-ERROR.

     IF NOT v-assem-partition OR (NOT AVAIL bf-eb-header) THEN DO:

         FIND xest WHERE RECID(xest) = RECID(est) NO-LOCK.
         FIND xef WHERE RECID(xef) = RECID(ef) NO-LOCK.
         FIND xeb WHERE RECID(xeb) = RECID(eb) NO-LOCK.

        RUN cec/d-updset.w (RECID(eb),6).
     END.
     ELSE
        RUN cec/d-updsetpart.w(INPUT ROWID(est),
                               INPUT ROWID(bf-eb-header)).
    
     IF ERROR-STATUS:ERROR                     AND
        lv-type NE lv-old-type                 AND
        NOT CAN-FIND(FIRST bf-eb
                     WHERE bf-eb.company EQ est.company
                       AND bf-eb.est-no  EQ est.est-no
                       AND bf-eb.form-no EQ 0) THEN DO:
       FIND CURRENT est.
       est.est-type = lv-old-type.
       RUN reset-est-type (OUTPUT lv-old-type).
       FIND CURRENT est NO-LOCK.
     END.
  END.

  IF lv-type NE lv-old-type THEN RUN redisplay-blanks (ROWID(eb)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-64-dec B-table-Win 
PROCEDURE valid-64-dec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-dec AS DEC DECIMALS 6 NO-UNDO.
   DEFINE OUTPUT PARAMETER op-error AS LOG NO-UNDO.
   DEFINE OUTPUT PARAMETER op-dec AS DEC DECIMALS 6 NO-UNDO.
    
    FIND FIRST tt-64-dec WHERE
      SUBSTRING(STRING(tt-64-dec.DEC),1,3) EQ substring(STRING(ip-dec),1,3) NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-64-dec  THEN
      op-error = YES.
    ELSE  op-dec = tt-64-dec.DEC .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust-user B-table-Win 
PROCEDURE valid-cust-user :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 custcount = "".
DEF VAR lActive AS LOG NO-UNDO.
RUN sys/ref/CustList.p (INPUT cocode,
                            INPUT 'EC',
                            INPUT YES,
                            OUTPUT lActive).
 {sys/inc/chblankcust.i ""EC""}
  
  IF ou-log THEN
    DO WITH FRAME {&FRAME-NAME}:
      IF LOOKUP(eb.cust-no:SCREEN-VALUE IN BROWSE {&browse-name},custcount) = 0 THEN DO:
          MESSAGE "Customer is not on Users Customer List.  "  SKIP
              "Please add customer to Network Admin - Users Customer List."  VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO eb.cust-no .
          RETURN ERROR.
      END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-eb-reckey B-table-Win 
PROCEDURE valid-eb-reckey :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF BUFFER bf-eb FOR eb.
   DEF VAR ls-key AS cha NO-UNDO.
   FIND FIRST bf-eb WHERE bf-eb.rec_key = eb.rec_key AND 
                          RECID(bf-eb) <> RECID(eb) NO-LOCK NO-ERROR.
   IF AVAIL bf-eb OR eb.rec_key = "" THEN DO:
      ls-key = STRING(TODAY,"99999999") +
               string(NEXT-VALUE(rec_key_seq,asi),"99999999").
      FIND CURRENT eb.
      eb.rec_key = ls-key.
      FIND CURRENT eb NO-LOCK.               
      CREATE rec_key.
      ASSIGN rec_key.rec_key = eb.rec_key
             rec_key.table_name = "eb".

   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-est-no B-table-Win 
PROCEDURE valid-est-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-est FOR est.

  DEF VAR lv-est-no LIKE est.est-no NO-UNDO.

                  
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     lv-est-no = TRIM(est.est-no:SCREEN-VALUE IN BROWSE {&browse-name})
     lv-est-no = FILL(" ", 8 - LENGTH(TRIM(lv-est-no))) + TRIM(lv-est-no)
     est.est-no:SCREEN-VALUE IN BROWSE {&browse-name} = lv-est-no.

    IF CAN-FIND(FIRST b-est
                WHERE b-est.company EQ gcompany
                  AND b-est.est-no  EQ lv-est-no
                  AND ROWID(b-est)  NE ROWID(est)) OR
       lv-est-no EQ ""                             THEN DO:
      MESSAGE TRIM(est.est-no:LABEL IN BROWSE {&browse-name}) +
              " is spaces or already exists, please re-enter..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO est.est-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-flute B-table-Win 
PROCEDURE valid-flute :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF lv-foam THEN DO WITH FRAME {&FRAME-NAME}:
    eb.flute:SCREEN-VALUE IN BROWSE {&browse-name} = "".
  END.
  ELSE DO:
    {est/valflute.i "eb.flute" ":SCREEN-VALUE" " IN BROWSE {&browse-name}"}
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-part-no B-table-Win 
PROCEDURE valid-part-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-eb FOR eb.

  DEF VAR lv-part-no LIKE eb.part-no NO-UNDO.
  DEF VAR lv-msg AS CHAR NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     lv-part-no = eb.part-no:SCREEN-VALUE IN BROWSE {&browse-name}
     lv-msg     = "".
   
        IF lv-part-no EQ ""                                                     OR
           (CAN-FIND(FIRST b-eb OF ef
                     WHERE b-eb.part-no EQ lv-part-no
                       AND (ROWID(b-eb) NE ROWID(eb) OR ll-is-copy-record)) AND
            (lv-copy-what NE "form" OR NOT ll-is-copy-record))                  THEN
          lv-msg = IF lv-part-no EQ "" THEN "may not be blank"
                                       ELSE "already exists on Form #" +
                                            TRIM(STRING(ef.form-no,">>>")).

    IF lv-msg NE "" THEN DO:
      MESSAGE TRIM(eb.part-no:LABEL IN BROWSE {&browse-name}) + " " +
              TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO eb.part-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-procat B-table-Win 
PROCEDURE valid-procat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    eb.procat:SCREEN-VALUE IN BROWSE {&browse-name} =
        CAPS(eb.procat:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF NOT CAN-FIND(FIRST fgcat
                    WHERE fgcat.company EQ cocode
                      AND fgcat.procat  EQ eb.procat:SCREEN-VALUE IN BROWSE {&browse-name}) OR
       eb.procat:SCREEN-VALUE IN BROWSE {&browse-name} EQ ""                                THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO eb.procat IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-ship-id B-table-Win 
PROCEDURE valid-ship-id :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST shipto
                    WHERE shipto.company EQ gcompany
                      AND shipto.cust-no EQ eb.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND shipto.ship-id EQ eb.ship-id:SCREEN-VALUE IN BROWSE {&browse-name}) AND
       NOT ll-new-shipto                                             THEN DO:
      MESSAGE "            Invalid entry, try help...             " SKIP(1)
              "                        OR                         " SKIP(1)
              "Do you wish to add this Shipto ID to this Customer?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE ll-new-shipto.
      IF NOT ll-new-shipto THEN DO:
        APPLY "entry" TO eb.ship-id IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-stock-no B-table-Win 
PROCEDURE valid-stock-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&frame-name}:
    IF eb.stock-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" AND
       NOT ll-add-set-part                                     AND
       NOT ll-crt-itemfg                                       AND
       NOT CAN-FIND(FIRST itemfg
                    WHERE itemfg.company EQ gcompany
                      AND itemfg.i-no    EQ eb.stock-no:SCREEN-VALUE IN BROWSE {&browse-name})
    THEN DO:
      MESSAGE "This item does not exist, would you like to add it?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE ll-ans AS LOG.  
      IF ll-ans THEN ll-crt-itemfg = YES.
      ELSE DO:
        APPLY "entry" TO eb.stock-no IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.
    END.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-style B-table-Win 
PROCEDURE valid-style :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST style
                    WHERE style.company  EQ gcompany
                      AND style.style    EQ eb.style:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND style.industry EQ "2") OR
       eb.style:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO eb.style IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-test B-table-Win 
PROCEDURE valid-test :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF lv-foam THEN DO WITH FRAME {&FRAME-NAME}:
    eb.test:SCREEN-VALUE IN BROWSE {&browse-name} = "".
  END.
  ELSE DO:
    {est/valtest.i "eb.flute" "eb.test" ":SCREEN-VALUE" " IN BROWSE {&browse-name}"}
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-wid-len B-table-Win 
PROCEDURE valid-wid-len :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    IF ll-warn AND ll-wid-len-warned EQ NO                                                  AND
       CAN-FIND(FIRST style
                WHERE style.company  EQ cocode
                  AND style.style    EQ eb.style:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                  AND style.industry EQ "2"
                  AND INDEX("DF",style.type) LE 0)                                          AND
       (eb.style:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}    NE eb.style OR
        {sys/inc/k16bv.i "DEC(eb.wid:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})"} NE eb.wid OR
        {sys/inc/k16bv.i "DEC(eb.len:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})"} NE eb.len)    AND
        {sys/inc/k16bv.i "DEC(eb.wid:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})"} GT
        {sys/inc/k16bv.i "DEC(eb.len:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})"}               THEN DO:
      MESSAGE "This is an abnormal box, carton width should not be"
              "greater than length." SKIP
              "Would you like to continue with abnormal box?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll-wid-len-warned.
      IF NOT ll-wid-len-warned THEN DO:
        APPLY "entry" TO ip-focus.
        RETURN ERROR.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-combo-qty B-table-Win 
FUNCTION display-combo-qty RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR lv-qty LIKE est-qty.eqty NO-UNDO.

  DEF BUFFER b-eb FOR eb.


  IF AVAIL est-qty AND AVAIL eb THEN DO:
    lv-qty = est-qty.eqty.

    FIND b-eb WHERE ROWID(b-eb) EQ ROWID(eb) NO-LOCK NO-ERROR.
    IF AVAIL b-eb AND b-eb.est-type EQ 8 THEN lv-qty = b-eb.bl-qty.
  END.

  RETURN lv-qty.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-cw-dim B-table-Win 
FUNCTION display-cw-dim RETURNS DECIMAL
  ( INPUT ip-is-corr-style AS LOG, INPUT  ip-dim AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR out-dim AS DEC DECIMALS 6 NO-UNDO.
  
  IF ip-is-corr-style AND ip-dim <> 0 AND v-cecscrn-dec = NO THEN 
     out-dim = ROUND(trunc(ip-dim,0) + ((ip-dim - trunc(ip-dim,0)) / K_FRAC),2).
  ELSE out-dim = ip-dim.

  RETURN out-dim.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-set B-table-Win 
FUNCTION display-set RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  
  RETURN IF eb.spare-char-2 EQ "Y" AND AVAIL eb THEN "Y" ELSE "N". /* Function return value. */ 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-tab B-table-Win 
FUNCTION display-tab RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN IF eb.tab-in EQ ? AND AVAIL eb THEN NO ELSE eb.tab-in. /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

