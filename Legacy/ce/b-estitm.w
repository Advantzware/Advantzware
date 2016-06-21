&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: ce\b-estitm.w

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
def var ls-add-what as cha no-undo.
def var li-new-estnum as int no-undo.
def var ll-new-record as log no-undo.
def var ll-is-copy-record as log no-undo.
def var char-val as cha no-undo.
def new shared buffer xest for est.
def new shared buffer xef for ef.
def new shared buffer xeb for eb.
def new shared buffer xqty for est-qty.

def var lv-part-no-prev like eb.part-no no-undo.
def var lv-eb-recid as recid no-undo.
def var lv-ef-recid as recid no-undo.
def var is-item-copied-from-est as log no-undo.
def var li-form# like ef.form-no no-undo.
def var li-est-form-qty like est.form-qty no-undo.
def var ls-cust-no as cha no-undo.
def var ls-ship-id as cha no-undo.
def var ls-set-part-no as cha no-undo.  /* set part-no from local-create-record*/

def var lv-crt-est-rowid as rowid no-undo.  /* refreshing for new record */
def var ll-add-set as log no-undo init no.
DEF VAR lv-cad-no LIKE eb.cad-no NO-UNDO.
DEF VAR lv-die-no LIKE eb.die-no NO-UNDO.
DEF VAR v-side-count AS INT NO-UNDO.
DEF VAR v-rowid-eb AS ROWID NO-UNDO.

&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

{sys/inc/var.i "new" "shared"}

def new shared temp-table formule field formule as dec extent 12.

def var ls-prev-val as cha no-undo.
def var lv-copy-qty as int extent 20 no-undo.
def var lv-copy-pr as dec extent 20 no-undo.
def var lv-copy-uom as cha extent 20 no-undo.
def var lv-copy-date as date extent 20 no-undo.
def var lv-estqty-recid as recid no-undo.
def var lv-foam as log no-undo.
def var lv-copied as rowid no-undo.
{cec/descalc.i "new"}
def new shared var xcal as dec no-undo.
def new shared var sh-wid as dec no-undo.
def new shared var sh-len as dec no-undo.
def var k_frac as dec init 6.25 no-undo.
def var ll-is-add-from-tool as log no-undo.
def var ll-crt-itemfg as log no-undo.
def var lv-frst-rowid as rowid no-undo.
def var lv-last-rowid as rowid no-undo.
def var lv-frst-rowid2 as rowid no-undo.
def var lv-last-rowid2 as rowid no-undo.
DEF VAR lv-eb-copy-frid AS RECID NO-UNDO.
DEF VAR lv-ef-copy-frid AS RECID NO-UNDO.
DEF VAR lv-copy-what AS cha NO-UNDO.   /* Blank or Form */
DEF VAR ll-part-no AS LOG NO-UNDO.
DEF VAR ll-mass-del AS LOG NO-UNDO.
DEF VAR prev-cust LIKE eb.cust-no NO-UNDO.
DEF VAR ll-new-shipto AS LOG NO-UNDO.
DEF VAR ll-form AS LOG NO-UNDO.
def var ll-warn as log no-undo.
DEF VAR ll-wid-len-warned AS LOG NO-UNDO.
DEF VAR old-bl-qty LIKE eb.bl-qty NO-UNDO.
DEF VAR ll-tandem AS LOG NO-UNDO.
DEF VAR lv-repo AS CHAR INIT "ON" NO-UNDO.
def var uom-list as cha no-undo.

/*DEF BUFFER bf-ef FOR ef.
DEF BUFFER bf-eb FOR eb.
DEF BUFFER bf-est FOR est.*/

DEFINE VARIABLE cadcamValue AS CHARACTER NO-UNDO.

RUN Get-Company  (OUTPUT gcompany).
RUN Get-location (OUTPUT gloc).

assign cocode = gcompany
       locode = gloc.

DEF NEW SHARED TEMP-TABLE tt-eb-set NO-UNDO LIKE eb.

DEF TEMP-TABLE tt-eb LIKE eb FIELD row-id AS ROWID INDEX row-id row-id.
DEF TEMP-TABLE tt-est-op LIKE est-op.

{est/inksvarn.i NEW}
{sys/ref/CustList.i NEW}
DO TRANSACTION:
  {ce/cecopy.i}
  {sys/inc/cegoto.i}
  {sys/inc/cadcam.i}
  {sys/inc/ceroute.i F}
  {sys/inc/cestyle.i F}
  {sys/inc/cefgitem.i}
  {sys/inc/graphic.i}
  {sys/inc/shiptorep.i}
  {sys/inc/custlistform.i ""EF""}
END.
{sys/inc/f16to32.i}

{methods/defines/hndldefs.i}

{est/artiosvar.i "new shared"}
{est/frmotvar.i "new shared"}

{cec/tt-eb-set-part.i "new"}

DEF VAR viEQtyPrev AS INT NO-UNDO.

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
eb.ship-id eb.part-dscr1 eb.stock-no eb.bl-qty eb.style ef.board ef.cal ~
eb.procat eb.len eb.wid eb.dep eb.cust-% eb.i-col eb.i-coat eb.form-no ~
eb.blank-no eb.num-wid eb.num-len eb.num-up eb.die-in ef.f-col ef.f-pass ~
ef.f-coat ef.f-coat-p eb.pur-man est.est-date 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-estitm eb.cust-no eb.part-no ~
eb.ship-id eb.part-dscr1 eb.stock-no eb.bl-qty eb.style ef.board ef.cal ~
eb.procat eb.len eb.wid eb.dep eb.cust-% eb.i-col eb.i-coat eb.die-in ~
ef.f-col ef.f-pass ef.f-coat ef.f-coat-p eb.pur-man est.est-date 
&Scoped-define ENABLED-TABLES-IN-QUERY-br-estitm eb ef est
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-estitm eb
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-br-estitm ef
&Scoped-define THIRD-ENABLED-TABLE-IN-QUERY-br-estitm est
&Scoped-define QUERY-STRING-br-estitm FOR EACH ef WHERE ef.company = est-qty.company ~
  AND ef.est-no = est-qty.est-no ~
  AND ef.eqty = est-qty.eqty NO-LOCK, ~
      EACH eb WHERE eb.company = ef.company ~
  AND eb.est-no = ef.est-no ~
  AND eb.form-no = ef.form-no NO-LOCK ~
    BY eb.form-no ~
       BY eb.blank-no
&Scoped-define OPEN-QUERY-br-estitm OPEN QUERY br-estitm FOR EACH ef WHERE ef.company = est-qty.company ~
  AND ef.est-no = est-qty.est-no ~
  AND ef.eqty = est-qty.eqty NO-LOCK, ~
      EACH eb WHERE eb.company = ef.company ~
  AND eb.est-no = ef.est-no ~
  AND eb.form-no = ef.form-no NO-LOCK ~
    BY eb.form-no ~
       BY eb.blank-no.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-cw-dim B-table-Win 
FUNCTION display-cw-dim RETURNS DECIMAL
  ( input ip-is-corr-style as log, input  ip-dim as decimal )  FORWARD.

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
      est.est-no FORMAT "99999999":U WIDTH 12 COLUMN-FONT 2
      eb.cust-no FORMAT "x(8)":U COLUMN-FONT 2
      eb.part-no FORMAT "x(15)":U COLUMN-FONT 2
      eb.ship-id COLUMN-LABEL "Ship To" FORMAT "x(8)":U WIDTH 12
            COLUMN-FONT 2
      eb.part-dscr1 FORMAT "x(30)":U COLUMN-FONT 2
      eb.stock-no COLUMN-LABEL "FG Item#" FORMAT "x(15)":U COLUMN-FONT 2
      eb.bl-qty COLUMN-LABEL "Qty" FORMAT ">>>,>>>,>>>":U WIDTH 15
      eb.style COLUMN-LABEL "Style" FORMAT "x(6)":U WIDTH 9 COLUMN-FONT 2
      ef.board FORMAT "x(12)":U COLUMN-FONT 2
      ef.cal FORMAT ">9.99999<":U COLUMN-FONT 2
      eb.procat FORMAT "x(5)":U COLUMN-FONT 2
      eb.len FORMAT ">9.99999":U COLUMN-FONT 2
      eb.wid FORMAT ">9.99999":U COLUMN-FONT 2
      eb.dep FORMAT ">9.99999":U COLUMN-FONT 2
      eb.cust-% COLUMN-LABEL "Qty/Set" FORMAT "->>,>>>":U WIDTH 10
      eb.i-col FORMAT ">9":U
      eb.i-coat FORMAT ">9":U
      eb.form-no COLUMN-LABEL "S" FORMAT ">>>":U
      eb.blank-no COLUMN-LABEL "B" FORMAT ">>>":U
      eb.num-wid FORMAT ">9":U
      eb.num-len FORMAT ">9":U
      eb.num-up COLUMN-LABEL "# Up" FORMAT ">>>,>>9":U
      eb.die-in FORMAT ">>>>9":U
      ef.f-col COLUMN-LABEL "Inks/Form" FORMAT ">>":U
      ef.f-pass COLUMN-LABEL "Passes/Form" FORMAT ">>":U
      ef.f-coat COLUMN-LABEL "Coatings/Form" FORMAT ">>":U
      ef.f-coat-p COLUMN-LABEL "Coat Passes/Form" FORMAT ">>":U
      eb.pur-man COLUMN-LABEL "Purch/Manuf" FORMAT "P/M":U
      est.est-date FORMAT "99/99/9999":U COLUMN-FONT 2
  ENABLE
      eb.cust-no
      eb.part-no
      eb.ship-id
      eb.part-dscr1
      eb.stock-no
      eb.bl-qty
      eb.style
      ef.board
      ef.cal
      eb.procat
      eb.len
      eb.wid
      eb.dep
      eb.cust-% HELP "Quantity Per Set"
      eb.i-col
      eb.i-coat
      eb.die-in
      ef.f-col
      ef.f-pass
      ef.f-coat
      ef.f-coat-p
      eb.pur-man
      est.est-date
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 148 BY 17.62
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
         HEIGHT             = 17.62
         WIDTH              = 148.
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
     _Options          = "NO-LOCK KEY-PHRASE"
     _TblOptList       = ","
     _OrdList          = "ASI.eb.form-no|yes,ASI.eb.blank-no|yes"
     _JoinCode[1]      = "ASI.ef.company = ASI.est-qty.company
  AND ASI.ef.est-no = ASI.est-qty.est-no
  AND ASI.ef.eqty = ASI.est-qty.eqty"
     _JoinCode[2]      = "ASI.eb.company = ASI.ef.company
  AND ASI.eb.est-no = ASI.ef.est-no
  AND ASI.eb.form-no = ASI.ef.form-no"
     _FldNameList[1]   > ASI.est.est-no
"est.est-no" ? "99999999" "character" ? ? 2 ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.eb.cust-no
"eb.cust-no" ? ? "character" ? ? 2 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.eb.part-no
"eb.part-no" ? ? "character" ? ? 2 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.eb.ship-id
"eb.ship-id" "Ship To" ? "character" ? ? 2 ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.eb.part-dscr1
"eb.part-dscr1" ? ? "character" ? ? 2 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.eb.stock-no
"eb.stock-no" "FG Item#" ? "character" ? ? 2 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.eb.bl-qty
"eb.bl-qty" "Qty" ">>>,>>>,>>>" "integer" ? ? ? ? ? ? yes ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.eb.style
"eb.style" "Style" ? "character" ? ? 2 ? ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.ef.board
"ef.board" ? "x(12)" "character" ? ? 2 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.ef.cal
"ef.cal" ? ">9.99999<" "decimal" ? ? 2 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.eb.procat
"eb.procat" ? ? "character" ? ? 2 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.eb.len
"eb.len" ? ? "decimal" ? ? 2 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.eb.wid
"eb.wid" ? ? "decimal" ? ? 2 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.eb.dep
"eb.dep" ? ? "decimal" ? ? 2 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.eb.cust-%
"eb.cust-%" "Qty/Set" "->>,>>>" "decimal" ? ? ? ? ? ? yes "Quantity Per Set" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.eb.i-col
"eb.i-col" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > ASI.eb.i-coat
"eb.i-coat" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ASI.eb.form-no
"eb.form-no" "S" ">>>" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > ASI.eb.blank-no
"eb.blank-no" "B" ">>>" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   = ASI.eb.num-wid
     _FldNameList[21]   = ASI.eb.num-len
     _FldNameList[22]   > ASI.eb.num-up
"eb.num-up" "# Up" ">>>,>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > ASI.eb.die-in
"eb.die-in" ? ">>>>9" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > ASI.ef.f-col
"ef.f-col" "Inks/Form" ">>" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > ASI.ef.f-pass
"ef.f-pass" "Passes/Form" ">>" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > ASI.ef.f-coat
"ef.f-coat" "Coatings/Form" ">>" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   > ASI.ef.f-coat-p
"ef.f-coat-p" "Coat Passes/Form" ">>" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   > ASI.eb.pur-man
"eb.pur-man" "Purch/Manuf" ? "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[29]   > ASI.est.est-date
"est.est-date" ? ? "date" ? ? 2 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
   def var phandle as widget-handle no-undo.
   def var char-hdl as cha no-undo.   
   RUN get-link-handle IN adm-broker-hdl
      (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
   phandle = WIDGET-HANDLE(char-hdl).
   
   RUN new-state in phandle ('update-begin':U).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-estitm B-table-Win
ON HELP OF br-estitm IN FRAME Corr
DO:
     def var ls-cur-val as cha no-undo.
     def var lv-eb-tmpid as recid no-undo.
     def var lv-handle as handle no-undo.          
     def var char-val2 as cha no-undo.        
     def var date-val as cha no-undo.
     def var date-val2 as cha no-undo.
     def var lv-rowid as rowid no-undo.

     DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.


     lw-focus = FOCUS.

     case lw-focus:name :
     when "part-no" then do:
         RUN blank-cp (YES).
         return no-apply.
      end.
      when "part-dscr1" then do:
         RUN blank-cp (YES).
         return no-apply.
      end.
      when "stock-no" then do:
         RUN blank-cp (YES).
         return no-apply.
      end.
      when "style" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-stylef.w (gcompany,ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value = entry(1,char-val).
              find style where style.company = gcompany and
                               style.style = eb.style:screen-value in browse {&browse-name}
                         no-lock no-error.            
              if avail style then do:
                ef.board:SCREEN-VALUE IN BROWSE {&browse-name} = style.material[1].
                RUN new-board.
              end.          
           end.  
           return no-apply.
      end.
      when "procat" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-fgcat.w (gcompany,ls-cur-val,output char-val).
           if char-val <> "" then
              lw-focus:screen-value = entry(1,char-val).
           return no-apply.

       end.
       when "Board" then do:
           def var lv-ind like style.industry no-undo.
           ls-cur-val = lw-focus:screen-value.
           find style where style.company = gcompany and
                            style.style = eb.style:screen-value in browse {&browse-name}
                            no-lock no-error.   
           if avail style then lv-ind = style.industry.
           else lv-ind = "".  
           if avail style and style.type = "f" then  /* foam */
                 run windows/l-boardf.w (gcompany,lv-ind,ls-cur-val,output char-val).
           else run windows/l-board1.w (eb.company,lv-ind,lw-focus:screen-value, output lv-rowid).
           FIND FIRST ITEM WHERE ROWID(item) EQ lv-rowid NO-LOCK NO-ERROR.
           IF AVAIL ITEM AND ITEM.i-no NE lw-focus:SCREEN-VALUE THEN DO:
             lw-focus:SCREEN-VALUE = item.i-no.
             RUN new-board.
           END.

           return no-apply.   
       end.
       WHEN "cust-no" THEN DO:
           ls-cur-val = lw-focus:SCREEN-VALUE.
           RUN windows/l-cust.w (gcompany,ls-cur-val, OUTPUT char-val).
           IF char-val NE "" AND ls-cur-val NE ENTRY(1,char-val) THEN DO:
              lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
              APPLY "value-changed" TO eb.cust-no.
           END.
           RETURN NO-APPLY.
       END.  /* cust-no */
       WHEN "ship-id" THEN DO:
           ls-cur-val = lw-focus:SCREEN-VALUE.
           RUN windows/l-shipto.w (gcompany,gloc,eb.cust-no:SCREEN-VALUE,ls-cur-val, OUTPUT char-val).
           IF char-val NE "" AND ls-cur-val NE ENTRY(1,char-val) THEN DO:
              lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
              APPLY "value-changed" TO lw-focus.
           END.
           RETURN NO-APPLY.
       END.  /* ship-id */
       when "bl-qty" then do:
             FIND FIRST est-qty WHERE est-qty.company = gcompany
                                  AND est-qty.est-no = est.est-no NO-LOCK NO-ERROR.
             lv-estqty-recid = if avail est-qty then recid(est-qty) else ?.
             run est/estqtyd.w (lv-estqty-recid, recid(eb),eb.bl-qty:screen-value in browse {&browse-name}, output char-val, output char-val2, output date-val, output date-val2) .
             if char-val <> "?" 
                then assign lw-focus:screen-value = entry(1,char-val)
                            lv-copy-qty[2] = integer(entry(2,char-val))
                            lv-copy-qty[3] = integer(entry(3,char-val))
                            lv-copy-qty[4] = integer(entry(4,char-val))
                            lv-copy-qty[5] = integer(entry(5,char-val))
                            lv-copy-qty[6] = integer(entry(6,char-val))
                            lv-copy-qty[7] = integer(entry(7,char-val))
                            lv-copy-qty[8] = integer(entry(8,char-val))
                            lv-copy-qty[9] = integer(entry(9,char-val))
                            lv-copy-qty[10] = integer(entry(10,char-val))
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
             if char-val2 <> "?" 
                then assign lv-copy-qty[11] = integer(entry(1,char-val2))
                            lv-copy-qty[12] = integer(entry(2,char-val2))
                            lv-copy-qty[13] = integer(entry(3,char-val2))
                            lv-copy-qty[14] = integer(entry(4,char-val2))
                            lv-copy-qty[15] = integer(entry(5,char-val2))
                            lv-copy-qty[16] = integer(entry(6,char-val2))
                            lv-copy-qty[17] = integer(entry(7,char-val2))
                            lv-copy-qty[18] = integer(entry(8,char-val2))
                            lv-copy-qty[19] = integer(entry(9,char-val2))
                            lv-copy-qty[20] = integer(entry(10,char-val2))
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
             return no-apply.
       end.
       otherwise do:
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
           return no-apply.
        end.  /* otherwise */
  end case.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-estitm B-table-Win
ON return OF br-estitm IN FRAME Corr
anywhere
DO:
   apply "tab" to self.
   return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-estitm B-table-Win
ON ROW-DISPLAY OF br-estitm IN FRAME Corr
DO:
   DEF VAR lActive AS LOG NO-UNDO.
   IF v-cefgitem-log THEN
   DO:
       RUN fg/GetItemfgActInact.p (INPUT cocode,
                                   INPUT eb.stock-no,
                                   OUTPUT lActive).
       IF NOT lActive THEN
/*       FIND FIRST reftable WHERE                        */
/*            reftable.reftable EQ "FGSTATUS" AND         */
/*            reftable.company  EQ cocode AND             */
/*            reftable.loc      EQ "" AND                 */
/*            reftable.code     EQ eb.stock-no            */
/*            NO-LOCK NO-ERROR.                           */
/*                                                        */
/*       IF AVAIL reftable AND reftable.code2 EQ "I" THEN */
         eb.stock-no:BGCOLOR IN BROWSE {&browse-name} = 11.
      ELSE
         eb.stock-no:BGCOLOR IN BROWSE {&browse-name} = ?.

      RELEASE reftable.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-estitm B-table-Win
ON ROW-ENTRY OF br-estitm IN FRAME Corr
DO:
  /* This code displays initial values for newly added or copied rows. */

  {src/adm/template/brsentry.i}  
  /* not to have error no eb record avail when add new set item */
  if not avail eb AND lv-eb-recid <> ? then find eb where recid(eb)  = lv-eb-recid no-lock.
  
  ASSIGN
   ll-part-no        = NO
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
  
   if keyfunction(lastkey) = "page-up" or 
      keyfunction(lastkey) = "page-down" or
      keyfunction(lastkey) = "cursor-up" or
      keyfunction(lastkey) = "cursor-down" 
   then return no-apply.
 
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
     
  if not adm-new-record /*and not adm-adding-record */ AND AVAIL eb then   
     assign lv-eb-recid = recid(eb)
            lv-ef-recid = recid(ef).      

  {methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header"
     "(est.rec_key,'ESTIMATE:' + eb.rec_key + ' ' + {methods/headers/est.i})"}

  RUN setFarmTab.
      
  RUN custom-row-changed.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.cust-no
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
  IF LASTKEY NE -1 THEN DO: /*eb.cust-no:screen-value in browse {&browse-name} <> "" and */

    IF self:MODIFIED AND eb.ord-no NE 0 AND
       eb.cust-no:SCREEN-VALUE in browse {&browse-name} NE eb.cust-no AND
       eb.cust-no NE "" THEN
    DO:
      MESSAGE "Cannot Change Customer."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN NO-APPLY.
    END.
      
    IF SELF:MODIFIED THEN RUN new-cust-no.

    IF not can-find(cust where cust.company = gcompany and cust.cust-no = eb.cust-no:screen-value in browse {&browse-name} )
    then do:
       if eb.cust-no:screen-value = "" then do:
           message "Invalid Customer Number. Try Help." view-as alert-box error. 
           return no-apply.
       end.
       message "Customer " eb.cust-no:screen-value "does not exist. Do you want to add it?"
               view-as alert-box question button yes-no update ll-ans as log.
       if not ll-ans then  return no-apply.
       
       run est/custfly.w (eb.cust-no:screen-value).    
       IF not can-find(cust where cust.company = gcompany and cust.cust-no = eb.cust-no:screen-value in browse {&browse-name} )
       then RETURN no-apply.

    end.

    RUN valid-cust-user NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.part-no br-estitm _BROWSE-COLUMN B-table-Win
ON ENTRY OF eb.part-no IN BROWSE br-estitm /* Cust Part # */
DO:
  DEF VAR li-flen AS INT EXTENT 2 NO-UNDO.


  IF ll-add-set THEN DO:
    ll-add-set = NO.
    RUN est/crt-set.w (ROWID(est)) NO-ERROR.
    FIND FIRST tt-eb-set NO-LOCK NO-ERROR.
    IF AVAIL tt-eb-set THEN
      ASSIGN
       li-flen[1] = LENGTH(STRING(FILL("X",100),eb.part-no:FORMAT IN BROWSE {&browse-name}))  - 4
       li-flen[2] = LENGTH(STRING(FILL("X",100),eb.stock-no:FORMAT IN BROWSE {&browse-name})) - 4
       eb.part-no:SCREEN-VALUE IN BROWSE {&browse-name}    =
            TRIM(SUBSTR(tt-eb-set.part-no,1,li-flen[1])) + "-1-1"
       eb.part-dscr1:SCREEN-VALUE IN BROWSE {&browse-name} = tt-eb-set.part-dscr1
       eb.stock-no:SCREEN-VALUE IN BROWSE {&browse-name}   =
            IF tt-eb-set.stock-no NE "" THEN (TRIM(SUBSTR(tt-eb-set.stock-no,1,li-flen[2])) + "-1-1") ELSE ""
       eb.procat:SCREEN-VALUE = tt-eb-set.procat.
    ELSE RUN dispatch ("cancel-record").
  END.

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
ON LEAVE OF eb.part-dscr1 IN BROWSE br-estitm /* Item Description */
DO:
  IF LASTKEY NE -1                                           AND
     eb.part-no:SCREEN-VALUE IN BROWSE {&browse-name}  EQ "" AND
     eb.stock-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN
    RUN blank-cp (NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.part-dscr1 br-estitm _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF eb.part-dscr1 IN BROWSE br-estitm /* Item Description */
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
    IF eb.stock-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
        RUN fg/GetItemfgActInact.p (INPUT g_company,
                                    INPUT SELF:SCREEN-VALUE,
                                    OUTPUT lActive).
        IF NOT lActive THEN DO:
/*                                                                   */
/*         FIND FIRST reftable WHERE reftable.reftable EQ "FGSTATUS" */
/*                        AND reftable.company  EQ g_company         */
/*                        AND reftable.loc      EQ ""                */
/*                        AND reftable.code     EQ SELF:SCREEN-VALUE */
/*                        NO-LOCK NO-ERROR.                          */
/*         IF AVAIL reftable AND reftable.code2 = "I" THEN DO:       */
           MESSAGE eb.stock-no:SCREEN-VALUE + " has InActive Status. Order cannot be placed for the Inactive Item."
                   VIEW-AS ALERT-BOX ERROR.
           RETURN NO-APPLY.
        END.        
    END.
   
    ll-copy-fg = CAN-FIND(FIRST itemfg
                          WHERE itemfg.company EQ g_company
                            AND itemfg.i-no    EQ SELF:SCREEN-VALUE) AND
                 SELF:SCREEN-VALUE NE ls-prev-val.

    IF (eb.part-no:SCREEN-VALUE IN BROWSE {&browse-name}    EQ "" AND
        eb.part-dscr1:SCREEN-VALUE IN BROWSE {&browse-name} EQ "") OR
       ll-copy-fg                                                  THEN
      RUN blank-cp (NO).
   
    IF v-fg-copy OR ll-copy-fg THEN DO:
      RUN valid-part-no NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.

    RUN valid-stock-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
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


&Scoped-define SELF-NAME eb.bl-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.bl-qty br-estitm _BROWSE-COLUMN B-table-Win
ON ENTRY OF eb.bl-qty IN BROWSE br-estitm /* Qty */
DO:
  DEF BUFFER b-eb FOR eb.

  IF eb.est-type EQ 2 AND (eb.blank-no GT 1 OR eb.form-no GT 1) then do:
    IF INT({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN DO:
      FIND FIRST b-eb OF est
          WHERE b-eb.form-no NE 0
            AND b-eb.bl-qty  NE 0
          NO-LOCK NO-ERROR.
      IF AVAIL b-eb THEN
        {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(b-eb.bl-qty).
    END.
    apply "tab" to self.
    return no-apply.    
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.bl-qty br-estitm _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.bl-qty IN BROWSE br-estitm /* Qty */
DO:
  if lastkey = -1 then return.

  if int(eb.bl-qty:screen-value in browse {&browse-name} ) <= 0 then do:
     message "Quantity must be entered. " view-as alert-box error.
     return no-apply.
  end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.style
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.style br-estitm _BROWSE-COLUMN B-table-Win
ON ENTRY OF eb.style IN BROWSE br-estitm /* Style */
DO:
    ls-prev-val = self:screen-value.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.style br-estitm _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.style IN BROWSE br-estitm /* Style */
DO: 
  IF LASTKEY NE -1 THEN DO:
    RUN valid-style NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    if is-item-copied-from-est then return.

    if /*eb.style:modified in browse {&browse-name}*/ 
       self:screen-value <> ls-prev-val then do:
      find style where style.company = gcompany and
                       style.style = eb.style:screen-value in browse {&browse-name}
                       no-lock no-error.   
      if avail style then do:
         IF adm-adding-record AND ef.board:SCREEN-VALUE = "" THEN DO:
           ef.board:screen-value in browse {&browse-name} = style.material[1].
           RUN new-board.
         END.
         lv-foam = if style.type = "F" then  yes else no.
      end.   
    end.
    self:screen-value = caps(self:screen-value).

    RUN valid-wid-len NO-ERROR.
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


&Scoped-define SELF-NAME ef.board
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.board br-estitm _BROWSE-COLUMN B-table-Win
ON LEAVE OF ef.board IN BROWSE br-estitm /* Board */
DO:
   if lastkey = -1 then return.
   
   find item where item.company = gcompany
                and item.i-no = self:screen-value in browse {&browse-name}
                    no-lock no-error.
   if not avail item then do:
      message "Invalid Board. Try Help. " view-as alert-box error.
      return no-apply.
   end.              

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
   find first fgcat where fgcat.company = gcompany and
                          fgcat.procat > eb.procat:screen-value in browse {&browse-name}
                          no-lock no-error.
   if avail fgcat then eb.procat:screen-value in browse {&browse-name} = fgcat.procat.
   
                          
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.procat br-estitm _BROWSE-COLUMN B-table-Win
ON PAGE-UP OF eb.procat IN BROWSE br-estitm /* Category */
DO:
   find last fgcat where fgcat.company = gcompany and
                          fgcat.procat < eb.procat:screen-value in browse {&browse-name}
                          no-lock no-error.
   if avail fgcat then eb.procat:screen-value in browse {&browse-name} = fgcat.procat.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.len br-estitm _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.len IN BROWSE br-estitm /* Length */
DO:
   if lastkey = -1 then return.
   if decimal(eb.len:screen-value in browse {&browse-name}) = 0 then do:
        message "Length can not be 0. " view-as alert-box error.
        return no-apply.
   end.

  IF LASTKEY NE -1 THEN DO:
    RUN valid-wid-len NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
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
   if lastkey = -1 then return.
   if decimal(eb.wid:screen-value in browse {&browse-name}) = 0 then do:
        message "Width can not be 0. " view-as alert-box error.
        return no-apply.
   end.

  IF LASTKEY NE -1 THEN DO:
    RUN valid-wid-len NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
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


&Scoped-define SELF-NAME eb.cust-%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.cust-% br-estitm _BROWSE-COLUMN B-table-Win
ON ENTRY OF eb.cust-% IN BROWSE br-estitm /* Qty/Set */
DO:
  IF est.est-type NE 2 AND NOT CAN-FIND(FIRST tt-eb-set) THEN DO:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-col
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-col br-estitm _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF eb.i-col IN BROWSE br-estitm /* Colors */
DO:
  IF eb.blank-no EQ 1                                         AND
     DEC(ef.f-col:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
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
  IF eb.blank-no EQ 1                                          AND
     DEC(ef.f-coat:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
    ASSIGN
     ef.f-coat:SCREEN-VALUE IN BROWSE {&browse-name}   =
                              eb.i-coat:SCREEN-VALUE IN BROWSE {&browse-name}
     ef.f-coat-p:SCREEN-VALUE IN BROWSE {&browse-name} = "1".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.f-col
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.f-col br-estitm _BROWSE-COLUMN B-table-Win
ON ENTRY OF ef.f-col IN BROWSE br-estitm /* Inks/Form */
DO:
  IF est.est-type EQ 1 OR eb.blank-no NE 1 THEN DO:
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
  IF est.est-type EQ 1 OR eb.blank-no NE 1 THEN DO:
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
  IF est.est-type EQ 1 OR eb.blank-no NE 1 THEN DO:
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
  IF est.est-type EQ 1 OR eb.blank-no NE 1 THEN DO:
    /*APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}. geting error 4527 */
    APPLY "leave" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}

do with frame {&frame-name}:
  {&browse-name}:set-repositioned-row(1, "conditional").
end.

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

  ASSIGN
  ll-is-add-from-tool = yes  /* add from option button not from add button */
  ls-add-what = "est" .   /* new estimate */
  run est/d-addfol.w (INPUT no, output ls-add-what). /* one item or set cec/est-add.p */
  if ls-add-what = "" then return no-apply.  /* cancel */

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
  def buffer bf-est for est.
  def buffer bf-eb for eb.
  def buffer bf-est-qty for est-qty.


  IF est.est-type EQ 2 THEN
  FOR EACH bf-eb OF est WHERE ROWID(bf-eb) NE ROWID(eb):
    bf-eb.bl-qty = eb.bl-qty.
  END.

  find bf-est-qty where /*recid(bf-est-qty) = recid(est-qty)*/
                        bf-est-qty.company = gcompany
                    AND bf-est-qty.est-no = est.est-no 
                    AND bf-est-qty.eqty = eb.bl-qty 
      NO-ERROR.

  IF AVAIL bf-est-qty THEN DO:
    assign bf-est-qty.qty[1] = est-qty.eqty
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
           bf-est-qty.qty[11] = lv-copy-qty[11]
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
           bf-est-qty.qty-date[20] = lv-copy-date[20].

      find bf-est where bf-est.company = bf-est-qty.company and
                        bf-est.est-no = bf-est-qty.est-no.
      assign bf-est.est-qty[1] = est-qty.eqty
             bf-est.est-qty[2] = bf-est-qty.qty[2]
             bf-est.est-qty[3] = bf-est-qty.qty[3]
             bf-est.est-qty[4] = bf-est-qty.qty[4].

      FIND CURRENT bf-est NO-LOCK.
  END.

  FIND CURRENT bf-est-qty NO-LOCK.

  ASSIGN lv-copy-qty = 0
         lv-copy-date = ?
         lv-copy-pr = 0
         lv-copy-uom = "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE blank-add B-table-Win 
PROCEDURE blank-add :
/*------------------------------------------------------------------------------
  Purpose:     from ce/com/blk-add.p   addign blank for combo
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ip-recid AS RECID NO-UNDO.

DEF VAR lv-rowid AS ROWID NO-UNDO.

DEF BUFFER bf FOR ef.


FIND bf WHERE RECID(bf) EQ ip-recid.

RUN ce/newblank.p (ROWID(bf), OUTPUT lv-rowid).

FIND eb WHERE ROWID(eb) EQ lv-rowid NO-LOCK NO-ERROR.
lv-eb-recid = RECID(eb).

tmpstore = "".

RUN est/blks-frm.p (ROWID(ef)).
FIND CURRENT ef NO-ERROR.  

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

  {est/blankcp1.i}

  DO WITH FRAME {&FRAME-NAME}:
    IF AVAIL b-eb THEN
      ASSIGN
       eb.num-wid:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(b-eb.num-wid)
       eb.num-len:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(b-eb.num-len)
       eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(b-eb.num-up)
       eb.die-in:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(b-eb.die-in).

    IF AVAIL b-ef THEN
      ASSIGN
       ef.f-col:SCREEN-VALUE IN BROWSE {&browse-name}    = STRING(b-ef.f-col)
       ef.f-pass:SCREEN-VALUE IN BROWSE {&browse-name}   = STRING(b-ef.f-pass)
       ef.f-coat:SCREEN-VALUE IN BROWSE {&browse-name}   = STRING(b-ef.f-coat)
       ef.f-coat-p:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(b-ef.f-coat-p).
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

   def buffer bf-eb for eb .
   def var lv-panels as log no-undo.
   def var i as int no-undo.
   def var j as int no-undo.
   def var K_FRAC as dec init 6.25 no-undo.
   def var v-score-char like v-lscore-c extent 12.

   find first sys-ctrl  where sys-ctrl.company eq cocode
                           and sys-ctrl.name    eq "PANELS"
        no-lock no-error.
   if not avail sys-ctrl then do:
      create sys-ctrl.
      assign  sys-ctrl.company = cocode
              sys-ctrl.name    = "PANELS"
              sys-ctrl.descrip = "CE Lock=Yes Panel Size Popup when Overriding W&L?"
              sys-ctrl.log-fld = yes.
      MESSAGE sys-ctrl.descrip
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE sys-ctrl.log-fld.
   end.
   lv-panels = sys-ctrl.log-fld.

   find xest where recid(xest) = recid(est) no-lock.
   find xef where recid(xef) = recid(ef) no-lock.
   find xeb where recid(xeb) = recid(eb) no-lock.
   
   find style where style.company = eb.company and
                    style.style = eb.style
                    no-lock no-error.
   if avail style then do:
      run est/u2kinc1.p (RECID(xeb)).
      run est/u2kinc2.p (RECID(xeb)).
      find bf-eb WHERE ROWID(bf-eb) EQ ROWID(eb) exclusive-lock.    
      FIND FIRST formule NO-ERROR.
      assign bf-eb.t-wid = (formule[1])
          bf-eb.t-len = (formule[2])
          bf-eb.t-sqin = (formule[7] * formule[8])
          .
      /*bf-eb.t-sqin = if v-corr then bf-eb.t-sqin * .007 else bf-eb.t-sqin / 144.
      */
   end.

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
    MESSAGE "Do you wish to reset layout screen?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        UPDATE ll.

  IF ll THEN DO:
    IF NOT lv-foam THEN DO:
      /*{sys/inc/ceroute1.i w id l en}  */
      {ce/ceroute1.i w id l en} 
    END.
    RUN ce/calc-dim.p.
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

      def var k as int no-undo.
      def var counter as int no-undo.
      def var i as int no-undo.
      def var j as int no-undo.
      def var save_id as recid no-undo.
      def var save_id2 as recid no-undo.
      def buffer alt-item for item .
      def var choice as log no-undo.
      def buffer bf-eb for eb.
      
      find first style where style.company = eb.company and
                 style.style = eb.style no-lock no-error.
      if avail style then do:
         if k = 0 then k = integer(style.material[3]).

         RELEASE ITEM.

         IF style.material[2] NE "" THEN
            find first item where
                 item.company = eb.company and
                 item.i-no = style.material[2]
                 no-lock no-error.

         if avail item then k = integer(style.material[3]).

         RELEASE alt-item.

         IF style.material[6] NE "" THEN
            find first alt-item where
                 alt-item.company  = eb.company  and
                 alt-item.mat-type = "V"     and
                 alt-item.i-no     = style.material[6]
                 no-lock no-error.
      end.
      if not avail item or not avail alt-item or (k = 0) then do:
         find first ce-ctrl where ce-ctrl.company = eb.company and
                                  ce-ctrl.loc = eb.loc
                                   no-lock no-error.
         if k = 0 then k = ce-ctrl.def-inkcov.
         if not avail item then do:
            find first item where item.company = eb.company and
                       item.i-no = ce-ctrl.def-ink no-lock no-error.
         end.
         if not avail alt-item then
            find first alt-item where alt-item.company  = eb.company  and
                                      alt-item.mat-type = "V"     and
                                      alt-item.i-no     = ce-ctrl.def-coat
                                      no-lock no-error.
      end.
 
      ASSIGN
      save_id = recid(item)
      save_id2 = recid(alt-item)
      j = (integer(eb.i-col:screen-value in browse {&browse-name})
          + integer(eb.i-coat:screen-value in browse {&browse-name}))
      counter = 1
      choice = true.

      {sys/inc/roundup.i j}
      

/*    do i = 1 to 12:
       if eb.i-code2[i] ne "" then do:
          choice = no.
          leave.
       end.
      end.     
 commented to recalc every time */
  
      find bf-eb WHERE recid(bf-eb) = recid(eb) exclusive-lock.    
      if eb.i-col > 0 then assign bf-eb.i-pass = 1.
      if eb.i-coat > 0 then assign bf-eb.i-coat-p = 1.
      if choice then do i = 1 to 12:
         if i le integer(eb.i-col) then do with frame {&frame-name}:
              find item where recid(item) = save_id no-lock no-error.
              assign bf-eb.i-ps2[i]   = counter
                     bf-eb.i-code2[i] = item.i-no
                     bf-eb.i-dscr2[i] = item.i-name
                     bf-eb.i-%2[i]    = k.
         end.
         else if (i > integer(eb.i-col)) and
                 (i <= (integer(eb.i-col) + 
                       integer(eb.i-coat)) )
         then do:
              find alt-item where recid(alt-item) = save_id2 no-lock no-error.
              assign bf-eb.i-ps2[i]   = counter
                     bf-eb.i-code2[i] = if avail alt-item then alt-item.i-no else ""
                     bf-eb.i-dscr2[i] = if avail alt-item then alt-item.i-name else ""
                     bf-eb.i-%2[i]    = 100.
         end.
         else if (i >  (eb.i-col + eb.i-coat) )
         then do:
            assign bf-eb.i-ps2[i]   = 0  
                     bf-eb.i-code2[i] = ""
                     bf-eb.i-dscr2[i] = "" 
                     bf-eb.i-%2[i]    = 0.  
        
         end.
         if j <> 0 and i modulo j = 0 then counter = counter + 1.
         if counter > (eb.i-pass) then counter = eb.i-pass.         
      end. 

      {ce/updunit#.i bf-eb 0}
      {ce/updunit#.i bf-eb 1}

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
       (est.est-type NE 4 OR
        (est.form-qty EQ 1 AND ef.blank-qty EQ 1))              THEN DO:
      IF est.est-type EQ 2 THEN
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
    ll-rol = eb.len EQ 12 AND eb.dep EQ 0 AND eb.yld-qty GT 1.
  
    IF ll-rol THEN DO ON ENDKEY UNDO, LEAVE:
      MESSAGE "Is this estimate for a Roll?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE ll-rol.
    END.

    ll-set = eb.yld-qty GT 1 AND NOT ll-rol.
  END.

  FIND FIRST bf-eb
      WHERE bf-eb.company EQ xest.company
        AND bf-eb.est-no  EQ xest.est-no
        AND bf-eb.form-no EQ 0
      NO-ERROR.
      
  IF ll-set THEN DO:
    xest.est-type = 2.

    IF NOT AVAIL bf-eb THEN DO:
      {ce/set-info.a 2 "bf-" "x"}

      ASSIGN
       bf-eb.stock-no   = eb.stock-no
       bf-eb.part-no    = eb.part-no
       bf-eb.part-dscr1 = eb.part-dscr1
       bf-eb.part-dscr2 = eb.part-dscr2
       bf-eb.procat     = eb.procat
       bf-eb.tr-len     = eb.len
       bf-eb.tr-wid     = eb.wid
       bf-eb.tr-dep     = eb.dep.
       
      /*RUN cec/d-updset.w (RECID(eb),6).*/
    END.

    FIND FIRST fg-set WHERE fg-set.company = eb.company
                        AND fg-set.set-no = bf-eb.stock-no
                        AND fg-set.part-no = eb.stock-no NO-ERROR.
    IF AVAIL fg-set THEN DO:
      fg-set.part-qty = eb.yld-qty.
      FIND CURRENT fg-set NO-LOCK NO-ERROR.
    END.
  END.

  ELSE
  IF NOT AVAIL bf-eb OR bf-eb.part-no EQ "" THEN xest.est-type = 1.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-from-est B-table-Win 
PROCEDURE copy-from-est :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def buffer bf-new-eb for eb.
  def buffer bf-new-ef for ef.

  is-item-copied-from-est  = yes.

  find bf-new-eb where recid(bf-new-eb) = if recid(eb) <> ? then recid(eb) else lv-eb-recid .
  find bf-new-ef where recid(bf-new-ef) = if recid(eb) <> ? then recid(ef) else lv-ef-recid.
  
  buffer-copy xeb except xeb.company xeb.est-no xeb.form-no xeb.blank-no to bf-new-eb .
  buffer-copy xef except xef.company xef.est-no xef.form-no to bf-new-ef.
 
  
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
       assign eb.len:screen-value in browse {&browse-name} = string(xeb.len)
              eb.wid:screen-value in browse {&browse-name} = string(xeb.wid)
              eb.dep:screen-value in browse {&browse-name} = string(xeb.dep)
              eb.procat:screen-value in browse {&browse-name} = xeb.procat
              eb.style:screen-value in browse {&browse-name} = xeb.style
              eb.stock-no:screen-value in browse {&browse-name} = xeb.stock-no
              eb.part-dscr1:screen-value in browse {&browse-name} = xeb.part-dscr1
              eb.part-no:screen-value in browse {&browse-name} = xeb.part-no
              ef.board:screen-value in browse {&browse-name} = xef.board
              ef.cal:screen-value in browse {&browse-name} = string(xef.cal)
              eb.i-col:screen-value in browse {&browse-name} = string(xeb.i-col)
              eb.i-coat:screen-value in browse {&browse-name} = string(xeb.i-coat)
              .

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
      
      create e-itemfg-vend.
      
      ASSIGN e-itemfg-vend.company = eb.company
             e-itemfg-vend.item-type = NO
             e-itemfg-vend.est-no = eb.est-no
             e-itemfg-vend.eqty = eb.eqty
             e-itemfg-vend.form-no = eb.form-no
             e-itemfg-vend.blank-no = eb.blank-no
             e-itemfg-vend.run-qty[1] = 99999999.
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
            create e-itemfg-vend.
            BUFFER-COPY bf-e-itemfg-vend TO e-itemfg-vend
            ASSIGN e-itemfg-vend.est-no = eb.est-no
                 e-itemfg-vend.eqty = eb.eqty
                 e-itemfg-vend.form-no = eb.form-no
                 e-itemfg-vend.blank-no = eb.blank-no.

            CREATE reftable.
            ASSIGN
               reftable.reftable = "e-itemfg-vend.std-uom"
               reftable.company  = e-itemfg-vend.company
               reftable.loc      = ""
               reftable.code     = e-itemfg-vend.est-no
               reftable.val[1]   = e-itemfg-vend.form-no
               reftable.val[2]   = e-itemfg-vend.blank-no.

            FIND FIRST bf-e-itemfg WHERE
                 bf-e-itemfg.company EQ bf-e-itemfg-vend.company AND
                 bf-e-itemfg.i-no EQ bf-e-itemfg-vend.i-no
                 NO-LOCK NO-ERROR.

            IF AVAIL bf-e-itemfg THEN
            DO:
               reftable.code2 = bf-e-itemfg.std-uom.
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
  def buffer bf-notes for notes.
  

  FIND FIRST cust
      WHERE cust.company EQ eb.company
        AND cust.cust-no EQ eb.cust-no
      NO-LOCK NO-ERROR.
  
  if avail cust then
  for each notes
      where notes.rec_key   eq cust.rec_key
        and notes.note_type eq "D"
        and notes.note_code ne ""
      no-lock:

    find first bf-notes
        where bf-notes.rec_key   eq est.rec_key
          and bf-notes.note_type eq notes.note_type
          and bf-notes.note_code eq notes.note_code
      no-lock no-error.
      
    if not avail bf-notes then do:
      create bf-notes.
      buffer-copy notes except notes.note_form_no to bf-notes
      assign
       bf-notes.rec_key   = est.rec_key
       bf-notes.note_date = today
       bf-notes.note_time = time.
    end.
  end.

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
/*                                                                                                                                                      */
/*                                                                                                                                                      */
/*   FOR EACH est-prep                                                                                                                                  */
/*       WHERE est-prep.company EQ est.company                                                                                                          */
/*         AND est-prep.est-no  EQ est.est-no                                                                                                           */
/*       USE-INDEX est-qty NO-LOCK                                                                                                                      */
/*       BY est-prep.line DESC:                                                                                                                         */
/*     LEAVE.                                                                                                                                           */
/*   END.                                                                                                                                               */
/*   i = (IF AVAIL est-prep THEN est-prep.line ELSE 0) + 1.                                                                                             */
/*                                                                                                                                                      */
/*   for each prep where prep.company = gcompany and prep.dfault eq yes no-lock:                                                                        */
/*       create est-prep.                                                                                                                               */
/*       assign est-prep.e-num  = est.e-num                                                                                                             */
/*              est-prep.company = est.company                                                                                                          */
/*              est-prep.est-no = est.est-no                                                                                                            */
/*              est-prep.line   = i                                                                                                                     */
/*              est-prep.s-num  = eb.form-no                                                                                                            */
/*              est-prep.b-num  = 0 /*1 */                                                                                                              */
/*              est-prep.qty    = if prep.mat-type eq "r" and avail ef then ef.die-in                                                                   */
/*                                else if prep.mat-type eq "b" and  avail ef then ef.nsh-wid * ef.nsh-len /* ef.adh-sqin is 0 in Corrware - die inch */ */
/*                                else 1  /* mat-type eq "m" */                                                                                         */
/*             est-prep.code   = prep.code                                                                                                              */
/*             est-prep.dscr   = prep.dscr                                                                                                              */
/*             est-prep.cost   = prep.cost                                                                                                              */
/*             est-prep.spare-dec-1 = prep.spare-dec-1                                                                                                  */
/*             est-prep.ml     = prep.ml                                                                                                                */
/*             est-prep.simon  = prep.simon                                                                                                             */
/*             est-prep.mkup   = prep.mkup                                                                                                              */
/*             est-prep.amtz   = prep.amtz                                                                                                              */
/*             est-prep.mat-type = prep.mat-type.                                                                                                       */
/*                                                                                                                                                      */
/*       if lookup(est-prep.mat-type, "p,f") gt 0 then                                                                                                  */
/*           run sys/inc/flm-prep.p(recid(est), est-prep.s-num, output est-prep.qty).                                                                   */
/*       i = i + 1.                                                                                                                                     */
/*   end.                                                                                                                                               */
/*                                                                                                                                                      */
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
  def buffer bf-eb for eb.
  def var iCount as int no-undo.
  def var iArtiosCount as int no-undo.
  DEF VAR v-cust-no AS CHAR NO-UNDO.
  DEF VAR v-tmp LIKE eb.t-wid NO-UNDO.
  def var lv-layers AS DEC no-undo.
  DEF BUFFER b-eb FOR eb.
DEF BUFFER b-ef FOR ef.
    def buffer bf-est for est.
  def buffer bf-est-qty for est-qty.
DEF VAR lv-comm LIKE eb.comm NO-UNDO.
  DEF VAR lv-sman LIKE sman.sman NO-UNDO.
  DEF VAR ld-markup AS DEC NO-UNDO.

  
  for each tt-frmout no-lock:
      iArtiosCount = iArtiosCount + 1.
  end.
  
  ASSIGN
  ll-new-record = YES
  iCount = 0.
  for each tt-frmout NO-LOCK break by tt-frmout.form-no by tt-frmout.blank-no:
     iCount = iCount + 1.
      
    if iCount = 1 then do:
        RUN crt-new-est.
        /*RUN cec/new-est.p (IF iArtiosCount = 1 THEN 5 ELSE 6,
                      OUTPUT lv-crt-est-rowid).*/
     end. 
     
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
         assign eb.eqty = tt-frmout.quantity
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
      
        
        find FIRST cust where
            cust.company = gcompany and
            cust.cust-no = eb.cust-no
            no-lock no-error.

     if avail cust THEN DO: 
        ASSIGN 
         /*   eb.cust-no = cust.cust-no*/
            eb.sman = cust.sman  .

        IF AVAIL cust AND cust.sman NE "" THEN do:
          find sman where sman.company = gcompany
                   AND sman.sman = cust.sman
                 no-lock no-error.
          IF AVAIL sman THEN do:
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

      IF AVAIL shipto AND shipto.spare-char-1 NE "" THEN do:
          ASSIGN eb.sman = shipto.spare-char-1 .
          find sman where sman.company = gcompany
                   AND sman.sman = shipto.spare-char-1
                 no-lock no-error.
          eb.comm =  if avail sman then sman.scomm else 0. 
      END.

      find item where item.company = eb.company and
                     item.i-no = eb.cas-no
              no-lock no-error.
     if avail item then assign eb.cas-cnt = (item.box-case)
                              eb.cas-len = (item.case-l)
                              eb.cas-wid = (item.case-w)
                              eb.cas-dep = (item.case-d)
                              eb.cas-pal = (item.case-pall)
                              eb.cas-wt = (item.avg-w)         
                              .
     find FIRST item where item.company = eb.company and
                     item.i-no = eb.tr-no
              no-lock no-error.
     if avail item then assign eb.tr-len = (item.case-l)
                               eb.tr-wid = (item.case-w)
                               eb.tr-dep = (item.case-d)
                               .
   
     FIND FIRST item NO-LOCK
            WHERE item.company EQ gcompany
              AND item.i-no    EQ ef.board
            NO-ERROR.
        if avail item then do:
            assign ef.i-code = item.i-code
                  /*ef.flute = item.flute*/
                  ef.test = item.reg-no
                  ef.weight = item.basis-w.
            RUN sys/ref/uom-rm.p (item.mat-type, output uom-list).
            IF uom-list NE "" THEN ef.cost-uom = ENTRY(1,uom-list).
            if item.i-code = "R" then assign ef.lsh-len = item.s-len
                                             ef.lsh-wid = item.s-wid
                                             ef.gsh-wid = item.s-wid.
            if item.r-wid <> 0 then assign ef.roll = true
                                           ef.roll-wid = item.r-wid
                                           ef.lsh-wid = item.r-wid
                                           ef.gsh-wid = item.r-wid.  
           FIND FIRST e-item OF item NO-LOCK NO-ERROR.
           IF AVAIL e-item THEN ef.cost-uom = e-item.std-uom.                                        
        end.      
        find FIRST style where style.company = est.company and
                      style.style = eb.style
                      no-lock no-error.
     if avail style then do:
        assign eb.adhesive = style.material[7]
               eb.gluelap = style.dim-gl
               eb.fpanel = style.dim-pan5
               eb.lock = style.dim-fit
               eb.tuck = style.dim-tk.
        FIND FIRST ITEM WHERE ITEM.company = eb.company 
                          AND ITEM.i-no = eb.adhesive NO-LOCK NO-ERROR.
        IF AVAIL ITEM AND index("G,S,T",ITEM.mat-type) > 0 AND ITEM.i-no <> "No Joint"
        THEN eb.lin-in = eb.dep.
     end.  /* avail style */ 

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
          WHERE b-ef.company eq b-eb.company
            AND b-ef.est-no  eq b-eb.est-no
            AND b-ef.form-no eq b-eb.form-no
          NO-LOCK NO-ERROR.
      IF AVAIL b-ef THEN
        ASSIGN
       /*  ef.board  = b-ef.board */
         ef.cal    = (b-ef.cal).

  END.

  FIND xest WHERE ROWID(xest) EQ ROWID(est) NO-LOCK NO-ERROR.
  FIND xef WHERE ROWID(xef)   EQ ROWID(ef)  NO-LOCK NO-ERROR.
  find xeb where recid(xeb) = recid(eb) NO-LOCK NO-ERROR.

  run calc-pass.

  /*RUN calc-layout (YES).*/
  
  run calc-blank-size.
  
  ASSIGN 
      ef.gsh-wid = eb.t-wid
      ef.gsh-len = eb.t-len
      ef.nsh-wid = eb.t-wid
      ef.nsh-len = eb.t-len
      ef.trim-w = eb.t-wid
      ef.trim-l = eb.t-len .

  RUN cec/mach-seq.p (ef.form-no, est-qty.eqty, NO).

     RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-source",OUTPUT char-hdl).
     IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
     RUN init-box-design IN WIDGET-HANDLE(char-hdl) (THIS-PROCEDURE).

     RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"box-calc-target",OUTPUT char-hdl).
     IF VALID-HANDLE(WIDGET-HANDLE(ENTRY(1,char-hdl))) THEN
     RUN build-box IN WIDGET-HANDLE(ENTRY(1,char-hdl)) ("B").

     FIND FIRST box-design-hdr where
          box-design-hdr.design-no EQ 0 and
          box-design-hdr.company EQ cocode AND
          box-design-hdr.est-no EQ est.est-no AND
          box-design-hdr.form-no eq tt-artios.form-num AND
          box-design-hdr.blank-no eq tt-artios.blank-num
          NO-ERROR.
      
        IF AVAIL box-design-hdr THEN
        DO:
           box-design-hdr.box-image = tt-artios.DesignImg.
           FIND CURRENT box-design-hdr NO-LOCK NO-ERROR.
        END. 

  end.  /* for each tt-frmout */

  if iCount > 0 then do:
     RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
     run new_record in widget-handle(char-hdl)  (lv-crt-est-rowid).
  end. 

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
 
  /*
  def var i as int no-undo.
  def buffer bb for eb.
  def buffer bf for ef.
  
  
  create est-qty.
  assign est-qty.company = gcompany
         est-qty.est-no =  est.est-no
         est-qty.eqty = 0
         est-qty.qty-date = est.est-date
         .
  create ef.
  assign
   ef.est-type  = est.est-type
   ef.company   = gcompany
   ef.loc       = gloc
   ef.e-num     = est.e-num
   ef.est-no    = est.est-no
   ef.form-no   = 1
   ef.cust-seq  = 1
   ef.blank-qty = 1
   ef.lsh-wid   = ce-ctrl.ls-length
   ef.lsh-len   = ce-ctrl.ls-width
   lv-ef-recid  = recid(ef).
 
  RUN blank-add (lv-ef-recid).*/

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

def input parameter v-item like itemfg.i-no.
/*
{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared buffer xest    for est.
def shared buffer xef     for ef.
def shared buffer xeb     for eb.
*/
def var cocode as cha no-undo.
def var locode as cha no-undo.
def var tmpstore as cha no-undo.
def var i as int no-undo.

assign cocode = gcompany
       locode = gloc
       .
{ce/msfcalc.i}
{oe/fgfreight.i}

find first cust  where cust.company eq gcompany
                   and cust.cust-no eq xeb.cust-no
    no-lock no-error.
create itemfg.
assign
 itemfg.company    = gcompany
 itemfg.loc        = gloc
 itemfg.i-no       = v-item
 itemfg.i-code     = "C"
 itemfg.i-name     = xeb.part-dscr1
 itemfg.part-dscr1 = xeb.part-dscr2
 itemfg.sell-uom   = "M"
 itemfg.part-no    = xeb.part-no
 itemfg.cust-no    = xeb.cust-no
 itemfg.cust-name  = if avail cust then cust.name else ""
 itemfg.pur-uom    = IF xeb.pur-man THEN "EA" ELSE "M"
 itemfg.prod-uom   = IF xeb.pur-man THEN "EA" ELSE "M"
 itemfg.stocked    = yes
 itemfg.die-no     = xeb.die-no
 itemfg.plate-no   = xeb.plate-no
 itemfg.style      = xeb.style
 itemfg.procat     = xeb.procat
 itemfg.cad-no     = xeb.cad-no
 itemfg.upc-no     = xeb.upc-no
 itemfg.spc-no     = xeb.spc-no
 itemfg.isaset     = (xest.est-type eq 2 or xest.est-type eq 6) and
                     xeb.form-no eq 0
 itemfg.pur-man    = xeb.pur-man 
 itemfg.alloc      = xeb.set-is-assembled.

 RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT "").
 RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT xeb.loc).
 /* gdm - 11190901*/
 IF xeb.ship-id NE "" THEN DO:
  FIND FIRST shipto no-lock
    WHERE shipto.company EQ xeb.company
      AND shipto.cust-no EQ xeb.cust-no
      AND shipto.ship-id EQ xeb.ship-id NO-ERROR.
  IF AVAIL shipto THEN ASSIGN itemfg.ship-meth = shipto.ship-meth.
 END.
 /* gdm - 11190901 */

 IF itemfg.alloc NE ? THEN itemfg.alloc = NOT itemfg.alloc.

{oe/fgfreighta.i xeb}


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
   
{est/fgupdtax.i xeb }

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
  def buffer bb for eb.
  def buffer bf for ef.

  ll-new-record = YES.

  RUN ce/new-est.p (IF ls-add-what EQ "est" THEN 1 ELSE 2,
                    OUTPUT lv-crt-est-rowid).

  FIND eb WHERE ROWID(eb) EQ lv-crt-est-rowid NO-LOCK NO-ERROR.
  FIND FIRST ef OF eb NO-LOCK NO-ERROR.
  FIND FIRST est OF ef NO-LOCK NO-ERROR.
  ASSIGN
    lv-eb-recid = RECID(eb)
    lv-ef-recid = RECID(ef).

  /* ???? bugs : 2 records are created  , delete one ========== */
  for each bb WHERE bb.company = ""
                AND bb.est-no = "" :
      delete bb.
  end.
  for each bf where bf.company = "" AND bf.est-no = "" :
      delete bf.
  end.
      
  /*    
  def var ll-dumb as log no-undo.
    
  /*  don't use e-num any more as key index
  find last bf-est use-index e-num no-lock no-error.
  li-enum = if avail bf-est then bf-est.e-num else 0.
  */

  find first ce-ctrl where ce-ctrl.company = gcompany and
                           ce-ctrl.loc = gloc.
                          
  li-new-estnum = ce-ctrl.e-num + 1.
  ll-new-record = yes.
  ce-ctrl.e-num = li-new-estnum.

  CREATE est.
  assign est.est-type = IF ls-add-what EQ "est" THEN 1 ELSE 2
         est.company = gcompany
         est.loc = gloc
         est.est-no = string(li-new-estnum,">>>>>>>9")
         est.form-qty = 1
         est.est-date = today
         est.mod-date = ?
         .
  {sys/ref/est-add.i est}     
      
  run crt-est-childrecord.  /* create ef,eb,est-prep */
  lv-crt-est-rowid = rowid(eb).*/

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
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"form-blank-target",OUTPUT char-hdl).

    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
      RUN repo-on-off IN WIDGET-HANDLE(char-hdl) ("OFF").
      RUN repo-query IN WIDGET-HANDLE(char-hdl) (ROWID(eb)).
      RUN repo-on-off IN WIDGET-HANDLE(char-hdl) ("ON").
    END.
  END.

    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"container-source",OUTPUT char-hdl).

    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
      RUN set-first IN WIDGET-HANDLE(char-hdl) NO-ERROR.

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
  for each est-qty where est-qty.company = est.company 
                     and est-qty.est-no = est.est-no
                     :
     for each eb where eb.company = est.company 
                   and eb.est-no = est.est-no
                   and eb.eqty = est-qty.eqty:
         delete eb.             
     end.
     for each ef where ef.company = est.company 
                   and ef.est-no = est.est-no
                   and ef.eqty = est-qty.eqty:
         delete ef.             
     end.
     delete est-qty.
  end.   
  for each est-prep where est-prep.company = est.company
                      and est-prep.est-no = est.est-no:
      delete est-prep.
  end.
  for each est-op where est-op.company = est.company
                    and est-op.est-no = est.est-no:
      delete est-op.
  end.
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


  RUN crt-new-est.

  FIND b-eb WHERE RECID(b-eb) EQ lv-eb-recid NO-LOCK NO-ERROR.

  IF AVAIL b-eb THEN DO:
    RUN est/d-selest.w (ROWID(b-eb), NO, "",
                        OUTPUT ll-new-tandem, OUTPUT lv-eb-rowid).

    IF ll-new-tandem THEN DO:
      FIND FIRST xest OF b-eb NO-LOCK NO-ERROR.

      RELEASE xeb.

      RUN est/oeselest.p.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE form-add B-table-Win 
PROCEDURE form-add :
/*------------------------------------------------------------------------------
  Purpose:     ce/com/form-add.p
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ip-recid AS RECID NO-UNDO.

DEF VAR prev-board LIKE ef.board NO-UNDO.

DEF BUFFER bf FOR ef.
DEF BUFFER bf-est FOR est.


FIND bf-est WHERE RECID(bf-est) = ip-recid.

FIND FIRST ce-ctrl {sys/look/ce-ctrlW.i} NO-LOCK NO-ERROR.

IF bf-est.est-type NE 2 THEN bf-est.est-type = 4.

FIND LAST bf 
    WHERE bf.company EQ est-qty.company
      AND bf.est-no  EQ est-qty.est-no
      AND bf.eqty    EQ est-qty.eqty
    USE-INDEX est-qty NO-LOCK NO-ERROR.
IF AVAIL bf THEN
  ASSIGN
   i          = bf.form-no
   prev-board = bf.board.
ELSE i = 0.

CREATE ef.
ASSIGN
      ef.e-num     = est.e-num
      ef.form-no   = i + 1
      ef.cust-seq  = 1
      ef.est-type  = bf-est.est-type
      ef.company   = cocode
      ef.loc       = locode
      ef.est-no    = est.est-no
      ef.blank-qty = 1
      ef.lam-dscr  = IF ce-ctrl.avg-cscost EQ 1 THEN "R" ELSE "S"
      ef.f-pass    = 0
      ef.board = prev-board
      lv-ef-recid = RECID(ef).

RUN est/frms-est.p (ROWID(est)).
FIND CURRENT est NO-ERROR.

RUN blank-add (lv-ef-recid).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record B-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-eb FOR eb.


  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN
   ll-add-set = NO
   lv-cad-no  = ""
   lv-die-no  = "".
  
  IF NOT ll-is-add-from-tool THEN DO:
    ls-add-what = "est" .   /* new estimate */
    run est/d-addfol.w (INPUT no, output ls-add-what). /* one item or set cec/est-add.p */
    if ls-add-what = "" then return no-apply.  /* cancel */
  END.

  IF CAN-DO("form,blank",ls-add-what) THEN DO:
    {custom/checkuse.i}

    IF AVAIL eb AND eb.est-type EQ 2 THEN DO:
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

  ASSIGN
    ll-is-add-from-tool = NO
    cadcamValue = ''.

  IF ls-add-what EQ "est-from-tandem" THEN RUN est-from-tandem.
  ELSE IF ls-add-what = "farm" THEN DO:
     empty temp-table tt-frmout.
     run est/d-frmout.w (cocode).
     run createESTFarmOut.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-apply-entry B-table-Win 
PROCEDURE local-apply-entry :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  
  IF NOT AVAIL eb  THEN RETURN.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'apply-entry':U ) .

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
  def var is-first-record as log init no no-undo.
  def buffer bf-eb for eb.  
  def buffer bf-ef for ef.
  def buffer bf-est for est.
  DEF BUFFER b-ef FOR ef.
  DEF BUFFER b-eb FOR eb.
  DEF BUFFER b-ref FOR reftable.

  DEF VAR char-hdl AS cha NO-UNDO.
  def var i as int no-undo.
  def var xx as dec no-undo.
  def var lv-hld-cust like eb.cust-no no-undo.
  def var lv-hld-ship like eb.ship-id no-undo.
  DEF VAR li-est-type AS INT NO-UNDO.
  DEF VAR lv-hld-bl-qty LIKE eb.bl-qty NO-UNDO.
  DEF VAR lv-hld-icol LIKE eb.i-col NO-UNDO.
  DEF VAR lv-hld-icot LIKE eb.i-coat NO-UNDO.
  DEF VAR lv-hld-fcol LIKE ef.f-col NO-UNDO.
  DEF VAR lv-hld-fpas LIKE ef.f-pass NO-UNDO.
  DEF VAR lv-hld-fcot LIKE ef.f-coat NO-UNDO.
  DEF VAR lv-hld-fctp LIKE ef.f-coat-p NO-UNDO.
  DEF VAR lv-die-in LIKE ef.die-in NO-UNDO.
  DEF VAR lv-hld-wid LIKE eb.wid NO-UNDO.
  DEF VAR lv-hld-len LIKE eb.len NO-UNDO.
  DEF VAR lv-hld-dep LIKE eb.dep NO-UNDO.
  DEF VAR lv-hld-style LIKE eb.style NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lj AS INT NO-UNDO.
  def var uom-list as cha no-undo.
  DEF VAR lv-hld-board AS cha NO-UNDO.
  DEF VAR ld-markup AS DEC NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.
  DEF VAR lv-box-des AS CHAR INIT "S" NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  if not avail eb then find eb where recid(eb) = lv-eb-recid no-lock no-error.
  if not avail ef then find ef where recid(ef) = lv-ef-recid no-lock no-error.
  
  assign
   lv-hld-bl-qty = eb.bl-qty
   lv-hld-cust   = eb.cust-no
   lv-hld-ship   = eb.ship-id
   lv-hld-icol   = eb.i-col
   lv-hld-icot   = eb.i-coat
   lv-hld-fcol   = ef.f-col
   lv-hld-fpas   = ef.f-pass
   lv-hld-fcot   = ef.f-coat
   lv-hld-fctp   = ef.f-coat-p
   lv-hld-wid    = eb.wid
   lv-hld-len    = eb.len
   lv-hld-dep    = eb.dep
   lv-hld-style  = eb.style
   lv-hld-board  = ef.board.

  {est/blankcp2.i}

  run get-attribute in adm-broker-hdl ('Is-First-Est').
  if return-value = "Yes" then do:
       is-first-record = yes.
       run set-attribute-list in adm-broker-hdl ('Is-First-Est=No'). /* reset */
  end.     
  else is-first-record = no.
  /* Change 4th enabled record lock status PROGRESS only does upto 3 records 
     est,est-qty,eb,ef (ef has still no-lock status) */
  FIND CURRENT ef EXCLUSIVE-LOCK NO-ERROR NO-WAIT.  
  
  viEQtyPrev = eb.eqty.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
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

  IF NOT ll-is-copy-record THEN DO:
    {ce/uship-id.i ll-new-record}
  END.

  FOR EACH tt-eb-set BREAK BY tt-eb-set.company:
    IF FIRST(tt-eb-set.company) THEN DO:
      CREATE bf-eb.
      BUFFER-COPY tt-eb-set TO bf-eb
      ASSIGN
       bf-eb.cust-no = eb.cust-no.
      IF bf-eb.stock-no NE "" AND bf-eb.stock-no NE eb.stock-no            AND
         NOT CAN-FIND(FIRST itemfg WHERE itemfg.company EQ cocode
                                     AND itemfg.i-no    EQ bf-eb.stock-no) THEN DO:
        FIND xest WHERE ROWID(xest) EQ ROWID(est) NO-LOCK NO-ERROR.
        FIND xeb WHERE ROWID(xeb) EQ ROWID(bf-eb) NO-LOCK NO-ERROR.
        RUN fg/ce-addfg.p (bf-eb.stock-no).
      END.
    END.
    DELETE tt-eb-set.
  END.

  RUN update-sb-qtys (ROWID(eb)).

  FIND FIRST est-qty
      WHERE est-qty.company EQ gcompany
        AND est-qty.est-no  EQ est.est-no
        AND est-qty.eqty    EQ eb.bl-qty
      NO-ERROR.
  IF NOT AVAIL est-qty THEN
  FIND FIRST est-qty
      WHERE est-qty.company EQ gcompany
        AND est-qty.est-no  EQ est.est-no
        AND est-qty.eqty    EQ lv-hld-bl-qty
      NO-ERROR.
  IF NOT AVAIL est-qty THEN
  FIND FIRST est-qty
      WHERE est-qty.company EQ gcompany
        AND est-qty.est-no  EQ est.est-no
      NO-ERROR.

  IF AVAIL est-qty THEN DO:
      FOR EACH est-op
          WHERE est-op.company EQ est.company
            AND est-op.est-no  EQ est.est-no
            AND est-op.qty     EQ est-qty.eqty:
        est-op.qty = eb.bl-qty.
      END.

      est-qty.eqty = eb.bl-qty.

      FIND CURRENT est-qty NO-LOCK.

      /*== update all eb,ef eqty field ==*/
      for each bf-eb where bf-eb.company = est-qty.company and
                           bf-eb.est-no = est-qty.est-no:
          assign bf-eb.eqty = est-qty.eqty.
      end.  
      for each bf-ef where bf-ef.company = est-qty.company and
                           bf-ef.est-no = est-qty.est-no:
             bf-ef.eqty = est-qty.eqty.
      end.
      for each est-flm where est-flm.company = est-qty.company and
                           est-flm.est-no = est-qty.est-no:
             est-flm.eqty = est-qty.eqty.
      end.

      find bf-est where bf-est.company = est-qty.company and
                        bf-est.est-no = est-qty.est-no.
      bf-est.est-qty[1] = est-qty.eqty.
  END.

  {ce/estitm1.i}

   if v-shiptorep-log AND adm-new-record THEN do:  /* task 05301401 */
      RUN ce/markup.p (eb.company, ROWID(eb), OUTPUT ld-markup).
      RUN sys/inc/getsmncm-2.p (eb.cust-no, INPUT-OUTPUT eb.sman, eb.procat, ld-markup,
                              OUTPUT eb.comm,eb.ship-id).
  END.   

  if not ll-is-copy-record             and
     (ll-new-record or is-first-record) then do:
     ef.xgrain = "N".

     find xest where recid(xest) = recid(est).
     find xef where recid(xef) = recid(ef).
     find xeb where recid(xeb) = recid(eb).  

     run create-inst.

     IF xeb.blank-no EQ 1 AND ll-new-record THEN RUN create-prep.
  END.  /* new not copy */

  FIND CURRENT eb.

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
  /*ELSE DO:
     RUN est/u2kinc1.p (RECID(eb)).
     RUN est/u2kinc2.p (RECID(eb)).
     find first formule.

     eb.die-in = 0.
     do i = 1 to 4:
        if ef.leaf[i] ne "" and ef.leaf-bnum[i] ne 0 and
           ((ef.leaf-w[i] ne 0) and (ef.leaf-l[i] ne 0)) THEN do:
          find first item {sys/look/itemW.i} and item.i-no eq ef.leaf[i]
              no-lock no-error.
          if item.mat-type ne "W" then next.
          eb.die-in = eb.die-in +
                      ((ef.leaf-w[i] + ef.leaf-l[i]) * 2 * eb.num-up).
        end.
     end.

     eb.die-in = eb.die-in + (formule[12] * eb.num-up).
  END.*/                       

  if ll-crt-itemfg then do:
     find xest where recid(xest) = recid(est) no-lock no-error.
     find xeb where recid(xeb) = recid(eb) no-lock no-error.
     find xef where recid(xef) = recid(ef) no-lock no-error.
     run fg/ce-addfg.p (xeb.stock-no).
     FIND FIRST xeb NO-LOCK
         WHERE xeb.company  EQ eb.company
           AND xeb.est-no   EQ eb.est-no
           AND xeb.form-no  EQ 0
           AND xeb.stock-no NE ""
           AND NOT CAN-FIND(FIRST itemfg
                            WHERE itemfg.company EQ xeb.company
                              AND itemfg.i-no    EQ xeb.stock-no)
         NO-ERROR.
     IF AVAIL xeb THEN RUN fg/ce-addfg.p (xeb.stock-no).
     ll-crt-itemfg = no.
  end.

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

  FIND CURRENT est.
  FIND CURRENT est-qty NO-LOCK.

  IF (li-est-type GE 1 AND li-est-type LE 2 AND eb.i-col + eb.i-coat EQ 0) OR
     (li-est-type GE 3 AND li-est-type LE 4 AND ef.f-col + ef.f-coat EQ 0) THEN DO:
    {ce/delplate.i}
  END.

  IF li-est-type GE 3 AND li-est-type LE 4 THEN
    IF lv-hld-fcol NE ef.f-col    OR
       lv-hld-fpas NE ef.f-pass   OR
       lv-hld-fcot NE ef.f-coat   OR
       lv-hld-fctp NE ef.f-coat-p THEN DO:
      {sys/inc/flm-prep.i}
    END.
      
  IF NOT ll-new-record          AND
     (lv-hld-icol NE eb.i-col OR
      lv-hld-icot NE eb.i-coat) THEN DO:

    EMPTY TEMP-TABLE inks.

    DO li = 1 TO EXTENT(eb.i-code2):
      CREATE inks.
      ASSIGN
       inks.ps[1] = eb.i-ps2[li]
       inks.cd[1] = eb.i-code2[li]
       inks.ds[1] = eb.i-dscr2[li]
       inks.pc[1] = eb.i-%2[li].
    END.

    RUN est/def-inks.p (ROWID(eb), eb.i-col, eb.i-pass, eb.i-coat, eb.i-coat-p).

    ASSIGN
     li         = 0
     eb.i-ps2   = 0
     eb.i-code2 = ""
     eb.i-dscr2 = ""
     eb.i-%2    = 0.

    FOR EACH inks BY inks.iv:
      li = li + 1.
      IF li LE EXTENT(eb.i-code2) THEN
        ASSIGN
         eb.i-ps2[li]   = inks.ps[1]
         eb.i-code2[li] = inks.cd[1]
         eb.i-dscr2[li] = inks.ds[1]
         eb.i-%2[li]    = inks.pc[1].
    END.

    {ce/updunit#.i eb 0}
    {ce/updunit#.i eb 1}
  END.

  IF est.est-type GT 1                           AND
     (adm-new-record OR eb.yld-qty LT eb.bl-qty) THEN
    RUN set-yld-qty (ROWID(eb)).

  RUN ce/com/istandem.p (ROWID(est), OUTPUT ll-tandem).

  IF cegoto-log                     OR
     (est.est-type EQ 4       AND
      old-bl-qty NE eb.bl-qty AND
      NOT ll-new-record       AND
      NOT ll-tandem)                THEN RUN run-goto.

  if not ll-is-copy-record and ceroute-log AND li-est-type EQ 1 then do:
     find xest where recid(xest) = recid(est).
     find xef where recid(xef) = recid(ef).
     find xeb where recid(xeb) = recid(eb).

     for each est-op
         where est-op.company eq xest.company
           and est-op.est-no  eq xest.est-no
           and est-op.line    ge 500:
       delete est-op.
     end.
    
     if can-find(first est-op where est-op.company eq xest.company
                                and est-op.est-no  eq xest.est-no
                                and est-op.s-num   eq xef.form-no) then
     for each est-op
         where est-op.company eq xest.company
           and est-op.est-no  eq xest.est-no
           and est-op.s-num   eq xef.form-no
         no-lock:
     end.
  
     else do:
       /* Protect existing est-op records */
       FOR EACH tt-est-op:
         DELETE tt-est-op.
       END.

       for each est-op
           where est-op.company eq xest.company
             and est-op.est-no  eq xest.est-no:
         CREATE tt-est-op.
         BUFFER-COPY est-op TO tt-est-op.
         DELETE est-op.
       end.
                                
       xx = dec(xef.form-no).

       run ce/mach-seq.p (est-qty.eqty).

       for each est-op
           where est-op.company eq xest.company
             and est-op.est-no  eq xest.est-no
             and est-op.s-num   ne int(xx):
         delete est-op.
       end.

       FOR EACH tt-est-op:
         CREATE est-op.
         BUFFER-COPY tt-est-op TO est-op.
         DELETE tt-est-op.
       END.
     end.
  end.
      
  IF est.est-type NE 4 THEN
  FOR EACH bf-eb
      WHERE bf-eb.company EQ eb.company
        AND bf-eb.est-no  EQ eb.est-no
        AND ROWID(bf-eb)  NE ROWID(eb):
    assign
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

  IF cestyle-log                                     AND
     (adm-adding-record OR lv-hld-style NE eb.style) THEN DO:

    IF NOT adm-new-record THEN
      MESSAGE "Do you wish to reset box design?"
          VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE ll-ans2 AS LOG.
    ELSE ll-ans2 = YES.

    IF ll-ans2 THEN
       lv-box-des = "B".
    ELSE
       lv-box-des = "N".
  END.

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"boxdes-target",OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(ENTRY(1,char-hdl))) THEN
    RUN build-box IN WIDGET-HANDLE(ENTRY(1,char-hdl)) (lv-box-des).

  IF eb.pur-man THEN ef.nc = NO.

  IF adm-new-record AND eb.pur-man THEN RUN create-e-itemfg-vend.
  ELSE IF eb.pur-man AND eb.eqty <> viEQtyPrev THEN RUN update-e-itemfg-vend.

  ll-new-shipto = NO.
  RUN valid-eb-reckey.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record B-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN release-shared-buffers.
  
    IF ll-new-record THEN do:
        RUN get-link-handle IN adm-broker-hdl  (THIS-PROCEDURE,'Record-source':U,OUTPUT char-hdl).
        RUN New_Record IN WIDGET-HANDLE(char-hdl) (v-rowid-eb).
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
   DEF VAR v-neweb-est AS CHAR NO-UNDO.
   def var char-hdl as cha no-undo.
   DEF VAR li AS INT NO-UNDO.
   def var ll-dumb as log no-undo.

  /* Code placed here will execute PRIOR to standard behavior. */
  {custom/checkuse.i}

  RUN ce/d-cpwhat.w (OUTPUT lv-copy-what).
  IF lv-copy-what = "" THEN RETURN.

  IF lv-copy-what EQ "copy" THEN do:
      RUN ce/copyestN.w (lv-copy-what, est.est-no, OUTPUT v-neweb-est ) .
      IF v-neweb-est NE "" THEN do:
          FIND FIRST est WHERE est.company EQ cocode
              AND est.est-no EQ FILL(" ",8 - LENGTH(TRIM(v-neweb-est))) + TRIM(v-neweb-est) NO-LOCK NO-ERROR .
          
          IF AVAIL est THEN do:
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
  def var char-hdl as cha no-undo.
  def var ll-dumb as log no-undo.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR ll-error AS LOG NO-UNDO.

  DEF BUFFER b-eb FOR eb.
  DEF BUFFER b-ef FOR ef.
  DEF BUFFER b-est-flm FOR est-flm.
  DEF BUFFER est-misc FOR reftable.
  IF AVAIL eb THEN
      v-rowid-eb  = ROWID(eb).
  /* Code placed here will execute PRIOR to standard behavior. */
  if avail est then li-form# = est.form-qty. /* for set creation on crt-new-set */
    
  ls-cust-no = if avail eb then eb.cust-no else "" .  /* for new item record */
  ls-ship-id = if avail eb then eb.ship-id else "" .  /* for crt-new-set */

  /* Dispatch standard ADM method.                             */
  /*RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ).*/

  /* Code placed here will execute AFTER standard behavior.    */
  assign
   ls-set-part-no    = ""
   ll-add-set        = no.

  IF lv-copy-what = "" THEN DO:
    IF ls-add-what BEGINS "est" THEN DO:
      RUN crt-new-est.

      ll-add-set = ls-add-what eq "estset".

      /* refresh browser for new record */
      FIND b-eb WHERE RECID(b-eb) EQ lv-eb-recid NO-LOCK NO-ERROR.

      RUN get-link-handle IN adm-broker-hdl  (THIS-PROCEDURE,'Record-source':U,OUTPUT char-hdl).
      RUN New_Record-user IN WIDGET-HANDLE(char-hdl) (ROWID(b-eb)).

      ll-dumb = {&browse-name}:REFRESH() IN FRAME {&FRAME-NAME}.

      RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
      RUN new-state IN WIDGET-HANDLE(char-hdl) ('update-begin':U).  /* to have save button */

      DISPLAY est.est-no est.est-date WITH BROWSE {&browse-name}.
    END.

    ELSE
    IF ls-add-what = "form" THEN  DO:
      /* create ef,eb..*/
      ll-new-record = YES.
      RUN form-add (RECID(est)).  
    END.

    ELSE DO:
      /* create eb */
      ll-new-record = YES.
      RUN blank-add (RECID(ef)).
    END.

    {est/d-cadcam.i}
  END.

  ELSE DO:  /* copy */
    ll-new-record = YES.

    /* create ef and/or eb..*/
    IF lv-copy-what EQ "form" THEN RUN form-add (RECID(est)).
                              ELSE RUN blank-add (RECID(ef)). 

    RUN est/d-copy.w (lv-copy-what, lv-ef-copy-frid, lv-eb-copy-frid,
                      BUFFER ef, BUFFER eb, OUTPUT ll-error).

    FIND CURRENT eb NO-ERROR.
    FIND CURRENT ef NO-ERROR.

    /*IF ll-error THEN DO:
      DELETE eb.
      IF lv-copy-what EQ "form" THEN DELETE ef.
      RETURN "adm-error".
    END.*/
  
    ASSIGN BROWSE {&BROWSE-NAME} {&adm-tableio-fields} NO-ERROR.
  END.    /* end of copy */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def buffer bb for eb.
  def buffer bf for ef.
  def buffer best for est.
  def var li as int no-undo.
  def var ll-dum as log no-undo.
  def var char-hdl as cha no-undo.
  def var lv-rowid as rowid no-undo.
  def var li-est-type like est.est-type no-undo.
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
          RUN first-run IN WIDGET-HANDLE(char-hdl).
          RUN dispatch IN WIDGET-HANDLE(char-hdl) ("row-changed").
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
     if not avail eb then return.

     find style where style.company = gcompany and
                      style.style = eb.style:screen-value in browse {&browse-name}
                      no-lock no-error.   
     lv-foam = if avail style and style.type = "F" then yes else no.
     
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE li-cnt AS INTEGER NO-UNDO.

  DEF BUFFER enable-eb FOR eb.
    
  /* Code placed here will execute PRIOR to standard behavior. */
  {custom/checkuse.i}

  DO WITH FRAME {&FRAME-NAME}:
    /* move cursor to left end */
    DO li-cnt = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY "cursor-left" TO {&BROWSE-NAME}.
    END. /* do li-cnt */
  END. /* with frame */
   
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  RUN release-shared-buffers.

  DO WITH FRAME {&FRAME-NAME}:
    FOR EACH enable-eb
        WHERE enable-eb.company EQ est-qty.company
          AND enable-eb.est-no  EQ est-qty.est-no
          AND enable-eb.eqty    EQ est-qty.eqty
          AND enable-eb.form-no NE 0
        NO-LOCK BY enable-eb.form-no BY enable-eb.blank-no:

      LEAVE.
    END.
    IF AVAIL eb AND AVAIL enable-eb AND ROWID(enable-eb) EQ ROWID(eb) THEN
      APPLY "entry" TO eb.cust-no IN BROWSE {&browse-name}.
    ELSE
    IF AVAIL eb THEN
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

  EMPTY TEMP-TABLE tt-eb-set.

  lv-copied = ?.
  ll-new-shipto = NO.

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
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
  {methods/winReSizeLocInit.i}
  
  find first sys-ctrl where sys-ctrl.company eq cocode
                        and sys-ctrl.name    eq "CE W>L"
       no-lock no-error.
  if not avail sys-ctrl then do transaction:
     create sys-ctrl.
     assign sys-ctrl.company = cocode
            sys-ctrl.name    = "CE W>L"
            sys-ctrl.descrip = "Default to display Warning when Carton Width > Length."
            sys-ctrl.log-fld = yes.
     MESSAGE sys-ctrl.descrip
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
         UPDATE sys-ctrl.log-fld.
  end.
  ll-warn = sys-ctrl.log-fld.

     assign lv-eb-recid = recid(eb)
            lv-ef-recid = recid(ef).

     

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
  lv-repo = "OFF".

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  IF num-results("{&BROWSE-NAME}":U) > 0 THEN RUN dispatch ('get-last':U).
  IF AVAIL eb THEN
    ASSIGN
     lv-last-rowid  = ROWID(eb)
     lv-last-rowid2 = ROWID(ef).

  IF num-results("{&BROWSE-NAME}":U) > 0 THEN RUN dispatch ('get-first':U).
  IF AVAIL eb THEN
    ASSIGN
     lv-frst-rowid  = ROWID(eb)
     lv-frst-rowid2 = ROWID(ef).

  RUN setFarmTab.

  lv-repo = "ON".

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
  def var lv-rowid as rowid no-undo.
  def var ll-dumb as log no-undo.
  def var char-hdl as cha no-undo.
  def var li-row-num as int no-undo.
  def var li-cnt as int no-undo.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lActive AS LOG NO-UNDO.
  DEFINE VARIABLE rEfRow AS ROWID       NO-UNDO.
  DEFINE VARIABLE rEbRow AS ROWID       NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
    RUN valid-cust-user NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     IF eb.stock-no:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN DO:
        RUN fg/GetItemfgActInact.p (INPUT g_company,
                                    INPUT eb.stock-no:SCREEN-VALUE,
                                    OUTPUT lActive).
        IF NOT lActive THEN DO:
/*         FIND FIRST reftable WHERE reftable.reftable EQ "FGSTATUS"     */
/*                     AND reftable.company  EQ g_company                */
/*                     AND reftable.loc      EQ ""                       */
/*                     AND reftable.code     EQ eb.stock-no:SCREEN-VALUE */
/*                     NO-LOCK NO-ERROR.                                 */
/*         IF AVAIL reftable AND reftable.code2 = "I" THEN DO:           */
           MESSAGE eb.stock-no:SCREEN-VALUE + " has InActive Status. Order cannot be placed for the Inactive Item."
                VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO eb.stock-no.
           RETURN .
        END.
    END.

    RUN blank-cp (NO).

    RUN valid-part-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    /* == validation ===== */
     if int(eb.bl-qty:screen-value in browse {&browse-name}) <= 0 then do:
        message "Quantity must be entered. " view-as alert-box error.
        apply "entry" to eb.bl-qty.
        return no-apply.
     end.

    RUN valid-style NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     if /* eb.cust-no:screen-value <> "" and */
        not can-find(cust where cust.company = gcompany and cust.cust-no = eb.cust-no:screen-value)
     then do:
        message "Invalid Customer Number. Try Help." view-as alert-box error.
        apply "Entry" to eb.cust-no.
        return no-apply.
     end.

     RUN valid-ship-id NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-stock-no NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     if ef.board:screen-value in browse {&browse-name} <> "" and
        not can-find(item where item.company = gcompany
                     and item.i-no = ef.board:screen-value in browse {&browse-name} )
     then do:
        message "Invalid Board. Try Help. " view-as alert-box error.
        apply "entry" to ef.board.
        return no-apply.
     end.

     RUN valid-procat NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-wid-len NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     if decimal(eb.len:screen-value) = 0 then do:
        message "Length can not be 0. " view-as alert-box error.
        apply "entry" to eb.len.
        return no-apply.
     end.
     if decimal(eb.wid:screen-value) = 0 then do:
        message "Width can not be 0. " view-as alert-box error.
        apply "entry" to eb.wid.
        return no-apply.
     end.
  END.
  
  /* ====== end validation =======*/
  ASSIGN
   lv-rowid   = ROWID(est)
   li-row-num = BROWSE {&browse-name}:FOCUSED-ROW
   old-bl-qty = eb.bl-qty.

  /* === check record locked ==== */
  DO TRANSACTION:
    FIND CURRENT est EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    FIND CURRENT ef EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    FIND CURRENT eb EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF NOT AVAIL est OR NOT AVAIL ef OR NOT AVAIL eb THEN DO:
       MESSAGE "Estimate Record is being changed by someone else, wait a moment and try again..." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    end.
    FIND CURRENT est NO-LOCK NO-ERROR.
    FIND CURRENT ef NO-LOCK NO-ERROR.
    FIND CURRENT eb NO-LOCK NO-ERROR.
  END.

  /* To reposition after add */
  ASSIGN rEbRow = ?
         rEfRow = ?.
  IF AVAIL eb THEN
    rEbRow = ROWID(eb).
  IF AVAIL ef THEN
    rEfRow = ROWID(ef).

  RUN release-shared-buffers.
 
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
  
  /* Code placed here will execute AFTER standard behavior.    */
  RUN release-shared-buffers.

  /* move cursor to left run before open-query */
  DO li-cnt = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
    APPLY 'cursor-left' TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.
  END.  

  run dispatch('open-query').

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

  /* Repositions to 'current' row */
  IF rEfRow NE ? AND rEbRow NE ? THEN
    REPOSITION {&browse-name} TO ROWID rEfRow, rEbRow.

  IF ll-new-record THEN do:
      RUN get-link-handle IN adm-broker-hdl  (THIS-PROCEDURE,'Record-source':U,OUTPUT char-hdl).
      RUN New_Record IN WIDGET-HANDLE(char-hdl) (ROWID(eb)).
  END.

  /* If a row was added, move down one to be at the one added */
  IF lv-eb-recid <> ? THEN DO:
     DO WHILE RECID(eb) NE lv-eb-recid :
        RUN dispatch ('get-next') .
        LEAVE.
     END.
     run dispatch ('row-changed') NO-ERROR.
  END.

  ASSIGN ll-is-add-from-tool = no  /* reset */
         adm-new-record = NO
         adm-adding-record = NO
         ll-new-record = NO
         ll-is-copy-record = NO
         ll-part-no = NO
         ls-add-what = ""
         lv-copy-what = ""
         lv-eb-recid = ?.

      /* disable/enable FARM tab */
  run get-link-handle in adm-broker-hdl(this-procedure, "container-source", output char-hdl).
  RUN disable-enable-farm IN WIDGET-HANDLE(char-hdl) (eb.pur-man) NO-ERROR.
  /*run get-link-handle in adm-broker-hdl(this-procedure, "container-source", output char-hdl).
  run get-link-handle in adm-broker-hdl(widget-handle(char-hdl), "page-source", output char-hdl).     
  IF eb.pur-man THEN RUN enable-folder-page IN widget-handle(char-hdl) (INPUT 11) NO-ERROR.
  ELSE RUN disable-folder-page IN widget-handle(char-hdl) (INPUT 11) NO-ERROR.
  */

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
  RUN dispatch ('apply-entry').
  RUN setFarmTab.
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
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR li AS INT NO-UNDO.


  lv-rowid = ROWID(eb).

  RUN windows/l-esteb.w (cocode, locode, est.est-no, INPUT-OUTPUT lv-rowid).

  IF ROWID(eb) NE lv-rowid THEN DO:
    RUN dispatch ('get-first':U).
    DO WHILE ROWID(eb) NE lv-rowid:
      li = li + 1.
      IF li GT 200 THEN LEAVE.
      RUN dispatch ('get-next':U).
    END.
  END.

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
  DEF VAR lv-delete AS CHAR NO-UNDO.

  DEF BUFFER b-eb FOR eb.


  RUN check-delete NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  FOR EACH tt-eb:
    DELETE tt-eb.
  END.
  ll-mass-del = NO.

  IF AVAIL eb THEN RUN est/d-masdel.w (OUTPUT lv-delete).

  IF lv-delete NE "" THEN DO:
    ll-mass-del = YES.

    FOR EACH b-eb
        WHERE b-eb.company EQ eb.company
          AND b-eb.est-no  EQ eb.est-no
          AND (lv-delete   EQ "est"                                  OR
               (lv-delete  EQ "form" AND b-eb.form-no EQ eb.form-no) OR
               (lv-delete  EQ "blank" AND ROWID(b-eb) EQ ROWID(eb)))
        NO-LOCK:
      CREATE tt-eb.
      BUFFER-COPY b-eb TO tt-eb
      ASSIGN
       tt-eb.row-id = ROWID(b-eb).
    END.

    FOR EACH tt-eb:
      RUN repo-query (tt-eb.row-id).
      IF AVAIL eb THEN RUN dispatch ("delete-record").
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
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEFINE VARIABLE browser-handle AS HANDLE  NO-UNDO.
  DEFINE VARIABLE q-stat         AS LOGICAL NO-UNDO.
  DEFINE BUFFER bf-ef FOR ef.
  DEFINE BUFFER bf-eb FOR eb.
  DEFINE VARIABLE INextFORM AS INTEGER     NO-UNDO.

  hld-rowid = ROWID(ef).
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"add-est-source", output char-hdl).
  IF ip-nav-type NE "" THEN
  CASE ip-nav-type:
    WHEN "F" THEN RUN dispatch ('get-first':U).
    WHEN "L" THEN RUN dispatch ('get-last':U).
    WHEN "N" THEN do:                         
      INextFORM = eb.form-no + 1.
      IF NOT adm-query-opened THEN RETURN.


      FIND FIRST bf-eb 
        WHERE bf-eb.company EQ eb.company
          AND bf-eb.est-no  EQ eb.est-no
          AND bf-eb.eqty    EQ eb.eqty
          AND bf-eb.form-no EQ iNextForm
        NO-LOCK NO-ERROR.
      IF AVAIL bf-eb THEN
        FIND FIRST bf-ef 
          WHERE bf-ef.company EQ bf-eb.company
            AND bf-ef.est-no  EQ bf-eb.est-no
            AND bf-ef.eqty    EQ bf-eb.eqty
            AND bf-ef.form-no EQ bf-eb.form-no
          NO-LOCK NO-ERROR.
     
      IF AVAIL bf-eb AND AVAIL bf-ef THEN DO WITH FRAME Corr:
      
        
        IF num-results("br-estitm":U) = 0 THEN  /* Browse is empty */
          RUN new-state ('no-record-available,SELF':U).
        ELSE DO:          
          q-stat = QUERY br-estitm:REPOSITION-TO-ROWID(ROWID(bf-ef), ROWID(bf-eb)).            
          RUN dispatch IN THIS-PROCEDURE ('row-changed':U).
        END.
        
      END. /* if avail bf-eb ... */
      
    END. /* When N */
    WHEN "P" THEN DO: 
                    
      INextFORM = eb.form-no - 1.
        IF NOT adm-query-opened THEN RETURN.
  
  
        FIND FIRST bf-eb 
          WHERE bf-eb.company EQ eb.company
            AND bf-eb.est-no  EQ eb.est-no
            AND bf-eb.eqty    EQ eb.eqty
            AND bf-eb.form-no EQ iNextForm
          NO-LOCK NO-ERROR.
        IF AVAIL bf-eb THEN
          FIND FIRST bf-ef 
            WHERE bf-ef.company EQ bf-eb.company
              AND bf-ef.est-no  EQ bf-eb.est-no
              AND bf-ef.eqty    EQ bf-eb.eqty
              AND bf-ef.form-no EQ bf-eb.form-no
            NO-LOCK NO-ERROR.
  
        IF AVAIL bf-eb AND AVAIL bf-ef THEN DO WITH FRAME Corr:                    

          IF num-results("br-estitm":U) = 0 THEN  /* Browse is empty */
            RUN new-state ('no-record-available,SELF':U).
          ELSE DO:            
            q-stat = QUERY br-estitm:REPOSITION-TO-ROWID(ROWID(bf-ef), ROWID(bf-eb)).            
            RUN dispatch IN THIS-PROCEDURE ('row-changed':U).
          END.

        END. /* Avail avail bf-eb */                   
    END. /* When P */
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
      ef.cal:SCREEN-VALUE IN BROWSE {&browse-name} = string(item.cal).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-cust-no B-table-Win 
PROCEDURE new-cust-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
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

  IF op-est-type EQ 1                      AND
     (li-form-no NE 1 OR li-blank-no NE 1) THEN DO:
    ASSIGN
     op-est-type = 4
     eb.yld-qty  = eb.bl-qty.

    FOR EACH bf-eb OF bf-est WHERE ROWID(eb) NE ROWID(bf-eb):
      RUN set-yld-qty (ROWID(bf-eb)).
    END.
  END.

  IF (op-est-type EQ 2 AND li-form-no EQ 1 AND li-blank-no EQ 1) OR
     (op-est-type EQ 1 AND eb.cust-% GE 2) THEN DO:
    /*MESSAGE "Is this estimate a two piece box set?"
            VIEW-AS ALERT-BOX QUESTION
            BUTTON YES-NO UPDATE ll.
    IF ll THEN DO:
      RUN update-set.*/
      IF op-est-type EQ 1 THEN DO:
        op-est-type = 2.
        FOR EACH bf-eb
            WHERE bf-eb.company  EQ bf-est.company
              AND bf-eb.est-no   EQ bf-est.est-no
              AND bf-eb.blank-no GT 0:
          bf-eb.bl-qty = bf-est.est-qty[1].
        END.
      END.

      ELSE
      IF NOT adm-new-record AND (eb.cust-% GE 0 AND eb.cust-% LE 1) THEN DO:
        ll = ll-mass-del.
        IF NOT ll THEN
          MESSAGE "Change back to a single item estimate? "
              VIEW-AS ALERT-BOX QUESTION
              BUTTON YES-NO UPDATE ll.
        IF ll THEN op-est-type = 1.
      END.
    /*END.*/
  END.
  
  ELSE
  IF op-est-type EQ 4 THEN
  FOR EACH bf-eb
      WHERE bf-eb.company EQ bf-est.company
        AND bf-eb.est-no  EQ bf-est.est-no
        AND bf-eb.form-no NE 0:

    IF CAN-FIND(FIRST bf-set
                WHERE bf-set.company EQ bf-eb.company
                  AND bf-set.est-no  EQ bf-eb.est-no
                  AND bf-set.form-no EQ 0) THEN
      ASSIGN
       bf-eb.bl-qty  = bf-eb.bl-qty * bf-eb.cust-%
       bf-eb.yld-qty = bf-eb.bl-qty.

    ELSE
    IF bf-eb.yld-qty EQ 0 THEN bf-eb.yld-qty = bf-eb.bl-qty.
  END.

  bf-est.est-type = op-est-type.
  FOR EACH bf-ef
      WHERE bf-ef.company EQ bf-est.company 
        AND bf-ef.est-no  EQ bf-est.est-no:        
    FOR EACH bf-eb OF bf-ef:                    
      bf-eb.est-type = op-est-type.
    END.
    bf-ef.est-type = op-est-type.
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
  DEF VAR op-changed AS LOG NO-UNDO.
  DEF VAR lv-changed-to-page-six AS LOG NO-UNDO.

  DEF BUFFER b-eb FOR eb.

  IF AVAIL est THEN DO:
    IF est.est-type EQ 1 THEN DO:
      MESSAGE "Utilize Request/Yield Qty Pricing?" SKIP
              "(Single to Tandem/Combo Estimate Type)"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll.
      IF ll THEN DO TRANSACTION:
        FIND CURRENT est NO-ERROR.
        IF AVAIL est THEN DO:
          est.est-type = 4.
          RUN reset-est-type (OUTPUT li-est-type).
        END.
        FIND CURRENT est NO-LOCK NO-ERROR.
      END.
    END.

    IF est.est-type GT 1 THEN DO:
      lv-rowid = ROWID(eb).

      /*RUN ce/com/istandem.p (lv-rowid, OUTPUT ll).

      IF ll THEN*/
        RUN est/d-multbl.w (INPUT-OUTPUT lv-rowid, OUTPUT op-changed).
      /*ELSE
        RUN est/d-multib.w (INPUT-OUTPUT lv-rowid).*/

      IF est.est-type EQ 4 THEN
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
            IF AVAIL est THEN DO:
              est.est-type = 1.
              RUN reset-est-type (OUTPUT li-est-type).
            END.
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
  FIND FIRST b-ef OF b-eb EXCLUSIVE NO-ERROR.

  IF AVAIL b-ef THEN DO:
    IF b-eb.blank-no EQ 1 THEN
      ASSIGN
       b-eb.yld-qty = b-eb.bl-qty
       v-qty        = b-eb.yld-qty / b-eb.num-up.

    ELSE
    FOR EACH b-eb1 OF b-ef
        WHERE ROWID(b-eb1) NE ROWID(b-eb)
        BY b-eb1.yld-qty / b-eb1.num-up DESC:
      v-qty = b-eb1.yld-qty / b-eb1.num-up.
      LEAVE.
    END.

    {sys/inc/roundup.i v-qty}

    ASSIGN
     b-eb.die-in  = b-eb.die-in / b-eb.num-up
     b-eb.num-up  = TRUNC(b-eb.bl-qty / v-qty,0) +
                    INT(b-eb.bl-qty MODULO v-qty GT 0)
     b-eb.die-in  = b-eb.die-in * b-eb.num-up
     b-eb.yld-qty = v-qty * b-eb.num-up
     b-ef.die-in  = 0.

    FOR EACH b-eb1 OF b-ef NO-LOCK:
      b-ef.die-in = b-ef.die-in + b-eb1.die-in.
    END.

    IF b-eb.num-wid * b-eb.num-len NE b-eb.num-up THEN
      ASSIGN
       b-eb.num-wid = b-eb.num-up
       b-eb.num-len = 1.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setFarmTab B-table-Win 
PROCEDURE setFarmTab :
/*------------------------------------------------------------------------------
  Purpose:     
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

   IF est.est-type > 1 THEN DO: /* set est - copy for all forms*/
       FOR EACH e-itemfg-vend
                   WHERE e-itemfg-vend.company EQ eb.company
                     AND e-itemfg-vend.est-no = eb.est-no
                     AND e-itemfg-vend.eqty = eb.eqty.
           FIND FIRST reftable WHERE reftable.reftable EQ "e-itemfg-vend.std-uom" 
              AND reftable.company  EQ e-itemfg-vend.company   
              AND reftable.loc      EQ ""                      
              AND reftable.code     EQ e-itemfg-vend.est-no    
              AND reftable.val[1]   EQ e-itemfg-vend.form-no   
              AND reftable.val[2]   EQ e-itemfg-vend.blank-no
              NO-ERROR.

           IF AVAIL reftable THEN DELETE reftable.
           DELETE e-itemfg-vend.
       END.

      FOR EACH e-itemfg-vend
                   WHERE e-itemfg-vend.company EQ eb.company
                     AND e-itemfg-vend.est-no = eb.est-no
                     AND e-itemfg-vend.eqty = viEQtyPrev
                    /* AND e-itemfg-vend.form-no = eb.form-no
                     AND e-itemfg-vend.blank-no = eb.blank-no
                     AND e-itemfg-vend.i-no    EQ eb.stock-no*/
                     /*AND e-itemfg-vend.vend-no EQ ""*/  :

       CREATE bf-e-itemfg-vend.
       BUFFER-COPY e-itemfg-vend TO bf-e-itemfg-vend.
       ASSIGN bf-e-itemfg-vend.eqty = eb.eqty.


       FIND FIRST reftable WHERE reftable.reftable EQ "e-itemfg-vend.std-uom" 
              AND reftable.company  EQ e-itemfg-vend.company   
              AND reftable.loc      EQ ""                      
              AND reftable.code     EQ e-itemfg-vend.est-no    
              AND reftable.val[1]   EQ e-itemfg-vend.form-no   
              AND reftable.val[2]   EQ e-itemfg-vend.blank-no
              NO-ERROR.

       IF AVAIL reftable THEN vcUOM = reftable.code2.

       DELETE e-itemfg-vend.

       FIND FIRST reftable WHERE reftable.reftable EQ "e-itemfg-vend.std-uom" 
              AND reftable.company  EQ bf-e-itemfg-vend.company   
              AND reftable.loc      EQ ""                      
              AND reftable.code     EQ bf-e-itemfg-vend.est-no    
              AND reftable.val[1]   EQ bf-e-itemfg-vend.form-no   
              AND reftable.val[2]   EQ bf-e-itemfg-vend.blank-no
              NO-ERROR.
              
       IF NOT AVAIL reftable THEN DO:
          CREATE reftable.
          ASSIGN
               reftable.reftable = "e-itemfg-vend.std-uom"
               reftable.company  = bf-e-itemfg-vend.company
               reftable.loc      = ""
               reftable.code     = bf-e-itemfg-vend.est-no
               reftable.val[1]   = bf-e-itemfg-vend.form-no
               reftable.val[2]   = bf-e-itemfg-vend.blank-no.
       END.

       /*FIND FIRST bf-e-itemfg WHERE
                 bf-e-itemfg.company EQ bf-e-itemfg-vend.company AND
                 bf-e-itemfg.i-no EQ bf-e-itemfg-vend.i-no
                 NO-LOCK NO-ERROR.

       IF AVAIL bf-e-itemfg THEN reftable.code2 = bf-e-itemfg.std-uom.*/
       reftable.code2 = vcUOM.

      END.
   END.   /* end of set est */
   ELSE FOR EACH e-itemfg-vend
                   WHERE e-itemfg-vend.company EQ eb.company
                     AND e-itemfg-vend.est-no = eb.est-no
                     AND e-itemfg-vend.eqty = viEQtyPrev
                     AND e-itemfg-vend.form-no = eb.form-no
                     AND e-itemfg-vend.blank-no = eb.blank-no
                     AND e-itemfg-vend.i-no    EQ eb.stock-no
                     /*AND e-itemfg-vend.vend-no EQ ""*/  :

       CREATE bf-e-itemfg-vend.
       BUFFER-COPY e-itemfg-vend TO bf-e-itemfg-vend.
       ASSIGN bf-e-itemfg-vend.eqty = eb.eqty.


       FIND FIRST reftable WHERE reftable.reftable EQ "e-itemfg-vend.std-uom" 
              AND reftable.company  EQ e-itemfg-vend.company   
              AND reftable.loc      EQ ""                      
              AND reftable.code     EQ e-itemfg-vend.est-no    
              AND reftable.val[1]   EQ e-itemfg-vend.form-no   
              AND reftable.val[2]   EQ e-itemfg-vend.blank-no
              NO-ERROR.

       IF AVAIL reftable THEN vcUOM = reftable.code2.

       DELETE e-itemfg-vend.

       FIND FIRST reftable WHERE reftable.reftable EQ "e-itemfg-vend.std-uom" 
              AND reftable.company  EQ bf-e-itemfg-vend.company   
              AND reftable.loc      EQ ""                      
              AND reftable.code     EQ bf-e-itemfg-vend.est-no    
              AND reftable.val[1]   EQ bf-e-itemfg-vend.form-no   
              AND reftable.val[2]   EQ bf-e-itemfg-vend.blank-no
              NO-ERROR.
              
       IF NOT AVAIL reftable THEN DO:
          CREATE reftable.
          ASSIGN
               reftable.reftable = "e-itemfg-vend.std-uom"
               reftable.company  = bf-e-itemfg-vend.company
               reftable.loc      = ""
               reftable.code     = bf-e-itemfg-vend.est-no
               reftable.val[1]   = bf-e-itemfg-vend.form-no
               reftable.val[2]   = bf-e-itemfg-vend.blank-no.
       END.

       /*FIND FIRST bf-e-itemfg WHERE
                 bf-e-itemfg.company EQ bf-e-itemfg-vend.company AND
                 bf-e-itemfg.i-no EQ bf-e-itemfg-vend.i-no
                 NO-LOCK NO-ERROR.

       IF AVAIL bf-e-itemfg THEN reftable.code2 = bf-e-itemfg.std-uom.*/
       reftable.code2 = vcUOM.

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


  FIND b-eb WHERE ROWID(b-eb) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL eb THEN DO TRANSACTION:
    FIND FIRST b-est
        WHERE b-est.company EQ b-eb.company
          AND b-est.est-no  EQ b-eb.est-no
        NO-ERROR.

    IF AVAIL b-est THEN
    FOR EACH b-ef
        WHERE b-ef.company EQ b-est.company
          AND b-ef.est-no  EQ b-est.est-no
        BREAK BY b-ef.form-no:

      IF FIRST(b-ef.form-no) THEN b-est.form-qty = 0.

      b-est.form-qty = b-est.form-qty + 1.

      FOR EACH b-eb
          WHERE b-eb.company EQ b-ef.company
            AND b-eb.est-no  EQ b-ef.est-no
            AND b-eb.form-no EQ b-ef.form-no
          NO-LOCK
          BREAK BY b-eb.blank-no:

        IF FIRST(b-eb.blank-no) THEN b-ef.blank-qty = 0.

        b-ef.blank-qty = b-ef.blank-qty + 1.
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

  DEF VAR i AS INT NO-UNDO.
  DEF VAR lv-type LIKE est.est-type NO-UNDO.
  DEF VAR lv-old-type LIKE lv-type NO-UNDO.


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
  IF AVAIL bf-ord AND i LE 1 AND eb.cust-% LE 1 THEN DO:
    MESSAGE "Sorry, Order must be closed before changing to a set..." 
        VIEW-AS ALERT-BOX ERROR.
    RETURN.
  END.

  lv-old-type = est.est-type.

  IF lv-type NE lv-old-type THEN DO:
    FIND CURRENT est.
    est.est-type = lv-type.
    RUN reset-est-type (OUTPUT lv-type).
    FIND CURRENT est NO-LOCK.
  END.

  IF est.est-type EQ 2 THEN DO:

      FIND xest WHERE RECID(xest) = RECID(est) NO-LOCK.
      FIND xef WHERE RECID(xef) = RECID(ef) NO-LOCK.
      FIND xeb WHERE RECID(xeb) = RECID(eb) NO-LOCK.

    RUN cec/d-updset.w (RECID(eb),2).

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
                            INPUT 'EF',
                            INPUT YES,
                            OUTPUT lActive).
 {sys/inc/chblankcust.i ""EF""}
  
  IF ou-log THEN
    DO WITH FRAME {&FRAME-NAME}:
      IF LOOKUP(eb.cust-no:SCREEN-VALUE IN BROWSE {&browse-name},custcount) = 0 THEN do:
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
      ls-key = string(today,"99999999") +
               string(next-value(rec_key_seq,nosweat),"99999999").
      FIND CURRENT eb.
      eb.rec_key = ls-key.
      FIND CURRENT eb NO-LOCK.               
      create rec_key.
      assign rec_key.rec_key = eb.rec_key
             rec_key.table_name = "eb".

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
       NOT ll-crt-itemfg                                       AND
       NOT CAN-FIND(FIRST itemfg
                    WHERE itemfg.company EQ gcompany
                      AND itemfg.i-no    EQ eb.stock-no:SCREEN-VALUE IN BROWSE {&browse-name})
    THEN DO:
      MESSAGE "This item does not exist, would you like to add it?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE ll-ans as log.  
      IF ll-ans then ll-crt-itemfg = YES.
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
                      AND style.industry EQ "1") OR
       eb.style:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO eb.style IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
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
  DEF VAR lv-handle AS HANDLE NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    lv-handle = IF LOOKUP(FOCUS:NAME,"style,len") GT 0 THEN FOCUS ELSE ?.
    IF NOT VALID-HANDLE(lv-handle) THEN lv-handle = eb.wid:HANDLE IN BROWSE {&BROWSE-NAME}.

    IF ll-warn AND ll-wid-len-warned EQ NO                  AND
       CAN-FIND(FIRST style
                WHERE style.company  EQ cocode
                  AND style.style    EQ eb.style:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                  AND style.industry EQ "1"
                  AND INDEX("DF",style.type) LE 0)          AND
       (eb.style:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}    NE eb.style OR
        DEC(eb.wid:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE eb.wid   OR
        DEC(eb.len:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE eb.len)                 AND
       DEC(eb.wid:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) GT DEC(eb.len:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) THEN DO:
      MESSAGE "This is an abnormal box, carton width should not be"
              "greater than length." SKIP
              "Would you like to continue with abnormal box?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll-wid-len-warned.
      IF NOT ll-wid-len-warned THEN DO:
        APPLY "entry" TO lv-handle.
        RETURN ERROR.
      END.
    END.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-cw-dim B-table-Win 
FUNCTION display-cw-dim RETURNS DECIMAL
  ( input ip-is-corr-style as log, input  ip-dim as decimal ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var out-dim as dec no-undo.
  
  if ip-is-corr-style and ip-dim <> 0 then 
     /*round(trunc({1},0) + (({1} - trunc({1},0)) / K_FRAC),2)   sys/inc/k16.i */
     out-dim = round(trunc(ip-dim,0) + ((ip-dim - trunc(ip-dim,0)) / K_FRAC),2).
  else out-dim = ip-dim.
/*  
  message "function is corr-style? bstitm " ip-is-corr-style 
            " Input: " ip-dim "Return value:" out-dim view-as alert-box.
*/
  RETURN out-dim.   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

