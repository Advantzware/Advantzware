&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: oe\b-ordrel.w

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
{custom/globdefs.i}
{sys/inc/var.i new shared }
assign cocode = g_company
       locode = g_loc.

DEF BUFFER s-code FOR reftable.
DEF BUFFER ref-lot-no FOR reftable.
DEF BUFFER ref-sell-price FOR reftable.

DEF VAR char-hdl AS CHAR NO-UNDO.
def var ls-rel-stat as cha label "" form "x" no-undo.
def var lv-rel-recid as recid no-undo.
def new shared buffer xoe-ordl for oe-ordl.
def new shared buffer xoe-ord for oe-ord.
def new shared var out-recid as recid no-undo.
def new shared var relh-recid as recid no-undo.
def new shared var v-auto as log no-undo.
def new shared var nufile as log no-undo.   /* for jc-calc.p */
def new shared var lv-qty as int no-undo.
def new shared var fil_id as recid no-undo.
def var li-ship-no as int no-undo.  /* if ship-to is changed */

def var ll-unposted as log no-undo.
def var ls-po as cha no-undo.
def var ll-canceled as log no-undo.
def var lv-stat as cha no-undo.
DEF VAR ld-date as DATE NO-UNDO.
DEF VAR ll-skip AS LOG NO-UNDO.
DEF VAR lv-s-codes AS CHAR NO-UNDO.
DEF VAR lv-s-dscrs AS CHAR NO-UNDO.
DEF VAR lv-cust-x LIKE cust.cust-no NO-UNDO.
DEF VAR ll-transfer AS LOG NO-UNDO.
DEF VAR v-browse-in-update AS LOG NO-UNDO.
DEF VAR v-cust-no AS CHAR NO-UNDO.

RUN sys/ref/s-codes.p (OUTPUT lv-s-codes, OUTPUT lv-s-dscrs).

DEF TEMP-TABLE tt-report LIKE report FIELD phantom AS LOG
                                     FIELD po-no LIKE oe-rel.po-no
                                     FIELD qty LIKE oe-rel.qty
                                     FIELD printed AS LOG
                                     FIELD s-code AS CHAR
                                     FIELD lot-no AS CHAR
                                     FIELD sell-price AS DEC.

{oe/chkordl.i NEW}

DO TRANSACTION:
  {sys/inc/oeship.i}
  {sys/inc/oereleas.i}
  {sys/ref/relpost.i}
  {sys/inc/addxfer.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES oe-ordl
&Scoped-define FIRST-EXTERNAL-TABLE oe-ordl


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-ordl.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES oe-rel tt-report

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table tt-report.printed tt-report.s-code ~
oe-rel.ship-id get-rel-stat() @ ls-rel-stat oe-rel.carrier oe-rel.tot-qty ~
get-rel-qty() @ oe-rel.qty oe-rel.qty get-rel-qty() @ oe-rel.qty ~
tt-report.po-no tt-report.lot-no tt-report.key-02 oe-rel.ship-addr[1] ~
oe-rel.ship-city oe-rel.ship-state tt-report.sell-price oe-ordl.disc ~
oe-ordl.t-price 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table tt-report.printed ~
tt-report.s-code oe-rel.ship-id oe-rel.carrier oe-rel.tot-qty ~
tt-report.po-no tt-report.lot-no tt-report.key-02 oe-rel.ship-addr[1] ~
tt-report.sell-price 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table tt-report oe-rel
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table tt-report
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-br_table oe-rel
&Scoped-define QUERY-STRING-br_table FOR EACH oe-rel WHERE ~{&KEY-PHRASE}                    AND ~
oe-rel.company EQ oe-ordl.company AND ~
oe-rel.ord-no  EQ oe-ordl.ord-no AND ~
oe-rel.i-no    EQ oe-ordl.i-no   AND ~
oe-rel.line    EQ oe-ordl.line NO-LOCK, ~
      EACH tt-report WHERE tt-report.rec-id eq recid(oe-rel) NO-LOCK ~
    BY tt-report.key-01 ~
       BY oe-rel.po-no ~
        BY oe-rel.ship-no ~
         BY oe-rel.qty
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH oe-rel WHERE ~{&KEY-PHRASE}                    AND ~
oe-rel.company EQ oe-ordl.company AND ~
oe-rel.ord-no  EQ oe-ordl.ord-no AND ~
oe-rel.i-no    EQ oe-ordl.i-no   AND ~
oe-rel.line    EQ oe-ordl.line NO-LOCK, ~
      EACH tt-report WHERE tt-report.rec-id eq recid(oe-rel) NO-LOCK ~
    BY tt-report.key-01 ~
       BY oe-rel.po-no ~
        BY oe-rel.ship-no ~
         BY oe-rel.qty.
&Scoped-define TABLES-IN-QUERY-br_table oe-rel tt-report
&Scoped-define FIRST-TABLE-IN-QUERY-br_table oe-rel
&Scoped-define SECOND-TABLE-IN-QUERY-br_table tt-report


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
company||y|ASI.oe-rel.company
Carrier||y|ASI.oe-rel.Carrier
r-no||y|ASI.oe-rel.r-no
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "company,Carrier,r-no"':U).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-rel-qty B-table-Win 
FUNCTION get-rel-qty RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-rel-stat B-table-Win 
FUNCTION get-rel-stat RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-tot-qty B-table-Win 
FUNCTION get-tot-qty RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalendar 
     IMAGE-UP FILE "schedule/images/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY .86 TOOLTIP "PopUp Calendar".

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      oe-rel, 
      tt-report SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      tt-report.printed COLUMN-LABEL "Prt" FORMAT "Y/N":U COLUMN-FONT 0
      tt-report.s-code COLUMN-LABEL "S/I" FORMAT "!":U WIDTH 4
            COLUMN-FONT 0
      oe-rel.ship-id COLUMN-LABEL "Ship To" FORMAT "x(8)":U COLUMN-FONT 0
      get-rel-stat() @ ls-rel-stat COLUMN-LABEL "S" WIDTH 2
      oe-rel.carrier COLUMN-LABEL "Via" FORMAT "x(5)":U COLUMN-FONT 0
      oe-rel.tot-qty COLUMN-LABEL "Sched Qty" FORMAT "->>,>>>,>>9":U
      get-rel-qty() @ oe-rel.qty
      oe-rel.qty COLUMN-LABEL "Actual Qty" FORMAT "->>,>>>,>>9":U
            WIDTH 16 COLUMN-FONT 0
      get-rel-qty() @ oe-rel.qty
      tt-report.po-no COLUMN-LABEL "Customer PO#" COLUMN-FONT 0
      tt-report.lot-no COLUMN-LABEL "Customer Lot#" FORMAT "X(15)":U
            WIDTH 21.8 COLUMN-FONT 0
      tt-report.key-02 COLUMN-LABEL "Date" FORMAT "99/99/9999":U
            WIDTH 18.4
      oe-rel.ship-addr[1] COLUMN-LABEL "Ship To Address" FORMAT "x(26)":U
            COLUMN-FONT 0
      oe-rel.ship-city FORMAT "x(15)":U COLUMN-FONT 0
      oe-rel.ship-state FORMAT "x(2)":U COLUMN-FONT 0
      tt-report.sell-price COLUMN-LABEL "Sell Price" FORMAT "->>,>>>,>>9.99<<<<":U
            WIDTH 16 COLUMN-FONT 0
      oe-ordl.disc FORMAT "(>>>,>>9.99)":U
      oe-ordl.t-price COLUMN-LABEL "Ext Price" FORMAT "->>,>>>,>>9.99":U
  ENABLE
      tt-report.printed
      tt-report.s-code HELP "Enter (I)nvoice only, (S)hip only, (B)oth invoice & ship, or (T)ransfer"
      oe-rel.ship-id
      oe-rel.carrier
      oe-rel.tot-qty
      tt-report.po-no
      tt-report.lot-no
      tt-report.key-02
      oe-rel.ship-addr[1]
      tt-report.sell-price
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 147 BY 12.38
         FONT 0 ROW-HEIGHT-CHARS .62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnCalendar AT ROW 1.91 COL 125
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 0.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: ASI.oe-ordl
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
         HEIGHT             = 12.43
         WIDTH              = 147.
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
   NOT-VISIBLE Size-to-Fit Custom                                       */
/* BROWSE-TAB br_table btnCalendar F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnCalendar IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btnCalendar:HIDDEN IN FRAME F-Main           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "ASI.oe-rel WHERE ASI.oe-ordl ...,ASI.tt-report WHERE ASI.oe-rel ..."
     _Options          = "NO-LOCK KEY-PHRASE"
     _TblOptList       = ","
     _OrdList          = "ASI.tt-report.key-01|yes,ASI.oe-rel.po-no|yes,ASI.oe-rel.ship-no|yes,ASI.oe-rel.qty|yes"
     _JoinCode[1]      = "~{&KEY-PHRASE}                    AND
oe-rel.company EQ oe-ordl.company AND
oe-rel.ord-no  EQ oe-ordl.ord-no AND
oe-rel.i-no    EQ oe-ordl.i-no   AND
oe-rel.line    EQ oe-ordl.line"
     _JoinCode[2]      = "tt-report.rec-id eq recid(oe-rel)"
     _FldNameList[1]   > ASI.tt-report.printed
"tt-report.printed" "Prt" "Y/N" "logical" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > ASI.tt-report.s-code
"tt-report.s-code" "S/I" "!" "character" ? ? 0 ? ? ? yes "Enter (I)nvoice only, (S)hip only, (B)oth invoice & ship, or (T)ransfer" no no "4" yes no no "U" "" ""
     _FldNameList[3]   > ASI.oe-rel.ship-id
"oe-rel.ship-id" "Ship To" ? "character" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > "_<CALC>"
"get-rel-stat() @ ls-rel-stat" "S" ? ? ? ? ? ? ? ? no ? no no "2" yes no no "U" "" ""
     _FldNameList[5]   > ASI.oe-rel.carrier
"oe-rel.carrier" "Via" ? "character" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > ASI.oe-rel.tot-qty
"oe-rel.tot-qty" "Sched Qty" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > "_<CALC>"
"get-rel-qty() @ oe-rel.qty" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > ASI.oe-rel.qty
"oe-rel.qty" "Actual Qty" ? "decimal" ? ? 0 ? ? ? no ? no no "16" yes no no "U" "" ""
     _FldNameList[9]   > "_<CALC>"
"get-rel-qty() @ oe-rel.qty" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[10]   > ASI.tt-report.po-no
"tt-report.po-no" "Customer PO#" ? "character" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[11]   > ASI.tt-report.lot-no
"tt-report.lot-no" "Customer Lot#" "X(15)" "character" ? ? 0 ? ? ? yes ? no no "21.8" yes no no "U" "" ""
     _FldNameList[12]   > ASI.tt-report.key-02
"tt-report.key-02" "Date" "99/99/9999" "character" ? ? ? ? ? ? yes ? no no "18.4" yes no no "U" "" ""
     _FldNameList[13]   > ASI.oe-rel.ship-addr[1]
"oe-rel.ship-addr[1]" "Ship To Address" "x(26)" "character" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[14]   > ASI.oe-rel.ship-city
"oe-rel.ship-city" ? ? "character" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[15]   > ASI.oe-rel.ship-state
"oe-rel.ship-state" ? ? "character" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[16]   > ASI.tt-report.sell-price
"tt-report.sell-price" "Sell Price" "->>,>>>,>>9.99<<<<" "decimal" ? ? 0 ? ? ? yes ? no no "16" yes no no "U" "" ""
     _FldNameList[17]   = ASI.oe-ordl.disc
     _FldNameList[18]   > ASI.oe-ordl.t-price
"oe-ordl.t-price" "Ext Price" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
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
ON DEFAULT-ACTION OF br_table IN FRAME F-Main
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON HELP OF br_table IN FRAME F-Main
DO:
    DEF VAR char-val AS cha NO-UNDO.
    DEF VAR rec-val AS RECID NO-UNDO.
    DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.


    IF NOT AVAIL oe-rel AND lv-rel-recid NE ? THEN
    FIND oe-rel WHERE RECID(oe-rel) EQ lv-rel-recid NO-LOCK.

    lw-focus = FOCUS.

    CASE lw-focus:NAME:
         WHEN "ship-id" then do:
              FIND oe-ord NO-LOCK
                  WHERE oe-ord.company EQ oe-rel.company 
                    AND oe-ord.ord-no  EQ oe-rel.ord-no.
              IF tt-report.s-code:SCREEN-VALUE IN BROWSE {&browse-name} EQ "T" AND lv-cust-x NE "" THEN
                RUN windows/l-shipt3.w (g_company, g_loc, oe-ord.cust-no, lw-focus:SCREEN-VALUE, OUTPUT char-val, OUTPUT rec-val).
              ELSE
                RUN windows/l-shipt2.w (g_company, g_loc, oe-ord.cust-no, lw-focus:SCREEN-VALUE, OUTPUT char-val, OUTPUT rec-val).
              FIND shipto WHERE RECID(shipto) EQ rec-val NO-LOCK NO-ERROR. 
              IF AVAIL shipto AND lw-focus:SCREEN-VALUE NE shipto.ship-id THEN DO:
                 lw-focus:SCREEN-VALUE = shipto.ship-id.
                 RUN new-ship-id.
              END.
         END.
         WHEN "carrier" then do:
              RUN windows/l-carrie.w (g_company, g_loc, lw-focus:SCREEN-VALUE, OUTPUT char-val).
              IF char-val <> "" then lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
              RETURN NO-APPLY.
         END.
         WHEN "s-code" THEN DO:
              RUN windows/l-cddscr.w ("Release Types", lv-s-codes, lv-s-dscrs, lw-focus:SCREEN-VALUE, OUTPUT char-val).
              IF char-val NE "" THEN lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
         END.
    END CASE.

    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON return OF br_table IN FRAME F-Main
anywhere
DO:
   apply "tab" to self.
   return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
/*   {src/adm/template/brsleave.i} */
        {est/brsleave.i }
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON SCROLL-NOTIFY OF br_table IN FRAME F-Main
DO:
   RUN calendarPlacement.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  IF NOT v-browse-in-update THEN RUN set-buttons.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-report.printed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-report.printed br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF tt-report.printed IN BROWSE br_table /* Prt */
DO:
  IF INDEX("AB",get-rel-stat()) LE 0 THEN DO:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.

  RUN calendarPlacement.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-report.s-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-report.s-code br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF tt-report.s-code IN BROWSE br_table /* S/I */
DO:
  IF INDEX("AB",get-rel-stat()) GT 0 OR ll-transfer THEN DO:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.

  RUN calendarPlacement.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-report.s-code br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF tt-report.s-code IN BROWSE br_table /* S/I */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-s-code NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rel.ship-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rel.ship-id br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF oe-rel.ship-id IN BROWSE br_table /* Ship To */
DO:
  IF INDEX("AB",get-rel-stat()) GT 0 THEN DO:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rel.ship-id br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-rel.ship-id IN BROWSE br_table /* Ship To */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-ship-id NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.                         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rel.ship-id br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF oe-rel.ship-id IN BROWSE br_table /* Ship To */
DO:
  RUN new-ship-id.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rel.carrier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rel.carrier br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF oe-rel.carrier IN BROWSE br_table /* Via */
DO:
  IF INDEX("AB",get-rel-stat()) GT 0 THEN DO:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rel.carrier br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-rel.carrier IN BROWSE br_table /* Via */
DO:
    if lastkey = -1 then return.
    
    find first carrier where carrier.company = g_company and
                             carrier.carrier = self:screen-value
                             no-lock no-error.
    if not avail carrier then do:
       message "Invalid Carrier. Try Help. " view-as alert-box error.
       return no-apply.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rel.tot-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rel.tot-qty br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF oe-rel.tot-qty IN BROWSE br_table /* Sched Qty */
DO:
  IF INDEX("AB",get-rel-stat()) GT 0 THEN DO:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-report.po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-report.po-no br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF tt-report.po-no IN BROWSE br_table /* Customer PO# */
DO:
  IF INDEX("AB",get-rel-stat()) GT 0 THEN DO:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-report.po-no br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF tt-report.po-no IN BROWSE br_table /* Customer PO# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-po-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-report.lot-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-report.lot-no br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF tt-report.lot-no IN BROWSE br_table /* Customer Lot# */
DO:
  IF INDEX("AB",get-rel-stat()) GT 0 THEN DO:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-report.key-02
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-report.key-02 br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF tt-report.key-02 IN BROWSE br_table /* Date */
DO:
  ASSIGN
     btnCalendar:VISIBLE IN FRAME {&FRAME-NAME} = YES
     btnCalendar:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

  RUN calendarPlacement.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-report.key-02 br_table _BROWSE-COLUMN B-table-Win
ON HELP OF tt-report.key-02 IN BROWSE br_table /* Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-report.key-02 br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF tt-report.key-02 IN BROWSE br_table /* Date */
DO:
 IF NOT ll-skip THEN DO:

  btnCalendar:HIDDEN IN FRAME {&FRAME-NAME} = YES.

  IF INT(SUBSTR(tt-report.key-02:SCREEN-VALUE IN BROWSE {&browse-name},7,4)) LT 1 THEN
    tt-report.key-02:SCREEN-VALUE = SUBSTR(tt-report.key-02:SCREEN-VALUE,1,6) +
                                    STRING(YEAR(TODAY),"9999").

  ELSE
  IF INT(SUBSTR(tt-report.key-02:SCREEN-VALUE,7,4)) LT 90 THEN
    tt-report.key-02:SCREEN-VALUE = SUBSTR(tt-report.key-02:SCREEN-VALUE,1,6) +
            STRING(INT(SUBSTR(tt-report.key-02:SCREEN-VALUE,7,4)) + 2000,"9999").

  ELSE
  IF INT(SUBSTR(tt-report.key-02:SCREEN-VALUE,7,4)) LE 99 THEN
    tt-report.key-02:SCREEN-VALUE = SUBSTR(tt-report.key-02:SCREEN-VALUE,1,6) +
            STRING(INT(SUBSTR(tt-report.key-02:SCREEN-VALUE,7,4)) + 1900,"9999").

  IF LASTKEY NE -1 THEN DO:
    {custom/pastDatePrompt.i SELF:SCREEN-VALUE}

    RUN valid-key-02 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    IF INDEX("AB",lv-stat) GT 0 THEN DO: 
      IF KEYFUNCTION(LASTKEY) EQ "BACK-TAB" THEN RETURN NO-APPLY.
      ELSE RUN dispatch ("update-record").
    END.
    ELSE do:
        APPLY "entry" TO oe-rel.ship-addr[1].
        APPLY "tab" TO oe-rel.ship-addr[1].     
        RETURN NO-APPLY.
    END.
  END.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-report.sell-price
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-report.sell-price br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF tt-report.sell-price IN BROWSE br_table /* Sell Price */
DO:
  IF KEYFUNCTION(LASTKEY) EQ "BACK-TAB" THEN
  DO:
     APPLY "ENTRY" TO tt-report.key-02 IN BROWSE {&browse-name}.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar B-table-Win
ON CHOOSE OF btnCalendar IN FRAME F-Main
DO:
  DEFINE VARIABLE calendarDate AS CHARACTER NO-UNDO.

  RUN nosweat/popupcal.w (OUTPUT calendarDate).
  
  IF calendarDate NE '' THEN
     tt-report.key-02:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(DATE(calendarDate),"99/99/9999").

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
session:data-entry-return = yes.
{sys/inc/f3help.i}
{sys/inc/oeinq.i}
{sa/sa-sls01.i}
    
lv-cust-x = "".

FOR EACH cust NO-LOCK
    WHERE cust.company EQ cocode
      AND cust.active  EQ "X":
  lv-cust-x = cust.cust-no.
  LEAVE.
END.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

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
  {src/adm/template/row-list.i "oe-ordl"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-ordl"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-report-file B-table-Win 
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
                         WHERE oe-ord.company EQ oe-ordl.company
                           AND oe-ord.ord-no  EQ oe-ordl.ord-no
                           AND oe-ord.type    EQ "T").

  RUN delete-phantoms.

  FOR EACH tt-report:
    DELETE tt-report.
  END.

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
        FIND FIRST oe-rel NO-LOCK USE-INDEX seq-no NO-ERROR.
        v-nxt-r-no = IF AVAIL oe-rel THEN oe-rel.r-no + 1 ELSE 1.

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

  /*FOR EACH oe-rel NO-LOCK
      WHERE oe-rel.company EQ oe-ordl.company
        AND oe-rel.ord-no  EQ oe-ordl.ord-no
        AND oe-rel.i-no    EQ oe-ordl.i-no
        AND oe-rel.line    EQ oe-ordl.line
        AND oe-rel.link-no EQ 0
      USE-INDEX ord-item
      TRANSACTION:

    FIND FIRST s-code
        WHERE s-code.reftable EQ "oe-rel.s-code"
          AND s-code.company  EQ STRING(oe-rel.r-no,"9999999999")
        NO-LOCK NO-ERROR.
    lv-s-code[1] = IF AVAIL s-code THEN s-code.code ELSE "B".

    FOR EACH b-oe-rel
        WHERE b-oe-rel.company  EQ oe-rel.company
          AND b-oe-rel.ord-no   EQ oe-rel.ord-no
          AND b-oe-rel.i-no     EQ oe-rel.i-no
          AND b-oe-rel.line     EQ oe-rel.line
          AND b-oe-rel.po-no    EQ oe-rel.po-no
          AND b-oe-rel.ship-id  EQ oe-rel.ship-id
          AND b-oe-rel.rel-date EQ oe-rel.rel-date
          AND b-oe-rel.carrier  EQ oe-rel.carrier
          AND b-oe-rel.qty      EQ oe-rel.qty
          AND b-oe-rel.link-no  EQ 0
          AND ROWID(b-oe-rel)   NE ROWID(oe-rel)
        USE-INDEX ord-item:

      FIND FIRST s-code
          WHERE s-code.reftable EQ "oe-rel.s-code"
            AND s-code.company  EQ STRING(b-oe-rel.r-no,"9999999999")
          NO-LOCK NO-ERROR.
      lv-s-code[2] = IF AVAIL s-code THEN s-code.code ELSE "B".

      IF lv-s-code[1] EQ lv-s-code[2] THEN DELETE b-oe-rel.
    END.
  END.*/

  FOR EACH oe-rel
      WHERE oe-rel.company EQ oe-ordl.company
        AND oe-rel.ord-no  EQ oe-ordl.ord-no
        AND oe-rel.i-no    EQ oe-ordl.i-no
        AND oe-rel.line    EQ oe-ordl.line
      USE-INDEX ord-item:
    RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).
    IF INDEX("SIL",lv-stat) GT 0 OR 
       (INDEX("CZ",lv-stat) LE 0 AND oe-rel.qty EQ 0) THEN
      oe-rel.qty = oe-rel.tot-qty.
  END.

  RELEASE oe-rel.
  RELEASE b-oe-rell.
  RELEASE oe-rell.
  RELEASE oe-boll.
  RELEASE tt-report.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calendarPlacement B-table-Win 
PROCEDURE calendarPlacement :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
   
      IF btnCalendar:VISIBLE AND
         tt-report.key-02:X IN BROWSE {&browse-name} + 73 + BROWSE {&browse-name}:X GT 0 AND
         tt-report.key-02:Y IN BROWSE {&browse-name} + BROWSE {&browse-name}:Y GT 0 AND
         tt-report.key-02:X IN BROWSE {&browse-name} + 73 + BROWSE {&browse-name}:X LE BROWSE {&browse-name}:WIDTH-PIXELS AND
         tt-report.key-02:Y IN BROWSE {&browse-name} + BROWSE {&browse-name}:Y LE BROWSE {&browse-name}:HEIGHT-PIXELS THEN
         DO: /*do end needs to be here*/
            ASSIGN
               btnCalendar:X  = tt-report.key-02:X IN BROWSE {&browse-name} + 73 + BROWSE {&browse-name}:X.
               btnCalendar:Y  = tt-report.key-02:Y IN BROWSE {&browse-name} + BROWSE {&browse-name}:Y.
         END.
   END.

   END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-release B-table-Win 
PROCEDURE check-release :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lv-msg AS CHAR NO-UNDO.


IF lv-msg EQ "" AND xoe-ord.stat eq "H" THEN
  lv-msg = "customers on Credit Hold".

IF lv-msg EQ "" AND xoe-ord.stat EQ "W" THEN
  lv-msg = "unapproved web orders".

IF lv-msg EQ "" AND NOT xoe-ord.opened THEN
  lv-msg = "closed orders".

IF lv-msg EQ "" AND TRIM(oe-ordl.job-no) NE ""
                AND CAN-FIND(FIRST job
                             WHERE job.company EQ oe-ordl.company
                               AND job.job-no  EQ oe-ordl.job-no
                               AND job.job-no2 EQ oe-ordl.job-no2
                               AND job.stat    EQ "H") THEN
  lv-msg = "jobs on hold".

IF lv-msg NE "" THEN DO:
  MESSAGE "Can't release items for " +
          TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
  RETURN ERROR.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-bol B-table-Win 
PROCEDURE create-bol :
/*------------------------------------------------------------------------------
  Purpose:     from oe/oe-ordlr.i
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def var choice as log no-undo.

def var v-all-items as log no-undo.
def var v-first as log no-undo.
DEF VAR lv-save-recid AS RECID NO-UNDO.


find xoe-ord where xoe-ord.company = g_company and
                   xoe-ord.ord-no = oe-rel.ord-no no-lock.
find first oe-ctrl where oe-ctrl.company = xoe-ord.company no-lock .

{sys/inc/addrelse.i}
{sys/inc/oereordr.i}

choice = NO.

RUN check-release NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.

lv-save-recid = RECID(oe-rel).

/* ======== in sys/inc/addrelse.i 
find sys-ctrl where sys-ctrl.company eq cocode
                and sys-ctrl.name    eq "ADDRELSE"  no-lock no-error.
if not avail sys-ctrl then do:
   create sys-ctrl.
   assign sys-ctrl.company = cocode
          sys-ctrl.name = "ADDRELSE"
          sys-ctrl.log-fld = yes.
   v-do-bol = sys-ctrl.log-fld.             
end.    
else v-do-bol = avail sys-ctrl and sys-ctrl.log-fld and oe-ctrl.ship-from.
========= */       


SESSION:SET-WAIT-STATE("general").

    v-first = YES.  
    FIND FIRST xoe-ordl OF oe-rel NO-LOCK NO-ERROR.

    {oe/rel-stat.i lv-stat x}.  

    IF INDEX("CPZ",lv-stat) GT 0 THEN DO:
      MESSAGE ENTRY(INDEX("ABCPZ",lv-stat),"Actual,Backorder,Completed,Posted,Invoice")
              " release entries can not be released." VIEW-AS ALERT-BOX ERROR.
      RETURN.
    END.
    
    if v-first then do on endkey undo, retry:
         assign  v-first = no
                 choice  = yes.
        message "Create " +
                TRIM(STRING(AVAIL tt-report AND tt-report.s-code EQ "I","Invoice/BOL")) +
                " for Release Date-" + trim(string(oe-rel.rel-date)) +
                " and ShipID-" +  trim(oe-rel.ship-id) + " ?"
               view-as alert-box question button yes-no update choice.
    end.
    if choice then do:
        out-recid = recid(oe-rel).
       /* run oe/relbol.p (recid(xoe-ordl)).  */
            IF NOT AVAIL oe-rell THEN DO:

              v-cust-no = oe-rel.cust-no.

              IF addxfer-log THEN
              DO:
                 FIND FIRST s-code WHERE
                      s-code.reftable EQ "oe-rel.s-code" AND
                      s-code.company  EQ STRING(oe-rel.r-no,"9999999999")
                      NO-LOCK NO-ERROR.
                
                 IF AVAIL s-code THEN
                 DO:
                    IF s-code.CODE EQ 'T' AND lv-cust-x NE "" AND
                       CAN-FIND(FIRST shipto WHERE
                       shipto.company EQ cocode AND
                       shipto.cust-no EQ lv-cust-x AND
                       shipto.ship-no EQ oe-rel.ship-no AND
                       shipto.ship-id EQ oe-rel.ship-id) THEN
                       v-cust-no = lv-cust-x.

                    RELEASE s-code.
                 END.
              END.

              {oe/findrelh.i oe-rel v-cust-no}
              IF AVAIL oe-relh THEN
              FIND LAST oe-rell
                  WHERE oe-rell.company EQ oe-relh.company
                    AND oe-rell.r-no    EQ oe-relh.r-no
                  USE-INDEX r-no NO-LOCK NO-ERROR.
            END.
            IF AVAIL oe-rell THEN out-recid = RECID(oe-rell).
            ELSE DO: 
                v-auto = YES.
                out-recid = recid(oe-rel).                
                RUN oe/relbol.p (RECID(xoe-ordl)).
                v-auto = NO.               
            END.
            run oe/do-bol.p.
            FIND oe-rell WHERE RECID(oe-rell) EQ out-recid NO-LOCK NO-ERROR.
            IF AVAIL oe-rell AND oe-rell.link-no NE 0 THEN
            FIND oe-rel WHERE oe-rel.r-no EQ oe-rell.link-no NO-LOCK NO-ERROR.                    
    end.

/* neet to complete  =======
if oereordr-log then do:  
   find first itemfg {sys/look/itemfgrl.w}
                 and itemfg.i-no eq oe-ordl.i-no no-lock no-error.
   if avail itemfg then run oe/d-fgqty.w  /*oe/fg-qtys.p*/  (recid(itemfg)).
end.  
=============*/

RUN release-shared-buffers.

RUN notify-source.

SESSION:SET-WAIT-STATE("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-bol-all-item B-table-Win 
PROCEDURE create-bol-all-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def var choice as log no-undo.
def buffer bf-rel for oe-rel.
def var v-all-items as log no-undo.
def var v-first as log no-undo.
DEF VAR lv-save-recid AS RECID NO-UNDO.

find xoe-ord where xoe-ord.company = g_company and
                   xoe-ord.ord-no = oe-rel.ord-no no-lock.
find first oe-ctrl where oe-ctrl.company = xoe-ord.company no-lock .

{sys/inc/addrelse.i}
{sys/inc/oereordr.i}

choice = no.

RUN check-release NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.

lv-save-recid = RECID(oe-rel).

/* ======== in sys/inc/addrelse.i 
find sys-ctrl where sys-ctrl.company eq cocode
                and sys-ctrl.name    eq "ADDRELSE"  no-lock no-error.
if not avail sys-ctrl then do:
   create sys-ctrl.
   assign sys-ctrl.company = cocode
          sys-ctrl.name = "ADDRELSE"
          sys-ctrl.log-fld = yes.
   v-do-bol = sys-ctrl.log-fld.             
end.    
else v-do-bol = avail sys-ctrl and sys-ctrl.log-fld and oe-ctrl.ship-from.
========= */       

FOR EACH bf-rel
    WHERE bf-rel.company EQ xoe-ord.company
      AND bf-rel.ord-no  EQ xoe-ord.ord-no
      AND bf-rel.link-no EQ 0
    NO-LOCK:

  RUN oe/rel-stat.p (ROWID(bf-rel), OUTPUT lv-stat).
       
  IF INDEX("AB",lv-stat) EQ 0 THEN DO:
    choice = YES.
    LEAVE.
  END.
END.

if not choice then release bf-rel.

SESSION:SET-WAIT-STATE("general").
/*    
if avail bf-rel and v-do-bol then
do on endkey undo, return:
  choice = v-do-def.
  message "Create Bill of Lading?" update choice format "yes/no".
end.

else choice = no.
*/

find xoe-ordl of xoe-ord no-lock no-error.
if ambig xoe-ordl then
   message "All Items on Order?" view-as alert-box question
            button yes-no update choice .
if choice = ? then return .

v-all-items = choice.

FOR EACH bf-rel
    WHERE bf-rel.company EQ xoe-ord.company
      AND bf-rel.ord-no  EQ xoe-ord.ord-no
      AND bf-rel.link-no EQ 0
    NO-LOCK,
    first xoe-ordl of bf-rel where (recid(xoe-ordl) eq recid(oe-ordl) or v-all-items) no-lock
                      break by bf-rel.rel-date by bf-rel.ship-id:
                   
    if first-of(bf-rel.ship-id) then v-first = yes.
    
    RUN oe/rel-stat.p (ROWID(bf-rel), OUTPUT lv-stat).

    /*??? if index("AB",lv-stat) gt 0 then choice = no.
    else */

    if v-first then do on endkey undo, retry:
         assign  v-first = no
                 choice  = yes.
        message "Create BOL for Release Date-" + trim(string(bf-rel.rel-date)) +
                " and ShipID-" +  trim(bf-rel.ship-id) + " ?"
               view-as alert-box question button yes-no update choice.
    end.
    if choice then do:
        out-recid = recid(bf-rel).
       /* run oe/relbol.p (recid(xoe-ordl)).  */
        if last-of(bf-rel.ship-id) then do:

            v-cust-no = bf-rel.cust-no.
           
            IF addxfer-log THEN
            DO:
               FIND FIRST s-code WHERE
                    s-code.reftable EQ "oe-rel.s-code" AND
                    s-code.company  EQ STRING(bf-rel.r-no,"9999999999")
                    NO-LOCK NO-ERROR.
              
               IF AVAIL s-code THEN
               DO:
                  IF s-code.CODE EQ 'T' AND lv-cust-x NE "" AND
                    CAN-FIND(FIRST shipto WHERE
                    shipto.company EQ cocode AND
                    shipto.cust-no EQ lv-cust-x AND
                    shipto.ship-no EQ bf-rel.ship-no AND
                    shipto.ship-id EQ bf-rel.ship-id) THEN
                    v-cust-no = lv-cust-x.
                  
                  RELEASE s-code.
               END.
            END.

            {oe/findrelh.i bf-rel v-cust-no}
            IF AVAIL oe-relh THEN
            FIND LAST oe-rell
                WHERE oe-rell.company EQ oe-relh.company
                  AND oe-rell.r-no    EQ oe-relh.r-no
                USE-INDEX r-no NO-LOCK NO-ERROR.
            IF AVAIL oe-relh THEN out-recid = RECID(oe-rell).
            ELSE do: 
                v-auto = YES.
                out-recid = recid(bf-rel).                
                RUN oe/relbol.p (RECID(xoe-ordl)).
                v-auto = NO.               
            END.                       
            run oe/do-bol.p.
            FIND oe-rell WHERE RECID(oe-rell) EQ out-recid NO-LOCK NO-ERROR.
            IF AVAIL oe-rell AND oe-rell.link-no NE 0 THEN
            FIND oe-rel WHERE oe-rel.r-no EQ oe-rell.link-no NO-LOCK NO-ERROR.             
        end.   
    end.
end.

/* neet to complete  =======
if oereordr-log then do:  
   find first itemfg {sys/look/itemfgrl.w}
                 and itemfg.i-no eq oe-ordl.i-no no-lock no-error.
   if avail itemfg then run oe/d-fgqty.w  /*oe/fg-qtys.p*/  (recid(itemfg)).
end.  
=============*/

RUN release-shared-buffers.

RUN notify-source.

SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-job B-table-Win 
PROCEDURE create-job :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-bld-job LIKE job.job-no NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR li2 AS INT NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.

  DEF BUFFER b-job FOR job.
  DEF BUFFER oe-rel-job FOR reftable.


  RELEASE job.
  RELEASE itemfg.

  IF AVAIL oe-rel THEN DO TRANSACTION:
    FIND FIRST oe-rel-job NO-LOCK
        WHERE oe-rel-job.reftable EQ "oe-rel.job"
          AND oe-rel-job.code     EQ STRING(oe-rel.r-no,"9999999999")
        USE-INDEX code NO-ERROR.

    IF AVAIL oe-rel-job THEN
    FIND FIRST job NO-LOCK
        WHERE job.company EQ oe-rel-job.company
          AND job.job     EQ INT(oe-rel-job.code2)
        NO-ERROR.

    IF AVAIL job THEN DO:
      MESSAGE "Job already exists for Scheduled Release, rebuild standards?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll.
      IF NOT ll THEN RETURN. ELSE ll = NO.
    END.

    ELSE DO:
      RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).
      IF INDEX("LSABI",lv-stat) GT 0 THEN
      FIND FIRST itemfg NO-LOCK
          WHERE itemfg.company EQ oe-ordl.company
            AND itemfg.i-no    EQ oe-ordl.i-no
            AND itemfg.est-no  NE ""
            AND CAN-FIND(FIRST eb
                         WHERE eb.company  EQ itemfg.company
                           AND eb.est-no   EQ itemfg.est-no
                           AND eb.stock-no EQ itemfg.i-no)
          NO-ERROR.
    END.

    IF AVAIL itemfg THEN DO:
      FIND FIRST sys-ctrl
          WHERE sys-ctrl.company EQ itemfg.company
            AND sys-ctrl.name    EQ "JOBCREAT"
          NO-LOCK NO-ERROR.

      RUN jc/job-no.p (INPUT-OUTPUT v-bld-job, INPUT-OUTPUT li).

      IF v-bld-job EQ "" THEN DO:
        ASSIGN
         v-bld-job = " " + itemfg.est-no
         li        = 0.  

        IF AVAIL sys-ctrl THEN
          v-bld-job = SUBSTR(sys-ctrl.char-fld,1,1) + TRIM(v-bld-job).

        FIND LAST b-job NO-LOCK
            WHERE b-job.company      EQ itemfg.company
              AND TRIM(b-job.job-no) EQ v-bld-job
            NO-ERROR.

        IF AVAIL b-job THEN DO:
          ll = NO.

          MESSAGE "Job(s) already exist for Estimate: " + TRIM(itemfg.est-no) +
                  ", create new one?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE ll.

          IF ll THEN li = b-job.job-no2 + 1.
        END.
      END.

      ELSE DO:
        IF NOT AVAIL sys-ctrl                 OR
          SUBSTR(sys-ctrl.char-fld,2,1) EQ "" THEN DO:
          MESSAGE "Must have System Control Parameter 'JOBCREATE'..."
              VIEW-AS ALERT-BOX ERROR.
          ll = NO.
        END.

        ELSE ll = YES.
        
        IF ll THEN DO:
          FIND LAST b-job NO-LOCK
              WHERE b-job.company EQ cocode
                AND b-job.job-no  BEGINS SUBSTR(sys-ctrl.char-fld,2,1)
              NO-ERROR.
            
          ASSIGN
           li        = (IF AVAIL b-job THEN INT(SUBSTR(b-job.job-no,2,5)) ELSE 0) + 1
           v-bld-job = SUBSTR(sys-ctrl.char-fld,2,1) + STRING(li,"99999")
           li        = 0.
        END.
      END.
    END.

    IF ll THEN DO:
      li2 = 1.
      FIND LAST job NO-LOCK
          WHERE job.company EQ itemfg.company USE-INDEX job
          NO-ERROR.
      FIND LAST job-hdr NO-LOCK
          WHERE job-hdr.company EQ itemfg.company USE-INDEX job
          NO-ERROR.
      IF job-hdr.job GT job.job THEN li2 = job-hdr.job + 1.
      IF job.job GE job-hdr.job THEN li2 = job.job + 1.

      CREATE job.
      ASSIGN
       job.job        = li2
       job.company    = itemfg.company
       job.loc        = locode
       job.est-no     = itemfg.est-no
       job.start-date = TODAY
       job.stat       = "P"
       job.job-no     = v-bld-job
       job.job-no2    = li.

      CREATE oe-rel-job.
      ASSIGN
       oe-rel-job.reftable = "oe-rel.job"
       oe-rel-job.company  = oe-rel.company
       oe-rel-job.code     = STRING(oe-rel.r-no,"9999999999")
       oe-rel-job.code2    = STRING(job.job,"9999999999").
    END.

    IF AVAIL job THEN DO:
      SESSION:SET-WAIT-STATE("general").

      nufile = YES.
      RUN jc/jc-calc.p (RECID(job)).
      fil_id = RECID(job).
      RUN po/do-po.p.
      nufile = NO.

      SESSION:SET-WAIT-STATE("").
    END.
  END.

  RUN release-shared-buffers.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-report-record B-table-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-report-record-1 B-table-Win 
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
     tt-report.po-no   = /*IF AVAIL inv-line THEN inv-line.po-no
                         ELSE
                         IF AVAIL oe-boll THEN oe-boll.po-no
                         ELSE
                         IF AVAIL oe-rell THEN oe-rell.po-no
                         ELSE*/ oe-rel.po-no
     tt-report.qty     = oe-rel.qty
     tt-report.printed = (AVAIL oe-relh AND oe-relh.printed) OR
                         INDEX("PCZ",lv-stat) GT 0.

    FIND FIRST s-code
        WHERE s-code.reftable EQ "oe-rel.s-code"
          AND s-code.company  EQ STRING(oe-rel.r-no,"9999999999")
        NO-LOCK NO-ERROR.
    tt-report.s-code = IF ll-transfer            THEN "T"
                       ELSE
                       IF oe-ordl.is-a-component AND
                          (NOT AVAIL s-code OR
                           s-code.code NE "T")   THEN "S"
                       ELSE
                       IF AVAIL s-code           THEN s-code.code
                       ELSE
                       IF AVAIL oe-rell          THEN oe-rell.s-code
                                                 ELSE "B".

    FIND FIRST ref-lot-no WHERE
         ref-lot-no.reftable EQ "oe-rel.lot-no" AND
         ref-lot-no.company  EQ STRING(oe-rel.r-no,"9999999999")
         NO-LOCK NO-ERROR.

    IF AVAIL ref-lot-no THEN
    DO:
       tt-report.lot-no = ref-lot-no.CODE.
       RELEASE ref-lot-no.
    END.

    FIND FIRST ref-sell-price WHERE
         ref-sell-price.reftable EQ "oe-rel.sell-price" AND
         ref-sell-price.company  EQ STRING(oe-rel.r-no,"9999999999")
         NO-LOCK NO-ERROR.

    IF AVAIL ref-sell-price THEN
    DO:
       tt-report.sell-price = ref-sell-price.val[1].
       RELEASE ref-sell-price.
    END.

    IF oeinq THEN 
      tt-report.key-01 = STRING(9999999999 - INT(tt-report.key-01),"9999999999").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE custom-panel-state B-table-Win 
PROCEDURE custom-panel-state :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT-OUTPUT PARAM io-panel-state AS CHAR NO-UNDO.


  RUN enable-ticket.

  IF NOT AVAIL oe-ordl OR oe-ordl.stat EQ "C" OR oe-ordl.opened EQ NO THEN
    io-panel-state = "disable-all".
  ELSE
  IF AVAIL oe-rel THEN
    IF io-panel-state EQ "add-only" OR io-panel-state EQ "disable-all" THEN
      io-panel-state = "initial".
    ELSE DO:
      IF AVAIL tt-report AND tt-report.s-code EQ "I" THEN
        io-panel-state = "NoBOL".

      FIND FIRST itemfg NO-LOCK
          WHERE itemfg.company      EQ oe-ordl.company
            AND itemfg.i-no         EQ oe-ordl.i-no
            AND TRIM(itemfg.est-no) NE ""
          NO-ERROR.

      IF TRIM(oe-ordl.job-no) NE ""               OR
         TRIM(oe-ordl.est-no) NE ""               OR
         NOT AVAIL itemfg                         OR
         NOT CAN-FIND(FIRST eb
                      WHERE eb.company  EQ itemfg.company
                        AND eb.est-no   EQ itemfg.est-no
                        AND eb.stock-no EQ itemfg.i-no
                        AND eb.est-type NE 3
                        AND eb.est-type NE 4
                        AND eb.est-type NE 7
                        AND eb.est-type NE 8)     OR
         oe-ordl.is-a-component                   THEN
        io-panel-state = io-panel-state + "," + "NoJob".
     
      IF SUBSTR(io-panel-state,1,1) EQ "," THEN
        io-panel-state = SUBSTR(io-panel-state,2,1000).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-phantoms B-table-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-ticket B-table-Win 
PROCEDURE enable-ticket :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE relStatus AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pHandle AS HANDLE NO-UNDO.


  out-recid = ?.

  IF AVAIL oe-rel THEN DO:
    FIND FIRST oe-ord NO-LOCK
        WHERE oe-ord.company EQ oe-rel.company 
          AND oe-ord.ord-no  EQ oe-rel.ord-no
        NO-ERROR.
    {oe/rel-stat.i relStatus}
    {methods/run_link.i "container-source" "relTicketEnabled" "(CAN-DO('A,B',relStatus))"}
    IF NOT CAN-DO('A,B',relStatus) THEN RETURN.
    /*FIND FIRST oe-relh NO-LOCK WHERE oe-relh.r-no EQ oe-rel.link-no NO-ERROR.*/
    IF AVAIL oe-relh THEN DO:
      IF AVAIL oe-rell THEN out-recid = RECID(oe-rell).
      RUN custom/setUserPrint.p (g_company,'oe-relh_.',
                                 'begin_cust-no,end_cust-no,begin_relnum,end_relnum,begin_ord-no,end_ord-no,tb_printed,tb_posted',
                                 oe-relh.cust-no + ',' + oe-relh.cust-no + ',' +
                                 STRING(oe-relh.release#) + ',' + STRING(oe-relh.release#) + ',' +
                                 STRING(oe-rel.ord-no) + ',' + STRING(oe-rel.ord-no) + ',' +
                                 STRING(oe-relh.printed) + ',' + STRING(oe-relh.posted)).
    END.
  END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record B-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-oe-ordl FOR oe-ordl.
  DEF BUFFER b-oe-rel FOR oe-rel.


  /* Code placed here will execute PRIOR to standard behavior. */
  IF CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}) THEN
    RETURN "ADM-ERROR".

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

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
  def var ll-ans as log no-undo.
  def var ldt-ship as date form "99/99/9999" no-undo.
  def buffer bf-rel for oe-rel .
  def var ld-prev-rel-qty as int no-undo.
  def var v-qty-sum as int no-undo.
  DEF VAR ls-key-02 LIKE tt-report.key-02 NO-UNDO.

  DEF BUFFER b-ordl FOR oe-ordl.

  
  /* Code placed here will execute PRIOR to standard behavior. */
  if not avail oe-rel and lv-rel-recid <> ? then
     find oe-rel where recid(oe-rel) = lv-rel-recid.
  ld-prev-rel-qty = if adm-new-record then 0 else oe-rel.qty.
  
  find oe-ord of oe-ordl no-lock.

  ldt-ship = oe-rel.rel-date.
  ls-po = tt-report.po-no.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  oe-rel.po-no = tt-report.po-no.

  FIND FIRST s-code
      WHERE s-code.reftable EQ "oe-rel.s-code"
        AND s-code.company  EQ STRING(oe-rel.r-no,"9999999999")
      NO-ERROR.
  IF NOT AVAIL s-code THEN DO:
    CREATE s-code.
    ASSIGN
     s-code.reftable = "oe-rel.s-code"
     s-code.company  = STRING(oe-rel.r-no,"9999999999").
  END.
  s-code.code = tt-report.s-code.

  IF oe-ordl.is-a-component AND CAN-DO("B,I",s-code.code) THEN s-code.code = "S".

  FIND FIRST ref-lot-no WHERE
       ref-lot-no.reftable EQ "oe-rel.lot-no" AND
       ref-lot-no.company  EQ STRING(oe-rel.r-no,"9999999999")
       EXCLUSIVE-LOCK NO-ERROR.

  IF NOT AVAIL ref-lot-no THEN
  DO:
     CREATE ref-lot-no.
     ASSIGN
       ref-lot-no.reftable = "oe-rel.lot-no"
       ref-lot-no.company  = STRING(oe-rel.r-no,"9999999999").
  END.

  ref-lot-no.CODE = tt-report.lot-no:SCREEN-VALUE IN BROWSE {&browse-name}.

  FIND FIRST ref-sell-price WHERE
       ref-sell-price.reftable EQ "oe-rel.sell-price" AND
       ref-sell-price.company  EQ STRING(oe-rel.r-no,"9999999999")
       EXCLUSIVE-LOCK NO-ERROR.

  IF NOT AVAIL ref-sell-price THEN
  DO:
     CREATE ref-sell-price.
     ASSIGN 
       ref-sell-price.reftable = "oe-rel.sell-price"
       ref-sell-price.company  = STRING(oe-rel.r-no,"9999999999").
  END.

  ref-sell-price.val[1] = DEC(tt-report.sell-price:SCREEN-VALUE IN BROWSE {&browse-name}).
  
  RELEASE ref-lot-no.
  RELEASE ref-sell-price.

  IF INDEX("AB",lv-stat) LE 0 THEN
    oe-rel.rel-date = DATE(INT(SUBSTR(tt-report.key-02,1,2)),
                           INT(SUBSTR(tt-report.key-02,3,2)),
                           INT(SUBSTR(tt-report.key-02,5,4))).

  v-qty-sum = 0.
  for each bf-rel where bf-rel.company = oe-ord.company
                       and bf-rel.ord-no = oe-ord.ord-no 
                       AND bf-rel.LINE = oe-ordl.LINE    /* 01/20/03 YSK TASK 01170303*/
                       and bf-rel.i-no = oe-ordl.i-no no-lock :
      RUN oe/rel-stat.p (ROWID(bf-rel), OUTPUT lv-stat).
      FIND FIRST s-code
          WHERE s-code.reftable EQ "oe-rel.s-code"
            AND s-code.company  EQ STRING(bf-rel.r-no,"9999999999")
          NO-LOCK NO-ERROR.
      IF (NOT AVAIL s-code OR INDEX("BS",s-code.code) GT 0) AND
         NOT CAN-DO("C,Z",lv-stat)                          THEN
        v-qty-sum = v-qty-sum + bf-rel.qty. 
  end.

  if v-qty-sum + oe-ordl.ship-qty gt oe-ordl.qty + 
    (oe-ordl.qty * (oe-ordl.over-pct / 100)) 
  then message "Total Planned release quantity will exceed the Or" +
                        "der quantity + the Overrun %..."
                view-as alert-box warning.

  IF ldt-ship NE oe-rel.rel-date                      AND
     NOT adm-new-record                               AND
     CAN-FIND(FIRST bf-rel
              WHERE bf-rel.company  EQ oe-rel.company
                AND bf-rel.ord-no   EQ oe-rel.ord-no
                AND bf-rel.link-no  EQ 0
                AND bf-rel.rel-date EQ ldt-ship
                AND ROWID(bf-rel)   NE ROWID(oe-rel)) THEN DO:
    MESSAGE "Update all other Scheduled Releases for this order with a" SKIP
            "release date of " + STRING(ldt-ship,"99/99/9999") + " to " +
            STRING(oe-rel.rel-date,"99/99/9999")
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE ll-ans.
    IF ll-ans THEN        
    FOR EACH bf-rel
        WHERE bf-rel.company  EQ oe-rel.company
          AND bf-rel.ord-no   EQ oe-rel.ord-no
          AND bf-rel.link-no  EQ 0
          AND bf-rel.rel-date EQ ldt-ship
          AND ROWID(bf-rel)   NE ROWID(oe-rel):
      RUN oe/rel-stat.p (ROWID(bf-rel), OUTPUT lv-stat).
      IF INDEX("SLI",lv-stat) GT 0 THEN bf-rel.rel-date = oe-rel.rel-date.
    END.        
  END.

  IF ls-po NE tt-report.po-no                AND
     CAN-FIND(FIRST bf-rel
              WHERE bf-rel.company  EQ oe-rel.company
                AND bf-rel.ord-no   EQ oe-rel.ord-no
                AND bf-rel.link-no  EQ 0
                AND ROWID(bf-rel)   NE ROWID(oe-rel)) THEN DO:
    MESSAGE "Change item PO Number on all items? "
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans.
    IF ll-ans THEN DO:
      ll-ans = NO.
      MESSAGE "All ship dates?"
          VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE ll-ans.
      IF NOT ll-ans THEN DO:
        ldt-ship = oe-rel.rel-date.
        MESSAGE "Which ship date do you wish to update? " UPDATE ldt-ship.
      END.         
      FOR EACH  bf-rel
          WHERE bf-rel.company EQ oe-rel.company
            AND bf-rel.ord-no  EQ oe-rel.ord-no
            AND bf-rel.link-no EQ 0
            AND (ll-ans OR (bf-rel.rel-date EQ ldt-ship)):
        RUN oe/rel-stat.p (ROWID(bf-rel), OUTPUT lv-stat).
        IF INDEX("SLI",lv-stat) GT 0 THEN bf-rel.po-no = tt-report.po-no.               
      END.
    END.        
  END.

  if (li-ship-no <> 0 and li-ship-no <> oe-rel.ship-no) /* or
     adm-adding-record */ then do:
     find oe-ord where oe-ord.company = oe-rel.company 
                   and oe-ord.ord-no = oe-rel.ord-no no-lock .

     RUN oe/custxship.p (oe-rel.company,
                         oe-ord.cust-no,
                         oe-rel.ship-id,
                         BUFFER shipto).

     if avail shipto then do:
        assign oe-rel.ship-no = shipto.ship-no
                                 oe-rel.ship-addr[1] = shipto.ship-addr[1]
                                 oe-rel.ship-addr[2] = shipto.ship-addr[2]
                                 oe-rel.ship-city = shipto.ship-city
                                 oe-rel.ship-state = shipto.ship-state
                                 oe-rel.ship-zip = shipto.ship-zip
                                 oe-rel.ship-i[1] = shipto.notes[1]
                                 oe-rel.ship-i[2] = shipto.notes[2]
                                 oe-rel.ship-i[3] = shipto.notes[3]
                                 oe-rel.ship-i[4] = shipto.notes[4].
/* maybe later
        IF shipto.notes[1] <> "" OR shipto.notes[2] <> "" OR
           shipto.notes[3] <> "" OR shipto.notes[4] <> "" THEN DO:
                  FIND FIRST notes WHERE notes.rec_key = oe-rel.rec_key NO-LOCK NO-ERROR.
                  IF NOT AVAIL notes THEN DO:
                     CREATE notes.
                     ASSIGN notes.rec_key = oe-rel.rec_key
                            notes.note_date = TODAY
                            notes.note_title = shipto.notes[1]
                            notes.note_text = shipto.notes[1] + CHR(13) +
                                              shipto.notes[2] + CHR(13) +
                                              shipto.notes[3] + CHR(13) +
                                              ship.notes[4] + CHR(13).
                   END.
         END.
         ===========*/
     END.
  end.   

  FIND b-ordl WHERE ROWID(b-ordl) EQ ROWID(oe-ordl).
  b-ordl.t-rel-qty = b-ordl.t-rel-qty + oe-rel.qty - ld-prev-rel-qty.
  FIND b-ordl WHERE ROWID(b-ordl) EQ ROWID(oe-ordl) NO-LOCK.

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
  ll-canceled = YES.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN release-shared-buffers.

  ASSIGN
     btnCalendar:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
     btnCalendar:HIDDEN IN FRAME {&FRAME-NAME} = YES.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var v-qty-sum as int no-undo.
  def var v-nxt-r-no as int no-undo.
  def var v-lst-rel as date INIT TODAY no-undo.
  def var v-pct-chg as dec no-undo.
  def var v-ship-id like oe-rel.ship-id no-undo.
  def var v-carrier like oe-rel.carrier no-undo.
  def var v-num-shipto as int no-undo.
  def var v-qty-mod as log no-undo.
  def buffer bf-rel for oe-rel.

  
  /* Code placed here will execute PRIOR to standard behavior. */

  /** Find last release in the oe-rel file. **/
  find first bf-rel use-index seq-no no-lock no-error.
  assign
    v-nxt-r-no = (if avail bf-rel then bf-rel.r-no else 0) + 1
    v-ship-id = IF AVAIL oe-rel THEN oe-rel.ship-id ELSE ""
    v-carrier = IF AVAIL oe-rel THEN oe-rel.carrier ELSE "".
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  /* --------------------------------------------------- oe/oe-rel.a   6/93 rd  */
  /* add module - order entry release lines                                     */
  /* -------------------------------------------------------------------------- */

  lv-rel-recid = recid(oe-rel).
  assign v-qty-sum  = 0.

  /*  {oe/oe-rel.a &fil="oe-ordl"}  */
  if avail oe-ordl then do:
     FIND FIRST oe-ord OF oe-ordl NO-LOCK.
     for each bf-rel where bf-rel.company = oe-ord.company
                       and bf-rel.ord-no = oe-ord.ord-no
                       and bf-rel.i-no = oe-ordl.i-no 
                       and bf-rel.LINE = oe-ordl.LINE
                       NO-LOCK:
         FIND FIRST s-code
             WHERE s-code.reftable EQ "oe-rel.s-code"
               AND s-code.company  EQ STRING(bf-rel.r-no,"9999999999")
             NO-LOCK NO-ERROR.
         IF NOT AVAIL s-code OR CAN-DO("B,S",s-code.code) THEN
           v-qty-sum = v-qty-sum + bf-rel.qty. 
     end.
     
     if v-qty-sum GE oe-ordl.qty + (oe-ordl.qty * (oe-ordl.over-pct / 100)) then
        message "Total Planned release quantity will exceed the Or" +
                        "der quantity + the Underrun %."
                view-as alert-box warning.
        
     find first sys-ctrl where sys-ctrl.company eq cocode
                          and sys-ctrl.name    eq "OECARIER"
               no-lock no-error.
     if not avail sys-ctrl then do:
       create sys-ctrl.
       assign sys-ctrl.company  = cocode
             sys-ctrl.name     = "OECARIER"
             sys-ctrl.descrip  = "Default carrier from Header or ShipTo:"
             sys-ctrl.char-fld = "ShipTo".       
       do while true:
          message "Default Shipping Carrier from Header or Shipto?" update sys-ctrl.char-fld.
          if sys-ctrl.char-fld = "Header" or sys-ctrl.char-fld = "ShipTo" then leave. 
       end.
     end.

     RELEASE shipto.

     IF oeship-cha EQ "OEShipto" THEN DO:
       FIND FIRST shipto NO-LOCK
           WHERE shipto.company EQ oe-ord.company
             AND shipto.cust-no EQ oe-ord.cust-no
             AND shipto.ship-id EQ v-ship-id
           NO-ERROR.
       IF v-carrier EQ "" THEN v-carrier = oe-ord.carrier.
     END.

     ELSE
     IF oe-ordl.est-no NE "" THEN
     FOR EACH eb NO-LOCK
         WHERE eb.company EQ oe-ordl.company
           AND eb.est-no  EQ oe-ordl.est-no
           AND eb.cust-no EQ oe-ord.cust-no
           AND eb.form-no NE 0,
         FIRST shipto OF eb NO-LOCK
         BREAK BY eb.stock-no DESC:
       IF LAST(eb.stock-no)           OR
          eb.stock-no EQ oe-ordl.i-no THEN LEAVE.
     END.

    /*========
        where shipto.company eq xoe-ord.company
          and shipto.cust-no eq eb.cust-no
          and shipto.ship-id eq eb.ship-id
          and shipto.ship-no eq eb.ship-no
        no-lock no-error.
    if not avail shipto then
    find first shipto
        where shipto.company eq xoe-ord.company
          and shipto.cust-no eq eb.cust-no
          and shipto.ship-id eq eb.ship-id
        no-lock no-error.
    if not avail shipto then
    find shipto
        where shipto.company eq xoe-ord.company
          and shipto.cust-no eq eb.cust-no
          and shipto.ship-no eq eb.ship-no
        no-lock no-error.
    */
    
    IF NOT AVAIL shipto THEN
    FOR EACH shipto
        WHERE shipto.company  EQ cocode
           AND shipto.cust-no EQ (IF lv-cust-x NE ""         AND
                                     tt-report.s-code EQ "T" THEN lv-cust-x
                                                             ELSE oe-ord.cust-no)
        NO-LOCK
        BREAK BY shipto.ship-no DESC:
      IF shipto.ship-id EQ oe-ord.cust-no OR LAST(shipto.ship-no) THEN LEAVE.
    END.

    IF v-carrier EQ "" AND AVAIL shipto THEN v-carrier = shipto.carrier.

    assign oe-rel.company   = cocode
           oe-rel.loc       = locode
           oe-rel.ord-no    = oe-ordl.ord-no
           oe-rel.i-no      = oe-ordl.i-no
           oe-rel.cust-no   = oe-ord.cust-no
           oe-rel.po-no     = if oe-ordl.po-no ne "" then oe-ordl.po-no 
                                                     else oe-ord.po-no
           oe-rel.qty       = oe-ordl.qty - v-qty-sum
           oe-rel.line      = oe-ordl.line
           oe-rel.s-comm[1] = oe-ord.s-comm[1]
           oe-rel.s-comm[2] = oe-ord.s-comm[2]
           oe-rel.s-comm[3] = oe-ord.s-comm[3]
           oe-rel.s-name[1] = oe-ord.sname[1]
           oe-rel.s-name[2] = oe-ord.sname[2]
           oe-rel.s-name[3] = oe-ord.sname[3]
           oe-rel.s-pct[1]  = oe-ord.s-pct[1]
           oe-rel.s-pct[2]  = oe-ord.s-pct[2]
           oe-rel.s-pct[3]  = oe-ord.s-pct[3]
           oe-rel.sman[1]   = oe-ord.sman[1]
           oe-rel.sman[2]   = oe-ord.sman[2]
           oe-rel.sman[3]   = oe-ord.sman[3]
           oe-rel.sold-no   = oe-ord.sold-no
           oe-rel.carrier   = if sys-ctrl.char-fld = "Shipto" and avail shipto then shipto.carrier
                              else v-carrier
           oe-rel.r-no      = v-nxt-r-no
           oe-rel.rel-date  = if oereleas-cha eq "LastShip" then oe-ord.last-date
                                                            else oe-ordl.req-date.

          if oe-rel.qty lt 0 then oe-rel.qty = 0.

    oe-rel.tot-qty = oe-rel.qty.

    if oe-rel.rel-date le v-lst-rel then oe-rel.rel-date = v-lst-rel + 1.

    if avail shipto then
       assign oe-rel.ship-addr[1] = shipto.ship-addr[1]
              oe-rel.ship-city    = shipto.ship-city
              oe-rel.ship-state   = shipto.ship-state
              oe-rel.ship-zip     = shipto.ship-zip
              oe-rel.ship-no      = shipto.ship-no
              oe-rel.ship-id      = shipto.ship-id
              oe-rel.ship-i[1]    = shipto.notes[1]
              oe-rel.ship-i[2]    = shipto.notes[2]
              oe-rel.ship-i[3]    = shipto.notes[3]
              oe-rel.ship-i[4]    = shipto.notes[4].
    else assign oe-rel.ship-no   = oe-ord.sold-no
                oe-rel.ship-id   = oe-ord.sold-id
                oe-rel.ship-i[1] = oe-ord.ship-i[1]
                oe-rel.ship-i[2] = oe-ord.ship-i[2]
                oe-rel.ship-i[3] = oe-ord.ship-i[3]
                oe-rel.ship-i[4] = oe-ord.ship-i[4].

    RUN create-report-record-1 (NO, oe-rel.rel-date).
  end.

  else do:
    message " Order Line item record is not avail..." view-as alert-box error.
    return error.
  end.

/*run dispatch('display-fields').*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       oe/oe-rel.del, oe/oerelunp.p 
------------------------------------------------------------------------------*/
  DEF BUFFER bf-rel FOR oe-rel.
  DEF BUFFER b-tt-report FOR tt-report.


  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT AVAIL oe-rel THEN DO:
      MESSAGE "Manually added actual releases may not be deleted..."
              VIEW-AS ALERT-BOX ERROR .
      RETURN.
  END.

  FIND bf-rel WHERE RECID(bf-rel) = RECID(oe-rel). 

  FIND FIRST oe-ord
      WHERE oe-ord.company EQ oe-rel.company 
        AND oe-ord.ord-no  EQ oe-rel.ord-no
      NO-LOCK.

  {oe/rel-stat.i lv-stat}

  IF INDEX("CPZ",lv-stat) > 0 THEN DO:
      MESSAGE "Posted releases may not be deleted, must delete BOL first!"
              VIEW-AS ALERT-BOX ERROR .
      RETURN.
  END.
  ELSE IF INDEX("AB",lv-stat) > 0 THEN DO:
      MESSAGE "Actual or Backordered release should not be deleted. " SKIP
              "Delete anyway?"
               VIEW-AS ALERT-BOX WARNING  BUTTON YES-NO UPDATE ll-ans1 AS LOG.
      IF ll-ans1 AND AVAIL oe-relh THEN DO:
         bf-rel.link-no = 0 .
         FOR EACH oe-rell WHERE oe-rell.company = bf-rel.company
                            AND oe-rell.r-no = oe-relh.r-no
                            AND oe-rell.i-no = bf-rel.i-no
                          USE-INDEX r-no:
             RUN oe/relldel1.p  (RECID(oe-rell)).
             DELETE oe-rell.
         END.
         FIND FIRST oe-rell WHERE oe-rell.company = bf-rel.company
                              AND oe-rell.r-no = oe-relh.r-no
                            USE-INDEX r-no NO-LOCK NO-ERROR.
         IF NOT AVAIL oe-rell THEN DO:
            FIND CURRENT oe-relh NO-ERROR.
            IF AVAIL oe-relh THEN DO: 
               DISABLE TRIGGERS FOR LOAD OF oe-relh.
               DELETE oe-relh.
            END.
         END.
      END.
      ELSE RETURN.
  END.
  ELSE
  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.
  /*
  find oe-ord where oe-ord.company = oe-rel.company 
                and oe-ord.ord-no = oe-rel.ord-no no-lock .
  if  oe-ord.posted then return.
     
  find first oe-relh where oe-relh.company = oe-rel.company 
                       and oe-relh.ord-no = oe-rel.ord-no
                       and oe-relh.ship-id = oe-rel.ship-id 
                       and oe-relh.posted = no use-index relh
                       no-lock no-error.

  if avail oe-relh then do:  
     find first oe-rell
         WHERE oe-rell.company EQ oe-relh.company
           AND oe-rell.r-no    EQ oe-relh.r-no
           and oe-rell.i-no = oe-rel.i-no
           and oe-rell.link-no = oe-rel.r-no
         USE-INDEX r-no no-lock no-error.
     if avail oe-rell then do:
        message "Acutal release entry exists for this planned release. Can not delete. " 
                view-as alert-box error.
        return.
     end.                      
  end.                      
  else if not ll-canceled then do:
       if oe-rel.link-no <> 0  then run unpost-item.       
       else do:
           MESSAGE "Delete Currently Selected Record?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE response AS LOGICAL.
           IF NOT response THEN  RETURN "ADM-ERROR":U.
              find first oe-ordl where oe-ordl.company eq oe-rel.company
                          and oe-ordl.ord-no  eq oe-rel.ord-no
                          and oe-ordl.i-no    eq oe-rel.i-no
                          and oe-ordl.line    eq oe-rel.line     no-error.
              if avail oe-ordl then oe-ordl.t-rel-qty = oe-ordl.t-rel-qty - oe-rel.qty. 
       end.

  end.              
  ll-canceled = no.
  */

  FIND FIRST notes WHERE notes.rec_key EQ oe-rel.rec_key NO-ERROR.
  IF AVAIL notes THEN DELETE notes.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /*FOR EACH b-tt-report WHERE NOT CAN-FIND(oe-rel WHERE RECID(oe-rel) EQ b-tt-report.rec-id):
    DELETE b-tt-report.
  END.*/

  lv-rel-recid = ?.

  RUN release-shared-buffers.
  
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
  IF NOT AVAIL {&FIRST-EXTERNAL-TABLE}                 OR
     NOT AVAIL {&FIRST-TABLE-IN-QUERY-{&browse-name}}  OR
     NOT AVAIL {&SECOND-TABLE-IN-QUERY-{&browse-name}} THEN
    RETURN "ADM-ERROR".

  /*DO WITH FRAME {&FRAME-NAME}:
    IF NOT oereleas-log THEN
      oe-rel.qty:LABEL IN BROWSE {&browse-name} = "Shipped Qty".
  END.*/

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
  
  /* Code placed here will execute PRIOR to standard behavior. */

  if avail oe-rel AND NOT adm-new-record then do:
    find oe-ord where oe-ord.company = g_company and
                    oe-ord.ord-no = oe-ordl.ord-no no-lock.
        
    RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).

    if index("CPZ", lv-stat) > 0 then do:
       MESSAGE ENTRY(INDEX("ABCPZ",lv-stat),"Actual,Backorder,Completed,Posted,Invoice")
               "release entries can not be modified"
           view-as alert-box error.
       adm-brs-in-update = NO.
       RETURN ERROR.
    end.
  end.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN release-shared-buffers.

  DO WITH FRAME {&FRAME-NAME}:
    IF INDEX("AB",lv-stat) GT 0 OR ll-transfer THEN
      APPLY "entry" TO tt-report.printed IN BROWSE {&browse-name}.
    ELSE
      APPLY "entry" to tt-report.s-code IN BROWSE {&browse-name}.
  END.
  
  v-browse-in-update = YES.

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
  relh-recid = ?.

  RUN build-report-file.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /*RUN delete-phantoms.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def buffer bf-rel for oe-rel .
  def var lv-repos-recid as recid no-undo.
  DEF VAR lv-key-02 LIKE tt-report.key-02 NO-UNDO.
  DEF VAR lv-printed LIKE tt-report.printed NO-UNDO.
  def var char-hdl as cha no-undo.


  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-s-code NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-ship-id NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
      
  RUN valid-po-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-key-02 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   
  /* ==== validation ==========*/
 /* if int(oe-rel.qty:screen-value in browse {&browse-name}) <= 0 then do:
       message "Planned release quantity must be greater than 0." view-as alert-box error.
       apply "entry" to oe-rel.qty.
       return no-apply.
  end.
  */
  find first bf-rel where bf-rel.company = oe-rel.company 
                      and bf-rel.ord-no = oe-rel.ord-no 
                      and bf-rel.line = oe-rel.line
                      and bf-rel.rel-date = ld-date
                      and bf-rel.ship-id = oe-rel.ship-id:SCREEN-VALUE IN BROWSE {&browse-name}
                          /*bf-rel.ship-no = oe-rel.ship-no:screen-value */
                      and bf-rel.i-no = oe-rel.i-no
                      and recid(bf-rel) <> recid(oe-rel)
                      no-lock no-error.
  if avail bf-rel then do:
     message "Shipto already exists for this date. Continue with unique PO#?"
             view-as alert-box question button yes-no update ll-ans as log.
     if not ll-ans then do:
        apply "entry" to tt-report.s-code.
        return no-apply.
     end.   
  end.

 /*
  if tt-report.key-02:modified and ld-date LT today then do:
       message "Release date can not be earlier than Today." view-as alert-box error.
       apply "entry" to tt-report.key-02.
       return no-apply.
  end.
 */

  ASSIGN
   lv-repos-recid = RECID(oe-rel)
   lv-key-02      = tt-report.key-02
   lv-printed     = tt-report.printed
   ll-skip        = YES
   btnCalendar:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
   btnCalendar:HIDDEN IN FRAME {&FRAME-NAME} = YES.

  RUN release-shared-buffers.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN release-shared-buffers.

  ll-skip = NO.

  IF tt-report.printed NE lv-printed THEN DO:
    {oe/rel-stat.i lv-stat}

    IF AVAIL oe-relh AND INDEX("AB",lv-stat) GT 0 THEN DO TRANSACTION:
      FIND CURRENT oe-relh.
      oe-relh.printed = tt-report.printed.
      FIND CURRENT oe-relh NO-LOCK.
    END.
  END.

  IF tt-report.key-02 NE lv-key-02 AND NOT adm-new-record THEN DO:
    RUN update-dates.

    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'record-source':U,OUTPUT char-hdl).
    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN get-link-handle IN adm-broker-hdl (WIDGET-HANDLE(char-hdl),'record-source':U,OUTPUT char-hdl).
    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN dispatch IN WIDGET-HANDLE(char-hdl) ("display-fields").
  END.
  adm-brs-in-update = NO.

  run dispatch('open-query').
  reposition {&browse-name} to recid lv-repos-recid no-error.
  if not error-status:error then run dispatch ('row-changed').

  ASSIGN
   lv-rel-recid       = ?
   v-browse-in-update = NO.

  RUN set-buttons.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view B-table-Win 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR phandle AS HANDLE NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /*{methods/template/local/setvalue.i} */
  DEFINE VARIABLE relStatus AS CHARACTER NO-UNDO.

  IF AVAILABLE oe-rel THEN DO:
    RUN oe/rel-stat.p (ROWID(oe-rel),OUTPUT relStatus).
    {methods/run_link.i "container-source" "relTicketEnabled" "(CAN-DO('A,B',relStatus))"}
  END.

  &IF "{&SETVALUE}" NE "no" &THEN
     &IF "{&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}" NE "" &THEN
         &Scoped-define TABLENAME {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}
     &ELSE
         &Scoped-define TABLENAME {&FIRST-EXTERNAL-TABLE}
     &ENDIF

     &IF INDEX("{&NORECKEY}","{&TABLENAME}") = 0 &THEN
         IF AVAILABLE {&TABLENAME} THEN
         DO:
           FIND CURRENT {&TABLENAME} NO-LOCK.
           {methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header"
                    "({&TABLENAME}.rec_key, string(oe-rel.ord-no))" }
           {methods/run_link.i "CONTAINER-SOURCE" "Notes-Message"
                    "(CAN-FIND(FIRST notes WHERE notes.rec_key = {&TABLENAME}.rec_key))"}
           {methods/run_link.i "CONTAINER-SOURCE" "MF-Message"
                    "(CAN-FIND(FIRST mfvalues WHERE mfvalues.rec_key = {&TABLENAME}.rec_key))"}
         END.
     &ENDIF
  &ENDIF
  APPLY "ENTRY" TO FRAME {&FRAME-NAME}.
  
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

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST oe-ord
        WHERE oe-ord.company EQ oe-rel.company 
          AND oe-ord.ord-no  EQ oe-rel.ord-no
        NO-LOCK.

    RUN oe/custxship.p (oe-rel.company,
                        oe-ord.cust-no,
                        oe-rel.ship-id:SCREEN-VALUE IN BROWSE {&browse-name},
                        BUFFER shipto).

    IF AVAIL shipto THEN                           
      ASSIGN
       oe-rel.ship-addr[1]:SCREEN-VALUE IN BROWSE {&browse-name} = shipto.ship-addr[1]
       oe-rel.ship-city:SCREEN-VALUE IN BROWSE {&browse-name}    = shipto.ship-city
       oe-rel.ship-state:SCREEN-VALUE IN BROWSE {&browse-name}   = shipto.ship-state 
       oe-rel.carrier:SCREEN-VALUE IN BROWSE {&browse-name}      = shipto.carrier
       li-ship-no = shipto.ship-no.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE notify-source B-table-Win 
PROCEDURE notify-source :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR lv-rowid AS ROWID NO-UNDO.


  IF AVAIL oe-ordl THEN DO:
    lv-rowid = IF AVAIL oe-rel THEN ROWID(oe-rel) ELSE ?.

    FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.

    IF AVAIL oe-ord THEN DO:
      RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"bolrel-source",OUTPUT char-hdl).

      IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
        RUN reposit-item IN WIDGET-HANDLE(char-hdl) (RECID(oe-ord), RECID(oe-ordl)).
    END.

    RUN reopen-query.

    IF lv-rowid NE ? THEN RUN repo-query (lv-rowid).
  END.

  RUN enable-ticket.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE release-item B-table-Win 
PROCEDURE release-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
session:set-wait-state ('general').
find first oe-ctrl where oe-ctrl.company = g_company no-lock no-error.
{sys/inc/addrelse.i}
/*
if v-do-bol then do:
  message "Create Bill of lading?" view-as alert-box question button yes-no
                 update choice as log.
  if choice then do:
     run create-bol.
     return.
  end.
end.
*/
  
  find xoe-ord where xoe-ord.company = g_company and
                     xoe-ord.ord-no = oe-ordl.ord-no no-lock.

  RUN check-release NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN.  

  RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).

  if index("ABCPZ", lv-stat) > 0 then do:
     message entry(index("ABCPZ",lv-stat),"Actual,Backorder,Completed,Posted,Invoice")
             "release entries can not be modified"
             view-as alert-box error.
     return .
  end.

  FOR EACH w-ordl:
    DELETE w-ordl.
  END.
  v-auto = NO.

  fil_id = recid(oe-ordl).  
  run oe/actrel.p (recid(oe-rel)).

  RUN release-shared-buffers.

  RUN notify-source.

session:set-wait-state ('').  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE release-item-all B-table-Win 
PROCEDURE release-item-all :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
session:set-wait-state ('general').
find first oe-ctrl where oe-ctrl.company = g_company no-lock no-error.
{sys/inc/addrelse.i}
/*
if v-do-bol then do:
  message "Create Bill of lading?" view-as alert-box question button yes-no
                 update choice as log.
  if choice then do:
     run create-bol.
     return.
  end.
end.
*/
  
  find xoe-ord where xoe-ord.company = g_company and
                     xoe-ord.ord-no = oe-ordl.ord-no no-lock.

  RUN check-release NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN.    

  RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).

  if index("ABCPZ", lv-stat) > 0 then do:
     message entry(index("ABCPZ",lv-stat),"Actual,Backorder,Completed,Posted,Invoice")
             "release entries can not be modified"
             view-as alert-box error.
     return .
  end.

  def var ls-rel-what as cha no-undo.
  run oe/d-relwht.w (output ls-rel-what) .
  
  FOR EACH w-ordl:
    DELETE w-ordl.
  END.
  v-auto = NO.

  if ls-rel-what = "all" then do:
     fil_id = recid(oe-ordl).  
     run oe/autorel.p .
  end.
  else if ls-rel-what = "item" then do:
       fil_id = recid(oe-ordl).  
       run oe/actrel.p (recid(oe-rel)).
  end.

  RUN release-shared-buffers.

  RUN notify-source.
  
session:set-wait-state ('').  
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
  
  RELEASE xoe-ord.
  RELEASE xoe-ordl.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE relticket-printed B-table-Win 
PROCEDURE relticket-printed :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


  FIND oe-rell WHERE RECID(oe-rell) EQ out-recid NO-LOCK NO-ERROR.
  IF AVAIL oe-rell AND oe-rell.link-no NE 0 THEN
  FIND oe-rel WHERE oe-rel.r-no EQ oe-rell.link-no NO-LOCK NO-ERROR.

  RUN notify-source.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-query B-table-Win 
PROCEDURE reopen-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN dispatch ('open-query').

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
  DEF INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.


  REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.

  RUN dispatch ("row-changed").

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
  {src/adm/template/sndkycas.i "company" "oe-rel" "company"}
  {src/adm/template/sndkycas.i "Carrier" "oe-rel" "Carrier"}
  {src/adm/template/sndkycas.i "r-no" "oe-rel" "r-no"}

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
  {src/adm/template/snd-list.i "oe-ordl"}
  {src/adm/template/snd-list.i "oe-rel"}
  {src/adm/template/snd-list.i "tt-report"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-buttons B-table-Win 
PROCEDURE set-buttons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
        
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN 
    RUN set-buttons IN WIDGET-HANDLE(char-hdl) ("initial").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE unpost-item B-table-Win 
PROCEDURE unpost-item :
/*------------------------------------------------------------------------------
  Purpose:    from oe/oerelunp.p 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-oe-rel FOR oe-rel.
DEF BUFFER b-oe-ordl FOR oe-ordl.
    
DEF VAR choice AS LOG NO-UNDO.


IF AVAIL oe-rel AND oe-rel.link-no EQ 0 THEN DO:
  MESSAGE "Cannot unpost planned releases, posted only..."
          VIEW-AS ALERT-BOX ERROR.
  RETURN.
END.

FIND FIRST oe-ord
    WHERE oe-ord.company EQ oe-rel.company 
      AND oe-ord.ord-no  EQ oe-rel.ord-no
    NO-LOCK NO-ERROR.

{oe/rel-stat.i lv-stat}

RELEASE oe-boll.

IF AVAIL oe-rell AND lv-stat EQ "P" THEN
FIND FIRST oe-boll
    WHERE oe-boll.company  EQ cocode
      AND oe-boll.ord-no   EQ oe-rell.ord-no
      AND oe-boll.line     EQ oe-rell.line
      AND oe-boll.i-no     EQ oe-rell.i-no
      AND oe-boll.r-no     EQ oe-rell.r-no
      AND oe-boll.rel-no   EQ oe-rell.rel-no
      AND oe-boll.b-ord-no EQ oe-rell.b-ord-no
    NO-LOCK NO-ERROR.

IF INDEX("CZ",lv-stat) NE 0 THEN 
  MESSAGE "Cannot unpost, this release has been Invoiced..."
     VIEW-AS ALERT-BOX ERROR.
ELSE
IF AVAIL oe-boll THEN
  MESSAGE "Sorry, first you must delete BOL: " +
          TRIM(STRING(oe-boll.bol-no,">>>>>>>>>>")) + "..."
     VIEW-AS ALERT-BOX ERROR.

ELSE DO:
  IF NOT choice THEN 
    MESSAGE "Warning, this will erase the actual release flag."
            "This will cause the item to appear as unreleased!"
            SKIP "Do you want to continue? "
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice .

  IF choice THEN DO:
    /* Added to remove release qty from order line total release */
    FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl).

    FIND b-oe-rel WHERE ROWID(b-oe-rel) EQ ROWID(oe-rel).

    b-oe-ordl.t-rel-qty = b-oe-ordl.t-rel-qty - oe-rel.qty.

    FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rel.link-no NO-ERROR.

    IF AVAIL oe-relh THEN DO:
      FOR EACH oe-rell
          WHERE oe-rell.company EQ oe-rel.company
            AND oe-rell.r-no    EQ oe-rel.link-no
            AND oe-rell.i-no    EQ oe-rel.i-no
            AND oe-rell.line    EQ oe-rel.line
            AND oe-rell.link-no EQ oe-rel.r-no:
        DELETE oe-rell.
      END.
      FIND FIRST oe-rell
          WHERE oe-rell.company EQ oe-relh.company
            AND oe-rell.r-no    EQ oe-relh.r-no
          USE-INDEX r-no NO-LOCK NO-ERROR.
      IF NOT AVAIL oe-rell THEN DO:
        oe-relh.posted = NO.
        DELETE oe-relh.
      END.
    END.

    b-oe-rel.link-no = 0.

    FIND CURRENT b-oe-ordl NO-LOCK.

    RUN reopen-query.

    RUN repo-query (ROWID(oe-rel)).
  END.  /* choice */
END.  /* else */

ll-unposted = choice.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-dates B-table-Win 
PROCEDURE update-dates :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-date AS DATE NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.

  DEF BUFFER b-oe-ordl FOR oe-ordl.


  lv-date = DATE(INT(SUBSTR(tt-report.key-02,1,2)),
                 INT(SUBSTR(tt-report.key-02,3,2)),
                 INT(SUBSTR(tt-report.key-02,5,4))).

  {oe/rel-stat.i lv-stat}

  IF INDEX("AB",lv-stat) GT 0 THEN DO:
    IF AVAIL oe-relh THEN DO TRANSACTION:
      FIND CURRENT oe-relh.
      oe-relh.rel-date = lv-date.
      FIND CURRENT oe-relh NO-LOCK.
    END.

    IF AVAIL oe-relh THEN RUN oe/d-dudate.w (ROWID(oe-relh)).
  END.

  ELSE
  IF INDEX("SLI",lv-stat) GT 0 THEN DO:
    ll = NO.
    IF AVAIL oe-ordl AND oe-ordl.req-date GT lv-date THEN
      MESSAGE "Change order line item due date to " + TRIM(STRING(lv-date)) "?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll.
    IF ll THEN DO:
      DISABLE TRIGGERS FOR LOAD OF b-oe-ordl.
      FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl).
      b-oe-ordl.req-date = lv-date.
      FIND CURRENT b-oe-ordl NO-LOCK NO-ERROR.
    END.
    ll = NO.
    FIND FIRST oe-ord OF oe-rel NO-LOCK NO-ERROR.
    IF AVAIL oe-ord AND oe-ord.due-date GT lv-date THEN
      MESSAGE "Change order header due date to " + TRIM(STRING(lv-date)) "?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll.
    IF ll THEN DO:
      FIND CURRENT oe-ord NO-ERROR.
      oe-ord.due-date = lv-date.
      FIND CURRENT oe-ord NO-LOCK NO-ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-key-02 B-table-Win 
PROCEDURE valid-key-02 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    ld-date = DATE(INT(SUBSTR(tt-report.key-02:SCREEN-VALUE IN BROWSE {&browse-name},1,2)),
                   INT(SUBSTR(tt-report.key-02:SCREEN-VALUE IN BROWSE {&browse-name},4,2)),
                   INT(SUBSTR(tt-report.key-02:SCREEN-VALUE IN BROWSE {&browse-name},7,4))) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE ERROR-STATUS:GET-MESSAGE(1) VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO tt-report.key-02 IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.

    ELSE tt-report.key-02:SCREEN-VALUE IN BROWSE {&browse-name} =
             STRING(ld-date,tt-report.key-02:FORMAT IN BROWSE {&browse-name}).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-po-no B-table-Win 
PROCEDURE valid-po-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER cust-po-mand FOR reftable.

  
  DO WITH FRAME {&FRAME-NAME}:
    RELEASE cust.

    FIND FIRST oe-ord NO-LOCK
        WHERE oe-ord.company EQ oe-rel.company
          AND oe-ord.ord-no  EQ oe-rel.ord-no
        NO-ERROR.

    IF AVAIL oe-ord THEN
    FIND FIRST cust NO-LOCK
        WHERE cust.company EQ oe-ord.company
          AND cust.cust-no EQ oe-ord.cust-no
          AND CAN-FIND(FIRST cust-po-mand
                       WHERE cust-po-mand.reftable EQ "cust.po-mand"
                         AND cust-po-mand.company  EQ cust.company
                         AND cust-po-mand.loc      EQ ""
                         AND cust-po-mand.code     EQ cust.cust-no
                         AND cust-po-mand.val[1]   EQ 1)
        NO-ERROR.
    
    IF AVAIL cust AND TRIM(tt-report.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ "" THEN DO:
      MESSAGE "PO# is mandatory for this Customer..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO tt-report.po-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-s-code B-table-Win 
PROCEDURE valid-s-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
    IF LOOKUP(tt-report.s-code:SCREEN-VALUE IN BROWSE {&browse-name},lv-s-codes) LE 0 THEN DO:
      MESSAGE "Invalid " + TRIM(tt-report.s-code:LABEL IN BROWSE {&browse-name}) +
              ", try help..." VIEW-AS ALERT-BOX.
      APPLY "entry" TO tt-report.s-code IN BROWSE {&browse-name}.
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
    FIND FIRST oe-ord
        WHERE oe-ord.company EQ oe-rel.company 
          AND oe-ord.ord-no  EQ oe-rel.ord-no
        NO-LOCK.

    RUN oe/custxship.p (oe-rel.company,
                        oe-ord.cust-no,
                        oe-rel.ship-id:SCREEN-VALUE IN BROWSE {&browse-name},
                        BUFFER shipto).

    IF AVAIL shipto THEN li-ship-no = shipto.ship-no.

    ELSE DO:
      MESSAGE "Invalid " + TRIM(oe-rel.ship-id:LABEL IN BROWSE {&browse-name}) +
              ", try help..." VIEW-AS ALERT-BOX.
      APPLY "entry" TO oe-rel.ship-id IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-rel-qty B-table-Win 
FUNCTION get-rel-qty RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF NOT AVAIL oe-rel THEN DO:
    FIND oe-rel WHERE RECID(oe-rel) EQ lv-rel-recid NO-LOCK NO-ERROR.
    IF AVAIL oe-rel THEN
      FIND FIRST tt-report WHERE tt-report.rec-id EQ RECID(oe-rel) NO-ERROR.
  END.

  RETURN IF /*(NOT oereleas-log AND INDEX("AB",get-rel-stat()) GT 0) OR*/
            INDEX("SIL",get-rel-stat()) GT 0                       THEN 0
         ELSE
         IF AVAIL tt-report                 AND
            INDEX("AB",get-rel-stat()) GT 0 THEN tt-report.qty
         ELSE
         IF AVAIL oe-rel THEN oe-rel.qty
         ELSE INT(oe-rel.qty:SCREEN-VALUE IN BROWSE {&browse-name}).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-rel-stat B-table-Win 
FUNCTION get-rel-stat RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF NOT AVAIL oe-rel THEN
  FIND oe-rel WHERE RECID(oe-rel) EQ lv-rel-recid NO-LOCK NO-ERROR.

  RUN oe/rel-stat.p (IF AVAIL oe-rel THEN ROWID(oe-rel) ELSE ?, OUTPUT lv-stat).

  RETURN lv-stat.
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-tot-qty B-table-Win 
FUNCTION get-tot-qty RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF NOT AVAIL oe-rel THEN DO:
    FIND oe-rel WHERE RECID(oe-rel) EQ lv-rel-recid NO-LOCK NO-ERROR.
    IF AVAIL oe-rel THEN
      FIND FIRST tt-report WHERE tt-report.rec-id EQ RECID(oe-rel) NO-ERROR.
  END.

  RETURN IF NOT oereleas-log                AND
            AVAIL tt-report                 AND
            INDEX("AB",get-rel-stat()) GT 0 THEN tt-report.qty
         ELSE
         IF AVAIL oe-rel THEN oe-rel.tot-qty
         ELSE INT(oe-rel.tot-qty:SCREEN-VALUE IN BROWSE {&browse-name}).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

