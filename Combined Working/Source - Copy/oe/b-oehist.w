&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  

  Description: from BROWSER.W - Basic SmartBrowser Object Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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

def var v-cost as CHAR no-undo.
def var v-pric as dec no-undo.
DEF VAR rec_key AS INT NO-UNDO.
DEF VAR lv-first-time AS LOG INIT YES NO-UNDO.
DEF VAR lv-ord-recid AS RECID NO-UNDO.
DEF VAR ll-mouse-click AS LOG NO-UNDO.
DEF BUFFER bf-ordl FOR oe-ordl.
DEF BUFFER bf-ord FOR oe-ord.
DEF VAR v-int AS INT NO-UNDO.
DEF VAR lv-search-by AS CHAR INIT "Item" NO-UNDO.

DEF TEMP-TABLE tt-ordl FIELD tt-recid AS RECID
                       FIELD ord-no LIKE oe-ordl.ord-no
                       FIELD i-no LIKE oe-ordl.i-no
                       FIELD ord-date LIKE oe-ord.ord-date
                       FIELD IS-SELECTED AS LOG COLUMN-LABEL "S" VIEW-AS TOGGLE-BOX
                       FIELD e-qty LIKE oe-ordl.qty
                       INDEX i1 tt-recid.
                       

DEF VAR historyQty AS DECIMAL NO-UNDO.
DEF VAR historyPrice LIKE oe-ordl.price NO-UNDO.
DEF VAR historyPrUOM LIKE oe-ordl.pr-uom NO-UNDO.
DEF VAR setFromHistory AS LOGICAL NO-UNDO.

/*{methods/prgsecur.i}*/
DEFINE VARIABLE v-prgmname LIKE prgrms.prgmname NO-UNDO.
DEFINE VARIABLE period_pos AS INTEGER NO-UNDO.

ASSIGN
  period_pos = INDEX(PROGRAM-NAME(1),".")
  v-prgmname = SUBSTR(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"/",period_pos - 9) + 1)
  v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

{custom/gcompany.i}
{custom/globdefs.i}
{sys/inc/var.i NEW SHARED}
{sys/inc/varasgn.i}

DEF VAR cellColumncolor AS HANDLE NO-UNDO EXTENT 20.
DEF VAR columnCount AS INTEGER NO-UNDO.
DEF VAR idx AS INTEGER NO-UNDO.
DEF VAR useColors AS CHAR NO-UNDO.
/* DEF VAR gcompany AS CHAR NO-UNDO. */
DEF VAR v-hide-cost AS LOG NO-UNDO.

DEF VAR lv-sort-by AS CHAR  NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR  NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.
/*&SCOPED-DEFINE SORTBY-PHRASE  BY oe-ordl.ord-no*/
gcompany = g_company.
{sys/inc/fgbrowse.i}
useColors = sys-ctrl.char-fld.

DO TRANSACTION:
   {sys/inc/fgsecur.i}
END.

IF fgsecurity-log THEN
DO:
   FIND FIRST usergrps WHERE
        usergrps.usergrps = fgsecurity-char
        NO-LOCK NO-ERROR.

   IF AVAIL usergrps AND
      (NOT CAN-DO(usergrps.users,USERID("NOSWEAT")) AND
       TRIM(usergrps.users) NE "*") THEN
      ASSIGN
         v-hide-cost = YES.
END.

&SCOPED-DEFINE sortby sortby
&SCOPED-DEFINE sortby-log                                                                                                                                  ~
    IF lv-sort-by EQ "order#"    THEN STRING(oe-ordl.ord-no,"9999999999")                                                                              ELSE ~
    IF lv-sort-by EQ "is-selected" THEN string(tt-ordl.IS-SELECTED)                                                                                  ELSE ~
    IF lv-sort-by EQ "qty"       THEN string(tt-ordl.e-qty,"-9999999999.9999999999")                                                                     ELSE ~
    IF lv-sort-by EQ "item"       THEN string(oe-ordl.i-no,"x(15)")                                                                     ELSE ~
    IF lv-sort-by EQ "item description"       THEN string(oe-ordl.part-dscr1,"x(20)")                                                                     ELSE ~
        ""

&SCOPED-DEFINE sortby BY oe-ordl.i-no

&SCOPED-DEFINE sortby-phrase-asc  ~
    BY ({&sortby-log})            ~
    {&sortby}

&SCOPED-DEFINE sortby-phrase-desc  ~
    BY ({&sortby-log}) DESC        ~
    {&sortby}

&SCOPED-DEFINE yellowColumnsName oe-hist
&SCOPED-DEFINE cellColumnDat browsers-oe-hist
&SCOPED-DEFINE autoFind
&SCOPED-DEFINE winReSize
&SCOPED-DEFINE sizeOption HEIGHT

{methods/defines/winReSize.i}

DEF VAR v-called-setCellColumns AS LOG NO-UNDO.
DEF VAR v-col-move AS LOG NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES cust
&Scoped-define FIRST-EXTERNAL-TABLE cust


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR cust.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-ordl oe-ordl

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table tt-ordl.IS-SELECTED oe-ordl.ord-no tt-ordl.e-qty oe-ordl.i-no tt-ordl.ord-date oe-ordl.i-name oe-ordl.part-dscr1 f-cost() @ v-cost f-price() @ v-pric oe-ordl.pr-uom oe-ordl.qty   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table tt-ordl.e-qty tt-ordl.IS-SELECTED   
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table tt-ordl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table tt-ordl
&Scoped-define SELF-NAME br_table
&Scoped-define OPEN-QUERY-br_table lv-sort-by = /*"Item#"*/ lv-sort-by-lab .  DO WITH FRAME f-main:     IF index(fi_sortby:SCREEN-VALUE, ~
       "Order#") GT 0  THEN         lv-sort-by = "Order#".     ELSE IF index(fi_sortby:SCREEN-VALUE, ~
       "Qty") GT 0  THEN         lv-sort-by = "Qty".     ELSE IF index(fi_sortby:SCREEN-VALUE, ~
       "Item") GT 0  THEN         lv-sort-by = "Item".     ELSE IF index(fi_sortby:SCREEN-VALUE, ~
       "Item Description") GT 0  THEN         lv-sort-by = "Item Description".     ELSE IF index(fi_sortby:SCREEN-VALUE, ~
       "s") GT 0  THEN         lv-sort-by = "is-selected". END.   IF lv-search-by = "Order" THEN DO:     v-int = INTEGER(lv-search) NO-ERROR.     IF ERROR-STATUS:ERROR THEN         lv-search-by = "Item". END.   IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-ordl, ~
       ~       EACH oe-ordl NO-LOCK WHERE recid(oe-ordl) EQ tt-ordl.tt-recid ~                              AND (IF lv-search-by = "Item" THEN ~                                     oe-ordl.i-no BEGINS lv-search ~                                   ELSE ~                                     (oe-ordl.ord-no EQ INTEGER(lv-search) OR lv-search = "" )) ~                            {&sortby-phrase-asc}. ELSE     OPEN QUERY {&SELF-NAME} FOR EACH tt-ordl, ~
       ~       EACH oe-ordl NO-LOCK WHERE recid(oe-ordl) EQ tt-ordl.tt-recid ~                              AND (IF lv-search-by = "Item" THEN ~                                     oe-ordl.i-no BEGINS lv-search ~                                   ELSE ~                                     (oe-ordl.ord-no EQ INTEGER(lv-search) OR lv-search = "")) ~                            {&sortby-phrase-desc}.
&Scoped-define TABLES-IN-QUERY-br_table tt-ordl oe-ordl
&Scoped-define FIRST-TABLE-IN-QUERY-br_table tt-ordl
&Scoped-define SECOND-TABLE-IN-QUERY-br_table oe-ordl


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table browse-order btn_clear_find Btn_move-sort ~
auto_find rsSearch lv-search btn-reset btn_detail fi_sortby 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find rsSearch lv-search ~
fi_sortby 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD f-cost B-table-Win 
FUNCTION f-cost RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD f-price B-table-Win 
FUNCTION f-price RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-reset 
     LABEL "Clear Search" 
     SIZE 15 BY 1.1.

DEFINE BUTTON btn_clear_find 
     LABEL "Clear" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn_detail 
     LABEL "&Detail" 
     SIZE 22 BY 1.1.

DEFINE BUTTON Btn_move-sort 
     LABEL "Move Columns" 
     SIZE 16 BY 1.1
     BGCOLOR 14  .

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     LABEL "Find" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sort" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

/*DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1
     BGCOLOR 15  NO-UNDO. */

DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Selected", 1,
"Order", 2,
"Qty", 3
     SIZE 12 BY 3 NO-UNDO.  

DEFINE VARIABLE rsSearch AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Item", "Item",
"Order", "Order"
     SIZE 25 BY .95 NO-UNDO. 

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 155 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      tt-ordl, 
      oe-ordl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      tt-ordl.IS-SELECTED COLUMN-LABEL 'S' LABEL-BGCOLOR 14 VIEW-AS TOGGLE-BOX 
      oe-ordl.ord-no COLUMN-LABEL 'Order#' LABEL-BGCOLOR 14
      tt-ordl.e-qty COLUMN-LABEL "Qty" LABEL-BGCOLOR 14
      oe-ordl.i-no FORMAT "x(15)":U LABEL-BGCOLOR 14
      tt-ordl.ord-date COLUMN-LABEL "Sale Date" FORMAT "99/99/9999":U
      oe-ordl.i-name FORMAT "x(30)":U
      oe-ordl.part-dscr1 FORMAT "x(20)":U 
      f-cost() @ v-cost COLUMN-LABEL "           Cost" FORMAT "X(15)"
      f-price() @ v-pric COLUMN-LABEL "Sell"
      oe-ordl.pr-uom COLUMN-LABEL "UOM" FORMAT "x(4)":U
      oe-ordl.qty FORMAT "->>,>>>,>>9.9<<":U
      ENABLE tt-ordl.e-qty tt-ordl.IS-SELECTED
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 155 BY 16.43
         FONT 0.


/* ************************  Frame Definitions  *********************** */

/*DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
     browse-order AT ROW 7.91 COL 17 NO-LABEL WIDGET-ID 4 
     btn_clear_find AT ROW 8.62 COL 16 WIDGET-ID 10
     auto_find AT ROW 9.33 COL 14 COLON-ALIGNED WIDGET-ID 8
     Btn_move-sort AT ROW 17.67 COL 10 WIDGET-ID 4
     rsSearch AT ROW 17.67 COL 3 NO-LABEL WIDGET-ID 12 
     lv-search AT ROW 17.67 COL 35 COLON-ALIGNED
     btn-reset AT ROW 17.67 COL 83
     btn_detail AT ROW 17.67 COL 100
     fi_sortby AT ROW 17.67 COL 130 COLON-ALIGNED WIDGET-ID 2
     RECT-5 AT ROW 17.43 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 . */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
     browse-order AT ROW 7.91 COL 17 NO-LABEL WIDGET-ID 4
     btn_clear_find AT ROW 8.62 COL 16 WIDGET-ID 10
     auto_find AT ROW 9.33 COL 14 COLON-ALIGNED WIDGET-ID 8
     rsSearch AT ROW 17.67 COL 3 NO-LABEL WIDGET-ID 12
     lv-search AT ROW 17.67 COL 35 COLON-ALIGNED
     btn-reset AT ROW 17.67 COL 67
     Btn_move-sort AT ROW 17.67 COL 83 WIDGET-ID 4
     btn_detail AT ROW 17.67 COL 100
     fi_sortby AT ROW 17.67 COL 130 COLON-ALIGNED WIDGET-ID 2
     RECT-5 AT ROW 17.43 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: ASI.cust
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
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
         HEIGHT             = 18.1
         WIDTH              = 155.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
{custom/yellowcolumns.i}

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
       auto_find:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       browse-order:HIDDEN IN FRAME F-Main           = TRUE. 

ASSIGN 
       br_table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

ASSIGN 
       btn_clear_find:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-5 IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _START_FREEFORM
lv-sort-by = "Order#".

DO WITH FRAME f-main:
    IF index(fi_sortby:SCREEN-VALUE, "Order#") GT 0  THEN
        lv-sort-by = "Order#".
    ELSE IF index(fi_sortby:SCREEN-VALUE, "Qty") GT 0  THEN
        lv-sort-by = "Qty".
    ELSE IF index(fi_sortby:SCREEN-VALUE, "s") GT 0  THEN
        lv-sort-by = "is-selected".
END.


IF lv-search-by = "Order" THEN DO:
    v-int = INTEGER(lv-search) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        lv-search-by = "Item".
END.


IF ll-sort-asc THEN
OPEN QUERY {&SELF-NAME} FOR EACH tt-ordl, ~
      EACH oe-ordl NO-LOCK WHERE recid(oe-ordl) EQ tt-ordl.tt-recid ~
                             AND (IF lv-search-by = "Item" THEN ~
                                    oe-ordl.i-no BEGINS lv-search ~
                                  ELSE ~
                                    oe-ordl.ord-no EQ INTEGER(lv-search)) ~
                           {&sortby-phrase-asc}.
ELSE
    OPEN QUERY {&SELF-NAME} FOR EACH tt-ordl, ~
      EACH oe-ordl NO-LOCK WHERE recid(oe-ordl) EQ tt-ordl.tt-recid ~
                             AND (IF lv-search-by = "Item" THEN ~
                                    oe-ordl.i-no BEGINS lv-search ~
                                  ELSE ~
                                    oe-ordl.ord-no EQ INTEGER(lv-search)) ~
                           {&sortby-phrase-desc}.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE"
     _TblOptList       = ","
     _OrdList          = "ASI.oe-ordl.i-no|yes"
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
ON ANY-PRINTABLE OF br_table IN FRAME F-Main
DO:
   if lv-first-time then assign lv-search = ""
                                lv-first-time = no.
   IF LASTKEY NE 8 THEN
   lv-search = lv-search + KEYLABEL(LASTKEY).
   DISPLAY lv-search WITH FRAME {&FRAME-NAME}.
   APPLY "leave" TO lv-search.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON BACKSPACE OF br_table IN FRAME F-Main
DO:
  lv-search = SUBSTR(lv-search,1,LENGTH(lv-search) - 1).
  APPLY "ANY-PRINTABLE" TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON DEFAULT-ACTION OF br_table IN FRAME F-Main
or return of self
DO:
  APPLY 'CHOOSE' TO btn_detail.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON MOUSE-SELECT-CLICK OF br_table IN FRAME F-Main
DO:
  ll-mouse-click = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-DISPLAY OF br_table IN FRAME F-Main
DO:

  IF useColors NE '' THEN
  DO idx = 1 TO columnCount:
    CASE useColors:
      WHEN 'Bronze/Maroon' THEN DO:
        FIND FIRST itemfg OF oe-ordl NO-LOCK NO-ERROR.
        IF AVAILABLE itemfg THEN
        ASSIGN
          cellColumncolor[idx]:FGCOLOR = IF itemfg.stocked THEN 15 ELSE ?
          cellColumncolor[idx]:BGCOLOR = IF itemfg.stocked THEN
                                      IF itemfg.q-onh GT 0 THEN 6 ELSE 4
                                    ELSE ?.
      END.
      OTHERWISE
      ASSIGN
        cellColumncolor[idx]:FGCOLOR = ?
        cellColumncolor[idx]:BGCOLOR = ?.
    END CASE.
  END. /* do idx */
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
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON START-SEARCH OF br_table IN FRAME F-Main
DO:
  DEF VAR sortDisplay AS CHAR.
  /* User clicked on column heading so don't change selection */
  ASSIGN fi_sortby.
  ASSIGN 
    ll-mouse-click = NO
    ll-sort-asc = NOT ll-sort-asc /* sortby */
    sortby = ll-sort-asc.

  IF rsSearch:SCREEN-VALUE = "Order" AND lv-search:SCREEN-VALUE NE "" THEN DO:
     RUN add-order-records.
  END.
  ELSE
      IF rsSearch:SCREEN-VALUE = "Item" THEN DO:
         RUN remove-dup-records.
      END.

  RUN startSearch.
  sortDisplay = TRIM(sortColumn + ' - ' + STRING(ll-sort-asc,'Ascending/Descending')).
  fi_sortBy:SCREEN-VALUE IN FRAME {&FRAME-NAME} = sortDisplay.

  /* ll-sort-asc = sortby. */


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:  
   DEF VAR li AS INT NO-UNDO.
   DEF VAR ll-refresh AS LOG NO-UNDO.


   ll-refresh = NO.
   DO WITH FRAME {&FRAME-NAME}:
       IF AVAIL tt-ordl THEN
      ASSIGN tt-ordl.e-qty = INTEGER(tt-ordl.e-qty:SCREEN-VALUE IN BROWSE br_table)
             tt-ordl.IS-SELECTED = IF tt-ordl.IS-SELECTED:SCREEN-VALUE IN BROWSE br_table = "YES" THEN YES
                                   ELSE NO.
      IF AVAIL tt-ordl THEN
      IF tt-ordl.e-qty:MODIFIED AND tt-ordl.IS-SELECTED = NO THEN
          ASSIGN tt-ordl.IS-SELECTED = YES ll-refresh = TRUE.
     
   END.


   IF ll-mouse-click THEN DO:
       IF BROWSE br_table:FOCUSED-ROW-SELECTED = YES AND tt-ordl.IS-SELECTED THEN DO:   
           ASSIGN tt-ordl.IS-SELECTED = NO ll-refresh = TRUE.
       END.
       ELSE
       IF BROWSE br_table:FOCUSED-ROW-SELECTED = YES AND tt-ordl.IS-SELECTED = NO THEN DO:   
           ASSIGN tt-ordl.IS-SELECTED = YES ll-refresh = TRUE.
       END.
   END.
   
   ll-mouse-click = NO.


  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}



  IF ll-refresh THEN
       BROWSE br_table:REFRESH().



END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-reset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-reset B-table-Win
ON CHOOSE OF btn-reset IN FRAME F-Main /* Clear Search */
DO:
   lv-search = "".
   DISPLAY lv-search WITH FRAME {&FRAME-NAME}.
   APPLY "leave" TO lv-search.
   APPLY "entry" TO BROWSE {&browse-name}.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_move-sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_move-sort B-table-Win
ON CHOOSE OF Btn_move-sort IN FRAME F-Main /* Move Columns */
DO:
  RUN move-columns.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_clear_find
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_clear_find B-table-Win
ON CHOOSE OF btn_clear_find IN FRAME F-Main /* Clear */
DO:
  RUN startSearch.
APPLY 'value-changed' TO BROWSE {&browse-name}.
MESSAGE "{&SORTBY-PHRASE} "
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  RUN local-open-query.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_detail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_detail B-table-Win
ON CHOOSE OF btn_detail IN FRAME F-Main /* Detail */
DO:
  run oe/d-hist2.w (oe-ordl.i-no,oe-ord.cust-no,THIS-PROCEDURE).
  IF setFromHistory THEN RUN closeHistory.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-search B-table-Win
ON LEAVE OF lv-search IN FRAME F-Main /* Search */
DO:

  ASSIGN {&SELF-NAME}.

  IF rsSearch:SCREEN-VALUE = "Order" AND lv-search:SCREEN-VALUE NE "" THEN DO:
     RUN add-order-records.
  END.
  ELSE
      IF rsSearch:SCREEN-VALUE = "Item" THEN DO:
         RUN remove-dup-records.
      END.
  {&OPEN-QUERY-{&BROWSE-NAME}}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-search B-table-Win
ON RETURN OF lv-search IN FRAME F-Main /* Search */
DO:
  APPLY 'LEAVE' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsSearch B-table-Win
ON VALUE-CHANGED OF rsSearch IN FRAME F-Main
DO:
    DEF VAR sortDisplay AS CHAR.
  ASSIGN {&SELF}.
  lv-search-by  = rsSearch:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
  IF rsSearch:SCREEN-VALUE = "Order" THEN
      ASSIGN
      lv-sort-by   = "order#" 
      sortColumn   = "Order#" .
  ELSE
      ASSIGN
          lv-sort-by  = "Item" 
          sortColumn  = "Item#" .
      
  /* User clicked on column heading so don't change selection */
  ASSIGN fi_sortby.
  ASSIGN 
    ll-mouse-click = NO
    ll-sort-asc = NOT ll-sort-asc /* sortby */
    sortby = ll-sort-asc.

  IF rsSearch:SCREEN-VALUE = "Order" AND lv-search:SCREEN-VALUE NE "" THEN DO:
     RUN add-order-records.
  END.
  ELSE
      IF rsSearch:SCREEN-VALUE = "Item" THEN DO:
         RUN remove-dup-records.
      END.

   RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

 /*RUN startSearch.*/
  sortDisplay = TRIM(sortColumn + ' - ' + STRING(ll-sort-asc,'Ascending/Descending')).
  fi_sortBy:SCREEN-VALUE IN FRAME {&FRAME-NAME} = sortDisplay.
  APPLY 'LEAVE' TO lv-search.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME 


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

RUN build-table.
IF columnCount EQ 0 THEN RUN getCellColumns.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

&SCOPED-DEFINE cellColumnDat browsers-oe-hist

{methods/browsers/setCellColumns.i}
DO WITH FRAME f-main:
    {custom/usrprint.i}

  IF rsSearch:SCREEN-VALUE = "Order" THEN
      ASSIGN
      fi_sortby:SCREEN-VALUE = "Order# - Descending"
      lv-sort-by-lab = "Order#" 
      sortColumn  = "Order#"
      lv-sort-by = "Order#" .
  
  ELSE
      ASSIGN
          fi_sortby:SCREEN-VALUE = "Item# - Descending"
          lv-sort-by-lab = "Item#"
          sortColumn  = "Item#" 
          lv-sort-by = "Item".
      sortby = YES.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-order-records B-table-Win 
PROCEDURE add-order-records :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH ASI.oe-ord OF ASI.cust WHERE oe-ord.ord-no = integer(lv-search:SCREEN-VALUE IN FRAME {&FRAME-NAME}) 
           NO-LOCK,
       EACH ASI.oe-ordl OF ASI.oe-ord NO-LOCK
       WHERE ASI.oe-ordl.qty NE 0
       BREAK BY ASI.oe-ordl.i-no BY oe-ord.ord-date BY oe-ord.ord-no:

      DO:
         FIND FIRST tt-ordl WHERE tt-ordl.tt-recid = RECID(oe-ordl)
              NO-ERROR.
         IF NOT AVAIL tt-ordl THEN DO:
             CREATE tt-ordl.
             ASSIGN tt-ordl.tt-recid   = RECID(oe-ordl)
                    tt-ordl.ord-no     = oe-ordl.ord-no
                    tt-ordl.i-no       = oe-ordl.i-no
                    tt-ordl.ord-date   = oe-ord.ord-date
                    tt-ordl.e-qty      = oe-ordl.qty.
         END.
      END.
  END.
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
  {src/adm/template/row-list.i "cust"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "cust"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table B-table-Win 
PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH ASI.oe-ord WHERE OF ASI.cust NO-LOCK,
       EACH ASI.oe-ordl OF ASI.oe-ord NO-LOCK
       WHERE ASI.oe-ordl.qty NE 0
       BREAK BY ASI.oe-ordl.i-no BY oe-ord.ord-date BY oe-ord.ord-no:

      IF LAST-OF(oe-ordl.i-no) THEN DO:
         FIND FIRST tt-ordl WHERE tt-ordl.tt-recid = RECID(oe-ordl)
              NO-ERROR.
         IF NOT AVAIL tt-ordl THEN DO:
             CREATE tt-ordl.
             ASSIGN tt-ordl.tt-recid   = RECID(oe-ordl)
                    tt-ordl.ord-no     = oe-ordl.ord-no
                    tt-ordl.i-no       = oe-ordl.i-no
                    tt-ordl.ord-date   = oe-ord.ord-date
                    tt-ordl.e-qty      = oe-ordl.qty.
         END.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE closeHistory B-table-Win 
PROCEDURE closeHistory :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS CHARACTER NO-UNDO.

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
  RUN closeHistory IN WIDGET-HANDLE(char-hdl) NO-ERROR.
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


  IF NOT AVAIL oe-bolh OR oe-bolh.posted THEN
    io-panel-state = "disable-all".

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-row-id B-table-Win 
PROCEDURE get-row-id :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-rowid-list AS CHAR NO-UNDO.
  DEF OUTPUT PARAM op-qty-list AS CHAR NO-UNDO.
  DEF VAR li AS INT NO-UNDO.              
  APPLY 'row-leave' TO BROWSE br_table.
  RELEASE oe-ordl.
  DO WITH FRAME f-main:
      IF AVAIL tt-ordl THEN
      FIND FIRST oe-ordl WHERE RECID(oe-ordl) EQ tt-ordl.tt-recid NO-LOCK NO-ERROR.

      /* 07091210 - to implement multi-select */

/*       IF br_table:NUM-SELECTED-ROWS GT 0 THEN                                             */
/*         DO li = 1 TO br_table:NUM-SELECTED-ROWS:                                          */
/*             br_table:FETCH-SELECTED-ROW (li) NO-ERROR.                                    */
/*                                                                                           */
/*             IF AVAIL tt-ordl THEN                                                         */
/*             FIND FIRST oe-ordl WHERE RECID(oe-ordl) EQ tt-ordl.tt-recid NO-LOCK NO-ERROR. */
/*             {util/tmsg.i ""TEST"" tt-ordl.e-qty tt-ordl.IS-SELECTED}                      */
/*             IF AVAIL oe-ordl THEN                                                         */
/*             op-rowid-list = op-rowid-list + string(ROWID(oe-ordl)) + ",".                 */
/*         END.                                                                              */

      FOR EACH tt-ordl WHERE tt-ordl.IS-SELECTED:

          FIND FIRST oe-ordl WHERE RECID(oe-ordl) EQ tt-ordl.tt-recid NO-LOCK NO-ERROR.

          IF AVAIL oe-ordl THEN
            ASSIGN op-rowid-list = op-rowid-list + string(ROWID(oe-ordl)) + ","
                   op-qty-list = op-qty-list + string(tt-ordl.e-qty) + ",".
      END.


  END.
  ASSIGN op-rowid-list = TRIM(op-rowid-list, ",")
         op-qty-list   = TRIM(op-qty-list, ",").
  


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCellColumns B-table-Win 
PROCEDURE getCellColumns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  columnCount = {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}.
  DO idx = 1 TO columnCount:
    cellColumncolor[idx] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(idx).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getQtyPrice B-table-Win 
PROCEDURE getQtyPrice :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER opQty AS DECIMAL NO-UNDO.
  DEFINE OUTPUT PARAMETER opPrice LIKE oe-ordl.price NO-UNDO.
  DEFINE OUTPUT PARAMETER opPrUOM LIKE oe-ordl.pr-uom NO-UNDO.
  DEFINE OUTPUT PARAMETER opSetFromHistory AS LOGICAL NO-UNDO.

  ASSIGN
    opQty            = historyQty
    opPrice          = historyPrice
    opPrUOM          = historyPrUOM
    opSetFromHistory = setFromHistory.

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
  /*DO WITH FRAME f-main:
    IF rsSearch:SCREEN-VALUE = "Order" THEN
    fi_sortby:SCREEN-VALUE = "Order# - Descending".
    ELSE
    fi_sortby:SCREEN-VALUE = "Item# - Descending".
    sortby = YES.
  END. */
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  IF v-called-setCellColumns = NO THEN DO:
     RUN setCellColumns.
     v-called-setCellColumns = YES.
  END.

  /* Code placed here will execute AFTER standard behavior.    */
  RUN setQtyPrice (0, 0, "", NO).
  APPLY "entry" TO BROWSE {&browse-name}.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-ord-no AS INT NO-UNDO.
  DEF VAR lv-item-no AS cha NO-UNDO.
  DEF VAR char-hdl AS cha NO-UNDO.
 

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN build-table.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
  RUN get-item-no IN WIDGET-HANDLE(char-hdl) (OUTPUT lv-ord-no, OUTPUT lv-item-no).
 
  IF lv-item-no <> "" THEN DO WITH FRAME {&FRAME-NAME}:
     ASSIGN rsSearch .     
     IF lv-search-by EQ "ITEM" THEN
       FIND FIRST tt-ordl WHERE tt-ordl.i-no = lv-item-no NO-LOCK NO-ERROR.      
     ELSE DO:
       lv-ord-no = INTEGER(lv-item-no) NO-ERROR.
       IF NOT ERROR-STATUS:ERROR THEN DO:
         FIND FIRST tt-ordl WHERE tt-ordl.ord-no = lv-ord-no NO-LOCK NO-ERROR.
       END.
     END.

     IF AVAIL tt-ordl THEN REPOSITION {&browse-name} TO ROWID ROWID(tt-ordl) NO-ERROR.
  END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE remove-dup-records B-table-Win 
PROCEDURE remove-dup-records :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH tt-ordl 
      BREAK BY tt-ordl.i-no.
      IF FIRST-OF(tt-ordl.i-no) THEN
          NEXT.
      ELSE
          DELETE tt-ordl.
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
  {src/adm/template/snd-list.i "cust"}
  {src/adm/template/snd-list.i "tt-ordl"}
  {src/adm/template/snd-list.i "oe-ordl"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setQtyPrice B-table-Win 
PROCEDURE setQtyPrice :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipQty AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER ipPrice LIKE oe-ordl.price NO-UNDO.
  DEFINE INPUT PARAMETER ipPrUOM LIKE oe-ordl.pr-uom NO-UNDO.
  DEFINE INPUT PARAMETER ipSetFromHistory AS LOGICAL NO-UNDO.

  ASSIGN
    historyQty     = ipQty
    historyPrice   = ipPrice
    historyPrUOM   = ipPrUOM
    setFromHistory = ipSetFromHistory.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setQtyPriceFromRow B-table-Win 
PROCEDURE setQtyPriceFromRow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ip-oe-ordl-rowid AS ROWID.
DEF BUFFER bf-oe-ordl FOR oe-ordl.

FIND bf-oe-ordl WHERE ROWID(bf-oe-ordl) EQ ip-oe-ordl-rowid NO-LOCK NO-ERROR.
IF AVAIL bf-oe-ordl THEN
    FIND FIRST tt-ordl WHERE tt-ordl.tt-recid EQ RECID(bf-oe-ordl)
     NO-LOCK NO-ERROR.
IF AVAIL bf-oe-ordl AND AVAIL tt-ordl THEN
  RUN setQtyPrice (tt-ordl.e-qty, bf-oe-ordl.price, bf-oe-ordl.pr-uom, YES) NO-ERROR.

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION f-cost B-table-Win 
FUNCTION f-cost RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR f-cost AS CHAR NO-UNDO.

  IF v-hide-cost = NO THEN
     f-cost = STRING(oe-ordl.cost,"->>>,>>>,>>9.99").

  RETURN f-cost.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION f-price B-table-Win 
FUNCTION f-price RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var ld-price as dec no-undo.
  /* don't convert to uom ea
  if oe-ordl.pr-uom = "EA" then ld-price = oe-ordl.price.
  else run custom/convcuom.p (oe-ordl.company,oe-ordl.pr-uom, "EA",0,0,0,0, oe-ordl.price, output ld-price).
  */
  ld-price = oe-ordl.price.
  return ld-price.
END FUNCTION.

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
      {&BROWSE-NAME}:COLUMN-MOVABLE = NOT v-col-move
         {&BROWSE-NAME}:COLUMN-RESIZABLE = NOT v-col-move
        v-col-move = NOT v-col-move .
      Btn_move-sort:LABEL = IF NOT v-col-move THEN "Move Columns" ELSE "Sort Columns".
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

