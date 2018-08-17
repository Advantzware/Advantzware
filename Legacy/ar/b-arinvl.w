&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  browsers/<table>.w

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

&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
DEF VAR lv-uom-list AS cha INIT "EA,MSF,M" NO-UNDO.
&IF DEFINED(UIB_is_Running) <> 0 &THEN
    &SCOPED-DEFINE NEW NEW GLOBAL
&ENDIF

DEF {&NEW} SHARED VAR g_lookup-var AS cha NO-UNDO.
{sys/inc/VAR.i "new shared" }
ASSIGN cocode = g_company
       locode = g_loc.
      
{oe/oe-sysct1.i NEW}

DEF VAR ll-inquiry AS LOG NO-UNDO.

&scoped-define SETVALUE NO

DEF VAR v-actdscr LIKE account.dscr NO-UNDO.

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

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES ar-inv
&Scoped-define FIRST-EXTERNAL-TABLE ar-inv


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ar-inv.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ar-invl

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table ar-invl.line ar-invl.actnum ~
get-actdscr() @ v-actdscr ar-invl.i-no ar-invl.part-no ar-invl.i-name ~
get-i-dscr () @ ar-invl.i-dscr ar-invl.i-dscr ar-invl.lot-no ~
ar-invl.inv-qty ar-invl.ship-qty ar-invl.cons-uom ar-invl.sf-sht ~
ar-invl.unit-pr ar-invl.pr-qty-uom ar-invl.disc calc-amt () @ ar-invl.amt ~
calc-amt () @ ar-invl.amt ar-invl.amt ar-invl.amt-msf ar-invl.cost ~
ar-invl.dscr[1] ar-invl.sman[1] ar-invl.s-pct[1] ar-invl.s-comm[1] ~
ar-invl.sman[2] ar-invl.s-pct[2] ar-invl.s-comm[2] ar-invl.sman[3] ~
ar-invl.s-pct[3] ar-invl.s-comm[3] ar-invl.bol-no ar-invl.ord-no ~
ar-invl.po-no 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table ar-invl.line ~
ar-invl.actnum ar-invl.i-no ar-invl.part-no ar-invl.i-name ar-invl.i-dscr ~
ar-invl.lot-no ar-invl.inv-qty ar-invl.ship-qty ar-invl.sf-sht ~
ar-invl.unit-pr ar-invl.pr-qty-uom ar-invl.cost ar-invl.dscr[1] ~
ar-invl.sman[1] ar-invl.s-pct[1] ar-invl.s-comm[1] ar-invl.sman[2] ~
ar-invl.s-pct[2] ar-invl.s-comm[2] ar-invl.sman[3] ar-invl.s-pct[3] ~
ar-invl.s-comm[3] ar-invl.bol-no ar-invl.ord-no ar-invl.po-no 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table ar-invl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table ar-invl
&Scoped-define QUERY-STRING-Browser-Table FOR EACH ar-invl WHERE ar-invl.x-no = ar-inv.x-no NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH ar-invl WHERE ar-invl.x-no = ar-inv.x-no NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table ar-invl
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table ar-invl


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 browse-order auto_find ~
Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calc-amt B-table-Win 
FUNCTION calc-amt RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-actdscr B-table-Win 
FUNCTION get-actdscr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-i-dscr B-table-Win 
FUNCTION get-i-dscr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     LABEL "Auto Find" 
     VIEW-AS FILL-IN 
     SIZE 60 BY 1 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 55 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      ar-invl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      ar-invl.line FORMAT "99":U
      ar-invl.actnum COLUMN-LABEL "Account Number" FORMAT "x(25)":U
      get-actdscr() @ v-actdscr COLUMN-LABEL "Account Description" FORMAT "x(45)":U
      ar-invl.i-no FORMAT "x(15)":U
      ar-invl.part-no FORMAT "x(15)":U
      ar-invl.i-name COLUMN-LABEL "Item Name" FORMAT "x(30)":U
      get-i-dscr () @ ar-invl.i-dscr
      ar-invl.i-dscr FORMAT "x(30)":U
      ar-invl.lot-no FORMAT "x(15)":U
      ar-invl.inv-qty COLUMN-LABEL "Invoice Qty" FORMAT "->>,>>>,>>9.9<":U
      ar-invl.ship-qty COLUMN-LABEL "Ship Quantity" FORMAT "->>,>>>,>>9.99":U
      ar-invl.cons-uom FORMAT "x(4)":U
      ar-invl.sf-sht COLUMN-LABEL "SqFt" FORMAT "->>,>>9.99<<":U
      ar-invl.unit-pr FORMAT "->>,>>>,>>9.99<<<<":U
      ar-invl.pr-qty-uom COLUMN-LABEL "UOM" FORMAT "x(4)":U
      ar-invl.disc COLUMN-LABEL "Dsct%" FORMAT ">>9.99":U WIDTH 8
      calc-amt () @ ar-invl.amt
      calc-amt () @ ar-invl.amt
      ar-invl.amt COLUMN-LABEL "Amount" FORMAT "->>,>>>,>>9.99":U
      ar-invl.amt-msf FORMAT "->>>>,>>9.99":U
      ar-invl.cost COLUMN-LABEL "Cost" FORMAT "->>>,>>>,>>9.99<<<<":U
      ar-invl.dscr[1] COLUMN-LABEL "Cost!UOM" FORMAT "x(4)":U
      ar-invl.sman[1] COLUMN-LABEL "SlsRep" FORMAT "x(3)":U
      ar-invl.s-pct[1] COLUMN-LABEL "% of Sale" FORMAT ">>9.99":U
      ar-invl.s-comm[1] COLUMN-LABEL "Comm%" FORMAT ">>9.99":U
      ar-invl.sman[2] COLUMN-LABEL "SlsRep" FORMAT "x(3)":U
      ar-invl.s-pct[2] COLUMN-LABEL "% of Sale" FORMAT ">>9.99":U
      ar-invl.s-comm[2] COLUMN-LABEL "Comm%" FORMAT ">>9.99":U
      ar-invl.sman[3] COLUMN-LABEL "SlsRep" FORMAT "x(3)":U
      ar-invl.s-pct[3] COLUMN-LABEL "% of Sale" FORMAT ">>9.99":U
      ar-invl.s-comm[3] COLUMN-LABEL "Comm%" FORMAT ">>9.99":U
      ar-invl.bol-no COLUMN-LABEL "Bol #" FORMAT ">>>>>>>9":U
      ar-invl.ord-no FORMAT ">>>>>9":U
      ar-invl.po-no COLUMN-LABEL "PO #" FORMAT "x(15)":U
  ENABLE
      ar-invl.line
      ar-invl.actnum
      ar-invl.i-no
      ar-invl.part-no
      ar-invl.i-name
      ar-invl.i-dscr
      ar-invl.lot-no
      ar-invl.inv-qty
      ar-invl.ship-qty
      ar-invl.sf-sht
      ar-invl.unit-pr
      ar-invl.pr-qty-uom
      ar-invl.cost
      ar-invl.dscr[1] HELP "Enter Cost Unit of Measure"
      ar-invl.sman[1] HELP "Enter Salesrep's Intitials or Number"
      ar-invl.s-pct[1]
      ar-invl.s-comm[1]
      ar-invl.sman[2] HELP "Enter Salesrep's Intitials or Number"
      ar-invl.s-pct[2]
      ar-invl.s-comm[2]
      ar-invl.sman[3] HELP "Enter Salesrep's Intitials or Number"
      ar-invl.s-pct[3]
      ar-invl.s-comm[3]
      ar-invl.bol-no
      ar-invl.ord-no
      ar-invl.po-no
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 6.19
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 7.43 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 7.43 COL 70 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 7.43 COL 132 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 7.43 COL 2
     RECT-4 AT ROW 7.19 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   External Tables: ASI.ar-inv
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
         HEIGHT             = 7.62
         WIDTH              = 145.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
{methods/template/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB Browser-Table TEXT-1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.ar-invl WHERE ASI.ar-inv <external> ... ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _JoinCode[1]      = "ASI.ar-invl.x-no = ASI.ar-inv.x-no"
     _FldNameList[1]   > ASI.ar-invl.line
"ar-invl.line" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.ar-invl.actnum
"ar-invl.actnum" "Account Number" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"get-actdscr() @ v-actdscr" "Account Description" "x(45)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.ar-invl.i-no
"ar-invl.i-no" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.ar-invl.part-no
"ar-invl.part-no" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.ar-invl.i-name
"ar-invl.i-name" "Item Name" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"get-i-dscr () @ ar-invl.i-dscr" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.ar-invl.i-dscr
"ar-invl.i-dscr" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.ar-invl.lot-no
"ar-invl.lot-no" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.ar-invl.inv-qty
"ar-invl.inv-qty" "Invoice Qty" "->>,>>>,>>9.9<" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.ar-invl.ship-qty
"ar-invl.ship-qty" "Ship Quantity" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   = ASI.ar-invl.cons-uom
     _FldNameList[13]   > ASI.ar-invl.sf-sht
"ar-invl.sf-sht" "SqFt" "->>,>>9.99<<" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.ar-invl.unit-pr
"ar-invl.unit-pr" ? "->>,>>>,>>9.99<<<<" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.ar-invl.pr-qty-uom
"ar-invl.pr-qty-uom" "UOM" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.ar-invl.disc
"ar-invl.disc" "Dsct%" ">>9.99" "decimal" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"calc-amt () @ ar-invl.amt" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"calc-amt () @ ar-invl.amt" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > ASI.ar-invl.amt
"ar-invl.amt" "Amount" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   = ASI.ar-invl.amt-msf
     _FldNameList[21]   > ASI.ar-invl.cost
"ar-invl.cost" "Cost" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > ASI.ar-invl.dscr[1]
"ar-invl.dscr[1]" "Cost!UOM" "x(4)" "character" ? ? ? ? ? ? yes "Enter Cost Unit of Measure" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > ASI.ar-invl.sman[1]
"ar-invl.sman[1]" "SlsRep" ? "character" ? ? ? ? ? ? yes "Enter Salesrep's Intitials or Number" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > ASI.ar-invl.s-pct[1]
"ar-invl.s-pct[1]" "% of Sale" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > ASI.ar-invl.s-comm[1]
"ar-invl.s-comm[1]" "Comm%" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > ASI.ar-invl.sman[2]
"ar-invl.sman[2]" "SlsRep" ? "character" ? ? ? ? ? ? yes "Enter Salesrep's Intitials or Number" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   > ASI.ar-invl.s-pct[2]
"ar-invl.s-pct[2]" "% of Sale" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   > ASI.ar-invl.s-comm[2]
"ar-invl.s-comm[2]" "Comm%" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[29]   > ASI.ar-invl.sman[3]
"ar-invl.sman[3]" "SlsRep" ? "character" ? ? ? ? ? ? yes "Enter Salesrep's Intitials or Number" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[30]   > ASI.ar-invl.s-pct[3]
"ar-invl.s-pct[3]" "% of Sale" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[31]   > ASI.ar-invl.s-comm[3]
"ar-invl.s-comm[3]" "Comm%" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[32]   > ASI.ar-invl.bol-no
"ar-invl.bol-no" "Bol #" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[33]   > ASI.ar-invl.ord-no
"ar-invl.ord-no" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[34]   > ASI.ar-invl.po-no
"ar-invl.po-no" "PO #" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON DEFAULT-ACTION OF Browser-Table IN FRAME F-Main
DO:
  def var phandle as widget-handle no-undo.
  def var char-hdl as cha no-undo.   
  RUN get-link-handle IN adm-broker-hdl
        (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
  phandle = WIDGET-HANDLE(char-hdl).
  
  IF NOT ll-inquiry THEN do: /*RUN new-state in phandle ('update-begin':U).*/
      RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"buttons-target",OUTPUT char-hdl).
    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) 
       THEN RUN browser-dbclicked IN WIDGET-HANDLE(char-hdl).
  END.
  ELSE DO:
      RUN new-state in phandle ('update-begin':U).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON HELP OF Browser-Table IN FRAME F-Main
DO:
   DEF VAR lv-handle AS HANDLE NO-UNDO.
   DEF VAR char-val AS cha NO-UNDO.
   DEFINE VARIABLE op-rec-id AS RECID NO-UNDO .
   DEFINE VARIABLE op-row-id AS ROWID NO-UNDO .

   CASE FOCUS:NAME:
       when "i-no" then do:
           RUN windows/l-itemfg.w (g_company, ar-inv.cust-no,ar-invl.i-no:SCREEN-VALUE IN BROWSE {&browse-name}, OUTPUT char-val).
           IF char-val <> "" THEN ASSIGN ar-invl.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = ENTRY(1,char-val).
       END.
       when "part-no" then do:
           RUN windows/l-cpart.w (g_company,ar-inv.cust-no,FOCUS:SCREEN-VALUE, OUTPUT char-val,OUTPUT op-row-id).
           IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
       END.
       when "bol-no" then do:
           RUN windows/l-bolhsp.w (g_company, ?,FOCUS:SCREEN-VALUE, OUTPUT char-val,OUTPUT op-rec-id).
           IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
       END.
       when "ord-no" then do:
           RUN windows/l-ordno.w (g_company,ar-inv.cust-no ,"",FOCUS:SCREEN-VALUE, OUTPUT char-val,OUTPUT op-rec-id).
           IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
       END.
       when "pr-qty-uom" then do:
           RUN windows/l-stduom.w (g_company, lv-uom-list,FOCUS:SCREEN-VALUE, OUTPUT char-val).
           IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
       END.
       when "dscr[1]" then do:
           RUN windows/l-stduom.w (g_company,"EA,M",FOCUS:SCREEN-VALUE, OUTPUT char-val).
           IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
       END.
       OTHERWISE DO:
          lv-handle = focus:handle.
          run applhelp.p.             
          if g_lookup-var <> "" then do:
             lv-handle:screen-value = g_lookup-var.             
          end.   /* g_lookup-var <> "" */
          g_lookup-var = "".
       END.
   END CASE.
 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON return OF Browser-Table IN FRAME F-Main
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  DEF VAR char-hdl AS CHAR NO-UNDO.

  /* This code displays initial values for newly added or copied rows. */
  RUN get-link-handle IN adm-broker-hdl
                    (THIS-PROCEDURE, "dont-enable-source", OUTPUT char-hdl).
                      
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN RETURN NO-APPLY.

  {src/adm/template/brsentry.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   /*{src/adm/template/brsleave.i} */
    {brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
 /* {methods/template/local/setvalue.i} */
  {custom/setvalue.i}  /* send ar-inv rec_key to notes */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-invl.actnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.actnum Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ar-invl.actnum IN BROWSE Browser-Table /* Account Number */
DO:
    IF LASTKEY = -1 THEN RETURN.

    IF ar-invl.actnum:MODIFIED IN BROWSE {&browse-name} THEN DO:
       FIND FIRST account WHERE account.company = g_company AND
                                account.TYPE <> "T" AND
                                account.actnum = ar-invl.actnum:SCREEN-VALUE
                                NO-LOCK NO-ERROR.
       IF NOT AVAIL account THEN DO:
          MESSAGE "Invalid GL Account Number." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
       END.
       v-actdscr:SCREEN-VALUE IN BROWSE {&browse-name} = account.dscr.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-invl.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ar-invl.i-no IN BROWSE Browser-Table /* Item No */
DO:
      IF LASTKEY = -1 THEN RETURN.

    IF ar-invl.i-no:MODIFIED IN BROWSE {&browse-name} THEN DO:
       FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company = g_company 
               AND itemfg.i-no = ar-invl.i-no:SCREEN-VALUE
             NO-ERROR.
       IF NOT AVAIL itemfg THEN DO:
          MESSAGE "Invalid FG Item Number." VIEW-AS ALERT-BOX ERROR.
          RETURN.
       END.
       ASSIGN
         ar-invl.i-dscr:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.part-dscr1
         ar-invl.i-name:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.i-name.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-invl.pr-qty-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.pr-qty-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ar-invl.pr-qty-uom IN BROWSE Browser-Table /* UOM */
DO:
    IF LASTKEY = -1 THEN RETURN.
    IF LOOKUP(ar-invl.pr-qty-uom:SCREEN-VALUE IN BROWSE {&browse-name},lv-uom-list) <= 0 THEN DO:
       MESSAGE "Invalid Unit of Measure." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-invl.dscr[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.dscr[1] Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ar-invl.dscr[1] IN BROWSE Browser-Table /* Cost!UOM */
DO:
   IF LASTKEY = -1 THEN RETURN.
    IF LOOKUP(ar-invl.dscr[1]:SCREEN-VALUE IN BROWSE {&browse-name},"EA,M") <= 0 THEN DO:
       MESSAGE "Invalid Unit of Measure.  EA or M is valid." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-invl.sman[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.sman[1] Browser-Table _BROWSE-COLUMN B-table-Win
ON HELP OF ar-invl.sman[1] IN BROWSE Browser-Table /* SlsRep */
DO:
   DEF VAR char-val AS CHAR NO-UNDO.

   run windows/l-sman2.w (g_company, output char-val).
   if char-val ne "" THEN
      ar-invl.sman[1]:SCREEN-VALUE IN BROWSE {&browse-name} = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.sman[1] Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ar-invl.sman[1] IN BROWSE Browser-Table /* SlsRep */
DO:
   IF LASTKEY = -1 THEN RETURN.

   IF ar-invl.sman[1]:MODIFIED IN BROWSE {&browse-name} THEN DO:

      IF ar-invl.sman[1]:SCREEN-VALUE NE "" AND
         NOT CAN-FIND(FIRST sman WHERE
         sman.company EQ g_company AND
         sman.sman EQ ar-invl.sman[1]:SCREEN-VALUE) THEN
         DO:
            MESSAGE "Invalid Sales Rep." VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
         END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-invl.sman[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.sman[2] Browser-Table _BROWSE-COLUMN B-table-Win
ON HELP OF ar-invl.sman[2] IN BROWSE Browser-Table /* SlsRep */
DO:
   DEF VAR char-val AS CHAR NO-UNDO.

   run windows/l-sman2.w (g_company, output char-val).
   if char-val ne "" THEN
      ar-invl.sman[2]:SCREEN-VALUE IN BROWSE {&browse-name} = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.sman[2] Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ar-invl.sman[2] IN BROWSE Browser-Table /* SlsRep */
DO:
   IF LASTKEY = -1 THEN RETURN.

   IF ar-invl.sman[2]:MODIFIED IN BROWSE {&browse-name} THEN DO:

      IF ar-invl.sman[2]:SCREEN-VALUE NE "" AND
         NOT CAN-FIND(FIRST sman WHERE
         sman.company EQ g_company AND
         sman.sman EQ ar-invl.sman[2]:SCREEN-VALUE) THEN
         DO:
            MESSAGE "Invalid Sales Rep." VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
         END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-invl.sman[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.sman[3] Browser-Table _BROWSE-COLUMN B-table-Win
ON HELP OF ar-invl.sman[3] IN BROWSE Browser-Table /* SlsRep */
DO:
   DEF VAR char-val AS CHAR NO-UNDO.

   run windows/l-sman2.w (g_company, output char-val).
   if char-val ne "" THEN
      ar-invl.sman[3]:SCREEN-VALUE IN BROWSE {&browse-name} = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.sman[3] Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ar-invl.sman[3] IN BROWSE Browser-Table /* SlsRep */
DO:
   IF LASTKEY = -1 THEN RETURN.

   IF ar-invl.sman[3]:MODIFIED IN BROWSE {&browse-name} THEN DO:

      IF ar-invl.sman[3]:SCREEN-VALUE NE "" AND
         NOT CAN-FIND(FIRST sman WHERE
         sman.company EQ g_company AND
         sman.sman EQ ar-invl.sman[3]:SCREEN-VALUE) THEN
         DO:
            MESSAGE "Invalid Sales Rep." VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
         END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-invl.bol-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.bol-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ar-invl.bol-no IN BROWSE Browser-Table /* Bol # */
DO:
    IF LASTKEY = -1 THEN RETURN.

    IF ar-invl.bol-no:MODIFIED IN BROWSE {&browse-name} 
     AND ar-invl.bol-no:SCREEN-VALUE GT "" THEN DO:
       FIND FIRST oe-bolh WHERE oe-bolh.company = g_company AND                                
                                oe-bolh.bol-no = INTEGER(ar-invl.bol-no:SCREEN-VALUE)
                                NO-LOCK NO-ERROR.
       IF NOT AVAIL oe-bolh THEN DO:
          MESSAGE "Invalid BOL Number." VIEW-AS ALERT-BOX ERROR.
          RETURN.
       END.
       ELSE DO:
        find FIRST oe-boll no-lock 
         WHERE oe-boll.company EQ oe-bolh.company
          AND oe-boll.bol-no EQ oe-bolh.bol-no
          and oe-boll.i-no EQ ar-invl.i-no:screen-value
         no-error.
        IF avail oe-boll and INTEGER(ar-invl.ord-no:SCREEN-VALUE) EQ 0 THEN
         assign
         ar-invl.ord-no:screen-value = string(oe-boll.ord-no)
         ar-invl.po-no:screen-value = string(oe-boll.po-no)
         .
        if avail oe-boll then
         find first oe-ordl no-lock 
          where oe-ordl.company eq oe-boll.company
            and oe-ordl.ord-no eq oe-boll.ord-no
            and oe-ordl.i-no eq oe-boll.i-no
          no-error.
          if avail oe-ordl then
         ar-invl.part-no:screen-value = oe-ordl.part-no
         .
       END.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-invl.ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.ord-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ar-invl.ord-no IN BROWSE Browser-Table /* Order# */
DO:
      IF LASTKEY = -1 THEN RETURN.

    IF ar-invl.ord-no:MODIFIED IN BROWSE {&browse-name} 
     AND ar-invl.ord-no:screen-value GT "" THEN DO:
       FIND FIRST oe-ord WHERE oe-ord.company = g_company AND                                
                                oe-ord.ord-no = INTEGER(ar-invl.ord-no:SCREEN-VALUE)
                                NO-LOCK NO-ERROR.
       IF NOT AVAIL oe-ord THEN DO:
          MESSAGE "Invalid Order Number." VIEW-AS ALERT-BOX ERROR.
          RETURN.
       END.
       
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

RUN oe/oe-sysct.p.

IF NOT v-oecomm-log THEN RUN show-comm (NO).

{sys/inc/f3help.i}
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "ar-inv"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ar-inv"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE auto-add B-table-Win 
PROCEDURE auto-add :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS CHAR NO-UNDO.


  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source", OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
    RUN auto-add IN WIDGET-HANDLE(char-hdl).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE auto-copy B-table-Win 
PROCEDURE auto-copy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS CHAR NO-UNDO.


  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source", OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
    RUN auto-copy IN WIDGET-HANDLE(char-hdl).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete_item B-table-Win 
PROCEDURE delete_item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-dumb AS LOG NO-UNDO.
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF VAR lv-loc LIKE rm-rctd.loc NO-UNDO.
  DEF VAR ll-renumber AS LOG NO-UNDO.
  DEF BUFFER b-po-ordl FOR po-ordl.

   RUN local-delete-record .
  
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE inquiry-mode B-table-Win 
PROCEDURE inquiry-mode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/winReSizeLocInit.i}

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"inquiry-source", OUTPUT char-hdl).

  ll-inquiry = VALID-HANDLE(WIDGET-HANDLE(char-hdl)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record B-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF ar-inv.posted THEN DO:
     MESSAGE "Invoice already posted. No Adding allowed!" VIEW-AS ALERT-BOX ERROR.     
     RETURN .
  END.

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
  DEF VAR out-qty LIKE ar-invl.qty NO-UNDO.
  DEF BUFFER bf-inv FOR ar-inv.

  /* Code placed here will execute PRIOR to standard behavior. */
  FIND bf-inv WHERE RECID(bf-inv) = RECID(ar-inv) .
  IF adm-new-record AND NOT adm-adding-record THEN .  /* copy */
  ELSE DO:  
    ASSIGN bf-inv.gross = bf-inv.gross - ar-invl.amt
           bf-inv.net   = bf-inv.net - ar-invl.amt.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ar-invl.qty = ar-invl.inv-qty.

  run sys/ref/convsuom.p (ar-invl.cons-uom,
                          ar-invl.pr-qty-uom,
                          ar-invl.sf-sht,
                          ar-invl.qty,
                          OUTPUT out-qty).

  assign
   ar-invl.amt     = if   (out-qty * ar-invl.unit-pr) eq 0
                     then (ar-invl.qty * ar-invl.unit-pr)
                     else (out-qty * ar-invl.unit-pr)
   ar-invl.amt-msf = ((ar-invl.qty * ar-invl.sf-sht) / 1000.0)
   bf-inv.gross    = bf-inv.gross + ar-invl.amt
   bf-inv.net      = bf-inv.net + ar-invl.amt.

   find first cust where cust.company eq g_company
                      and cust.cust-no eq ar-inv.cust-no no-lock no-error.
   ar-invl.tax = if ar-inv.tax-code ne "" and cust.sort eq "Y" then YES ELSE NO.
 
  IF ar-invl.bol-no GT 0 
    AND ar-invl.b-no EQ 0 THEN DO:
    FIND FIRST oe-bolh NO-LOCK
      WHERE oe-bolh.company EQ ar-invl.company
        AND oe-bolh.bol-no EQ ar-invl.bol-no 
      NO-ERROR.
    IF AVAIL oe-bolh THEN DO:
         
      ar-invl.b-no = oe-bolh.b-no.
      
    END.
      
  END.
  {ar/ar-invk.i bf-inv}

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
  IF NOT v-oecomm-log THEN RUN show-comm (NO).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-invl FOR ar-invl.
  DEF VAR li-next-line AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  FIND LAST bf-invl WHERE bf-invl.x-no = ar-inv.x-no USE-INDEX x-no NO-LOCK NO-ERROR.
  li-next-line = IF AVAIL bf-invl THEN bf-invl.LINE + 1 ELSE 1.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST ar-ctrl WHERE ar-ctrl.company = ar-inv.company NO-LOCK NO-ERROR.
   
  FIND FIRST cust WHERE
       cust.company EQ ar-inv.company AND
       cust.cust-no EQ ar-inv.cust-no
       NO-LOCK NO-ERROR.

  ASSIGN ar-invl.x-no = ar-inv.x-no
         ar-invl.company = ar-inv.company
         ar-invl.cust-no = ar-inv.cust-no
         ar-invl.inv-no = ar-inv.inv-no
         ar-invl.LINE = li-next-line
         ar-invl.po-no = ar-inv.po-no
         ar-invl.pr-qty-uom = "EA"
         ar-invl.cons-uom = "EA"
         ar-invl.dscr[1] = "M"
         ar-invl.actnum = IF AVAIL ar-ctrl THEN ar-ctrl.sales ELSE ""
         ar-invl.sman[1] = IF AVAIL cust THEN cust.sman ELSE ""
         ar-invl.s-pct[1] = IF ar-invl.sman[1] NE "" THEN 100 ELSE 0.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-inv FOR ar-inv.

  /* Code placed here will execute PRIOR to standard behavior. */
  IF ar-inv.posted THEN DO:
     MESSAGE "Invoice already posted. No Deletion allowed!" VIEW-AS ALERT-BOX ERROR.
     RETURN .
  END.

  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.

  FIND bf-inv WHERE RECID(bf-inv) = RECID(ar-inv).
  ASSIGN bf-inv.net = bf-inv.net - ar-invl.amt
         bf-inv.gross = bf-inv.gross - ar-invl.amt
         .
  {ar/ar-invk.i bf-inv}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"invhead-target", OUTPUT char-hdl).
  RUN reopen-query IN WIDGET-HANDLE(char-hdl).

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
  IF ar-inv.posted THEN DO:
     MESSAGE "Invoice already posted. No Editing allowed!" VIEW-AS ALERT-BOX ERROR.
     RETURN .
  END.

  IF NOT v-oecomm-log THEN RUN show-comm (NO).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-new-record AS LOG NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  /* === Validateion=======*/
  IF ar-invl.actnum:MODIFIED IN BROWSE {&browse-name} THEN DO:
       FIND FIRST account WHERE account.company = g_company AND
                                account.TYPE <> "T" AND
                                account.actnum = ar-invl.actnum:SCREEN-VALUE
                                NO-LOCK NO-ERROR.
       IF NOT AVAIL account THEN DO:
          MESSAGE "Invalid GL Account Number." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO ar-invl.actnum.
          RETURN NO-APPLY.
       END.
  END.
  IF ar-invl.bol-no:MODIFIED IN BROWSE {&browse-name}
   AND ar-invl.bol-no:SCREEN-VALUE GT "" THEN DO:
       /* Bol# check */
       FIND FIRST oe-boll NO-LOCK 
           WHERE oe-boll.company EQ g_company 
             AND oe-boll.bol-no  EQ INTEGER(ar-invl.bol-no:SCREEN-VALUE)
             AND oe-boll.i-no    EQ ar-invl.i-no:SCREEN-VALUE
           NO-ERROR.
       IF NOT AVAIL oe-boll THEN DO:
          MESSAGE "Invalid Bol # for item#." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO ar-invl.bol-no.
          RETURN NO-APPLY.
       END.
       
       IF INTEGER(ar-invl.ord-no:SCREEN-VALUE) NE oe-boll.ord-no THEN DO:
          MESSAGE "Invalid Bol # for item#." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO ar-invl.bol-no.
          RETURN NO-APPLY.       
       END.
       IF ar-invl.po-no:SCREEN-VALUE NE oe-boll.po-no THEN DO:
          MESSAGE "Invalid PO# for this BOL." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO ar-invl.bol-no.
          RETURN NO-APPLY.       
       END.       
  END.

  IF LOOKUP(ar-invl.pr-qty-uom:SCREEN-VALUE IN BROWSE {&browse-name} ,lv-uom-list) <= 0 THEN DO:
     MESSAGE "Invalid Unit of Measure." VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO ar-invl.pr-qty-uom.
     RETURN NO-APPLY.
  END.
  IF LOOKUP(ar-invl.dscr[1]:SCREEN-VALUE IN BROWSE {&browse-name},"EA,M") <= 0 THEN DO:
     MESSAGE "Invalid Unit of Measure. EA or M is valid." VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO ar-invl.pr-qty-uom.
     RETURN NO-APPLY.
  END.
   
  ll-new-record = adm-new-record.
    
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
 


  /* Code placed here will execute AFTER standard behavior.    */
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"invhead-target", OUTPUT char-hdl).
  RUN reopen-query IN WIDGET-HANDLE(char-hdl).
  FIND CURRENT ar-invl NO-LOCK NO-ERROR.
  RUN dispatch ('display-fields').
      
  IF NOT v-oecomm-log THEN RUN show-comm (NO).

  IF ll-new-record THEN RUN auto-add.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE printInv B-table-Win 
PROCEDURE printInv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF AVAILABLE ar-inv THEN DO:
    RUN custom/setUserPrint.p (ar-inv.company,'ar-inv_.',
                               'begin_inv,end_inv,begin_cust,end_cust,begin_date,end_date,tb_reprint,tb_posted',
                               STRING(ar-inv.inv-no) + ',' + STRING(ar-inv.inv-no) + ',' +
                               ar-inv.cust-no + ',' + ar-inv.cust-no + ',' +
                               STRING(ar-inv.inv-date) + ',' + STRING(ar-inv.inv-date) + ',' +
                               STRING(ar-inv.printed) + ',' + STRING(ar-inv.posted)).
    RUN listobjs/ar-inv_.w.
  END.

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
  
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"invhead-target", OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN reopen-query IN WIDGET-HANDLE(char-hdl).

  RUN dispatch ('open-query').

  DO WITH FRAME {&FRAME-NAME}:
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
  END.
  
 APPLY "VALUE-CHANGED":U TO BROWSE {&browse-name}.
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
  {src/adm/template/snd-list.i "ar-inv"}
  {src/adm/template/snd-list.i "ar-invl"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-comm B-table-Win 
PROCEDURE show-comm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-visible AS LOG NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     ar-invl.s-pct[1]:VISIBLE IN BROWSE {&browse-name}  = ip-visible
     ar-invl.s-pct[2]:VISIBLE IN BROWSE {&browse-name}  = ip-visible
     ar-invl.s-pct[3]:VISIBLE IN BROWSE {&browse-name}  = ip-visible
     ar-invl.s-comm[1]:VISIBLE IN BROWSE {&browse-name} = ip-visible
     ar-invl.s-comm[2]:VISIBLE IN BROWSE {&browse-name} = ip-visible
     ar-invl.s-comm[3]:VISIBLE IN BROWSE {&browse-name} = ip-visible.
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE undo-added B-table-Win 
PROCEDURE undo-added :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF adm-new-record THEN RUN dispatch ("cancel-record").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calc-amt B-table-Win 
FUNCTION calc-amt RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN IF ll-inquiry AND AVAIL ar-invl          AND /* Function return value. */
            NOT ar-invl.billable AND ar-invl.misc THEN 0
                                                  ELSE ar-invl.amt.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-actdscr B-table-Win 
FUNCTION get-actdscr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF AVAIL ar-invl THEN DO:
     FIND FIRST account WHERE account.company = g_company
                          AND account.actnum = ar-invl.actnum NO-LOCK NO-ERROR.
     IF AVAIL account THEN RETURN account.dscr.
     ELSE RETURN "".
  END.
  ELSE RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-i-dscr B-table-Win 
FUNCTION get-i-dscr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN IF AVAIL ar-invl AND ar-invl.i-dscr EQ "" THEN
           ar-invl.part-dscr1 ELSE ar-invl.i-dscr.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

