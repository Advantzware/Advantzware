&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: ar\repstin1.w

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

{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.

DEF BUFFER b-ar-inv FOR ar-inv.
DEF BUFFER b-ar-invl FOR ar-invl.
DEF BUFFER b-cust FOR cust.

DEF NEW SHARED VAR v-ar-acct LIKE ar-ctrl.receivables.
DEF NEW SHARED VAR v-ar-freight LIKE ar-ctrl.freight.
DEF NEW SHARED VAR v-ar-stax LIKE ar-ctrl.stax.
DEF NEW SHARED VAR v-ar-sales LIKE ar-ctrl.sales.
DEF NEW SHARED VAR v-ar-disc LIKE ar-ctrl.discount.
DEF NEW SHARED VAR v-return AS LOG INIT NO.
DEFINE VARIABLE lCheckFirst AS LOGICAL INIT YES NO-UNDO .
DEFINE VARIABLE cItemDscr AS CHARACTER NO-UNDO .
DEF VAR ld-new-diff AS DEC NO-UNDO.
DEF VAR v-col-move AS LOG NO-UNDO INIT TRUE.

RUN oe/getacct.p.

DEF VAR op-enable-price AS LOG NO-UNDO.
DEFINE VARIABLE l-enable-price AS LOG NO-UNDO.
DEFINE VARIABLE lAllowUpdate AS LOGICAL NO-UNDO .
DEFINE VARIABLE lAccessClose AS LOGICAL NO-UNDO .
DEFINE VARIABLE cAccessList AS CHARACTER NO-UNDO .
RUN methods/prgsecur.p
	    (INPUT "p-repstin.",
	     INPUT "Update", /* based on run, create, update, delete or all */
	     INPUT NO,    /* use the directory in addition to the program */
	     INPUT NO,    /* Show a message if not authorized */
	     INPUT NO,    /* Group overrides user security? */
	     OUTPUT lAllowUpdate, /* Allowed? Yes/NO */
	     OUTPUT lAccessClose, /* used in template/windows.i  */
	     OUTPUT cAccessList). /* list 1's and 0's indicating yes or no to run, create, update, delete */

&SCOPED-DEFINE sortby BY ar-invl.inv-no BY ar-invl.line

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ar-inv ar-invl

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table ar-inv.inv-no ar-inv.cust-no ar-inv.inv-date  ~
ar-invl.ord-no ar-invl.po-no ar-invl.i-no ar-invl.inv-qty ar-invl.pr-qty-uom ar-invl.unit-pr ~
ar-invl.disc ar-invl.amt ar-invl.sman[1] ar-invl.s-pct[1] ar-invl.s-comm[1] ~
ar-invl.sman[2] ar-invl.s-pct[2] ar-invl.s-comm[2] ar-invl.sman[3] ~
ar-invl.s-pct[3] ar-invl.s-comm[3] ar-inv.cust-name get-inv-dscr() @ cItemDscr
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table ar-invl.po-no ~
ar-invl.unit-pr ar-invl.sman[1] ar-invl.s-pct[1] ar-invl.s-comm[1] ~
ar-invl.sman[2] ar-invl.s-pct[2] ar-invl.s-comm[2] ar-invl.sman[3] ~
ar-invl.s-pct[3] ar-invl.s-comm[3] 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table ar-inv ar-invl  
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table ar-invl
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-br_table ar-inv
&Scoped-define QUERY-STRING-br_table FOR EACH  ~
      ar-inv WHERE ~{&KEY-PHRASE} AND  ar-inv.company eq cocode AND ~
     ar-inv.inv-no GE fi_inv-no AND (ar-inv.inv-no LE end_inv-no OR end_inv-no EQ 0 ) AND ar-inv.inv-date GE begin_inv-date AND ~
     ar-inv.inv-date LE end_inv-date AND ar-inv.cust-no GE begin_cust-no AND   ~
     (ar-inv.cust-no LE end_cust-no OR end_cust-no EQ "")    NO-LOCK , ~
EACH ar-invl WHERE ar-invl.x-no EQ ar-inv.x-no  ~
     AND (ar-invl.sman[1] GE begin_sman) AND (ar-invl.sman[1] LE end_sman OR end_sman EQ "") NO-LOCK    ~
    BY ar-invl.inv-no BY ar-invl.LINE ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH  ~
      ar-inv WHERE ~{&KEY-PHRASE} AND ar-inv.company eq cocode AND ~
     ar-inv.inv-no GE fi_inv-no AND (ar-inv.inv-no LE end_inv-no OR end_inv-no EQ 0 ) AND ar-inv.inv-date GE begin_inv-date AND ~
     ar-inv.inv-date LE end_inv-date AND ar-inv.cust-no GE begin_cust-no AND   ~
     (ar-inv.cust-no LE end_cust-no OR end_cust-no EQ "")    NO-LOCK , ~
EACH ar-invl WHERE ar-invl.x-no EQ ar-inv.x-no   ~
     AND (ar-invl.sman[1] GE begin_sman) AND (ar-invl.sman[1] LE end_sman OR end_sman EQ "") NO-LOCK    ~
    BY ar-invl.inv-no BY ar-invl.LINE ~{&SORTBY-PHRASE} .
&Scoped-define TABLES-IN-QUERY-br_table ar-inv ar-invl
&Scoped-define FIRST-TABLE-IN-QUERY-br_table ar-inv
&Scoped-define SECOND-TABLE-IN-QUERY-br_table ar-invl


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi_inv-no btn_go begin_cust-no br_table ~
end_inv-no end_cust-no begin_sman end_sman begin_inv-date end_inv-date 
&Scoped-Define DISPLAYED-OBJECTS fi_inv-no begin_cust-no end_inv-no ~
end_cust-no begin_sman end_sman begin_inv-date end_inv-date 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/*
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
company||y|asi.eb.company
Carrier||y|asi.eb.Carrier
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "company,Carrier"':U).

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
</FILTER-ATTRIBUTES> */     */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-inv-dscr B-table-Win 
FUNCTION get-inv-dscr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_go 
     LABEL "Go" 
     SIZE 10 BY 1.14.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Begin Customer#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE begin_inv-date AS DATE FORMAT "99/99/9999":U INITIAL ? 
     LABEL "Begin Inv Date" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE begin_sman AS CHARACTER FORMAT "X(3)":U 
     LABEL "Begin Sales#1" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "End Customer#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_inv-date AS DATE FORMAT "99/99/9999":U INITIAL ? 
     LABEL "End Inv Date" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE end_sman AS CHARACTER FORMAT "X(3)":U 
     LABEL "End Sales#1" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE fi_inv-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Begin Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE end_inv-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "End Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      ar-inv, 
      ar-invl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      ar-inv.inv-no COLUMN-LABEL "Invoice#" FORMAT ">>>>>>>":U
      
      ar-inv.inv-date COLUMN-LABEL "Inv Date" FORMAT "99/99/9999":U
      ar-inv.cust-no COLUMN-LABEL "Customer" FORMAT "x(8)":U
      ar-invl.ord-no FORMAT ">>>>>>>":U WIDTH 12
      ar-invl.amt COLUMN-LABEL "Ext Price" FORMAT "->>>,>>>,>>9.99":U
            WIDTH 20
      ar-invl.sman[1] COLUMN-LABEL "Slsmn" FORMAT "x(3)":U
      ar-invl.s-pct[1] COLUMN-LABEL "% of Sale" FORMAT ">>9.99":U
      ar-invl.s-comm[1] COLUMN-LABEL "Comm%" FORMAT ">>9.99":U
      ar-invl.sman[2] COLUMN-LABEL "Slsmn" FORMAT "x(3)":U
      ar-invl.s-pct[2] COLUMN-LABEL "% of Sale" FORMAT ">>9.99":U
      ar-invl.s-comm[2] COLUMN-LABEL "Comm%" FORMAT ">>9.99":U
      ar-invl.sman[3] COLUMN-LABEL "Slsmn" FORMAT "x(3)":U
      ar-invl.s-pct[3] COLUMN-LABEL "% of Sale" FORMAT ">>9.99":U
      ar-invl.s-comm[3] COLUMN-LABEL "Comm%" FORMAT ">>9.99":U
      ar-inv.cust-name COLUMN-LABEL "Customer Name" FORMAT "x(30)":U
      ar-invl.po-no COLUMN-LABEL "Purchase Order#" FORMAT "x(15)":U
            WIDTH 23
      ar-invl.i-no COLUMN-LABEL "FG Item#" FORMAT "x(15)":U WIDTH 23
      get-inv-dscr() @ cItemDscr COLUMN-LABEL "FG Description" FORMAT "x(30)":U 
      ar-invl.inv-qty COLUMN-LABEL "Inv Qty" FORMAT "->>>,>>>,>>>":U
      ar-invl.pr-qty-uom COLUMN-LABEL "UOM" FORMAT "x(4)":U
      ar-invl.unit-pr COLUMN-LABEL "Unit Price" FORMAT "->>,>>>,>>9.99<<<<":U
            WIDTH 16
      ar-invl.disc COLUMN-LABEL "Disc%" FORMAT ">>>,>>9.99":U
      
  ENABLE
      ar-invl.po-no
      ar-invl.unit-pr
      ar-invl.sman[1]
      ar-invl.s-pct[1]
      ar-invl.s-comm[1]
      ar-invl.sman[2]
      ar-invl.s-pct[2]
      ar-invl.s-comm[2]
      ar-invl.sman[3]
      ar-invl.s-pct[3]
      ar-invl.s-comm[3]
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 143 BY 15
         BGCOLOR 8 FONT 6.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi_inv-no AT ROW 1.71 COL 20 COLON-ALIGNED
     end_inv-no AT ROW 3.05 COL 20 COLON-ALIGNED WIDGET-ID 4
     begin_inv-date AT ROW 1.76 COL 50 COLON-ALIGNED WIDGET-ID 12
     end_inv-date AT ROW 3.05 COL 50 COLON-ALIGNED WIDGET-ID 14
     begin_cust-no AT ROW 1.76 COL 86.4 COLON-ALIGNED
     end_cust-no AT ROW 3.05 COL 86.4 COLON-ALIGNED WIDGET-ID 6
     begin_sman AT ROW 1.76 COL 119 COLON-ALIGNED WIDGET-ID 8
     end_sman AT ROW 3.05 COL 119 COLON-ALIGNED WIDGET-ID 10
     btn_go AT ROW 2.91 COL 132
     
     br_table AT ROW 4.81 COL 1
     
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
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
         HEIGHT             = 18.86
         WIDTH              = 143.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
/*{src/adm/method/navbrows.i}*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
/* BROWSE-TAB br_table begin_cust-no F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.
ASSIGN 
       br_table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 1.
     br_table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.


/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

         
&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "asi.ar-inv,asi.ar-invl WHERE asi.ar-inv ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "ar-inv.company eq cocode and
          ar-inv.inv-no eq fi_inv-no AND (ar-inv.inv-no LE end_inv-no OR end_inv-no EQ 0 ) AND (ar-inv.inv-date GE begin_inv-date) AND 
         (ar-inv.inv-date LE end_inv-date) AND (ar-inv.cust-no GE begin_cust-no ) AND 
         (ar-inv.cust-no LE end_cust-no OR end_cust-no EQ "")  "
     _JoinCode[2]      = "ar-invl.x-no eq ar-inv.x-no AND (ar-invl.sman[1] GE begin_sman) AND (ar-invl.sman[1] LE end_sman OR end_sman EQ "")"
     _FldNameList[1]   > asi.ar-inv.inv-no
"ar-inv.inv-no" "Invoice#" ">>>>>>>" "integer" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.ar-inv.inv-date
"ar-inv.inv-date" "Inv Date" "99/99/9999" ? ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no     
    _FldNameList[3]   > asi.ar-inv.cust-no
"ar-inv.cust-no" "Customer" "x(8)" "Character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no     
   _FldNameList[4]   > asi.ar-inv.ord-no
"ar-inv.ord-no" "Order" ">>>>>>>" "integer" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
    _FldNameList[5]   > asi.ar-inv.cust-name
"ar-inv.cust-name" "Customer Name" "x(30)" "Character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no     
   _FldNameList[6]   > "_<CALC>"
"get-inv-dscr() @ cItemDscr" "FG Description" "x(30)" "Character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no     
     _FldNameList[7]   > asi.ar-invl.po-no
"ar-invl.po-no" "Purchase Order#" ? "character" ? ? ? ? ? ? yes ? no no "23" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > asi.ar-invl.i-no
"ar-invl.i-no" "FG Item#" ? "character" ? ? ? ? ? ? no ? no no "23" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > asi.ar-invl.inv-qty
"ar-invl.inv-qty" "Inv Qty" "->>>,>>>,>>>" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > asi.ar-invl.pr-qty-uom
"ar-invl.pr-qty-uom" "UOM" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > asi.ar-invl.unit-pr
"ar-invl.unit-pr" "Unit Price" "->>,>>>,>>9.99<<<<" "decimal" ? ? ? ? ? ? yes ? no no "16" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > asi.ar-invl.disc
"ar-invl.disc" "Disc%" ">>>,>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > asi.ar-invl.amt
"ar-invl.amt" "Ext Price" "->>>,>>>,>>9.99" "decimal" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > asi.ar-invl.sman[1]
"ar-invl.sman[1]" "Slsmn" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > asi.ar-invl.s-pct[1]
"ar-invl.s-pct[1]" "% of Sale" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > asi.ar-invl.s-comm[1]
"ar-invl.s-comm[1]" "Comm%" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > asi.ar-invl.sman[2]
"ar-invl.sman[2]" "Slsmn" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > asi.ar-invl.s-pct[2]
"ar-invl.s-pct[2]" "% of Sale" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > asi.ar-invl.s-comm[2]
"ar-invl.s-comm[2]" "Comm%" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > asi.ar-invl.sman[3]
"ar-invl.sman[3]" "Slsmn" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > asi.ar-invl.s-pct[3]
"ar-invl.s-pct[3]" "% of Sale" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > asi.ar-invl.s-comm[3]
"ar-invl.s-comm[3]" "Comm%" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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

&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no B-table-Win
ON LEAVE OF begin_cust-no IN FRAME F-Main /* Begin Customer# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_inv-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_inv-date B-table-Win
ON LEAVE OF begin_inv-date IN FRAME F-Main /* Begin Inv Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_sman
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_sman B-table-Win
ON LEAVE OF begin_sman IN FRAME F-Main /* Begin Sales#1 */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON DEFAULT-ACTION OF br_table IN FRAME F-Main
DO:
    DEFINE VARIABLE rwRowid AS ROWID NO-UNDO .
    DEFINE VARIABLE lCancelBtn AS LOGICAL NO-UNDO .
    IF lAllowUpdate THEN do:
        RUN ar/d-repstin.w ("update" ,ROWID(ar-inv), ROWID(ar-invl), OUTPUT rwRowid, OUTPUT lCancelBtn).
        IF NOT lCancelBtn THEN do:
            RUN dispatch ("open-query").
            DO WITH FRAME {&FRAME-NAME}:
                REPOSITION br_table TO ROWID rwRowid NO-ERROR.
            END.
        END.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main B-table-Win
ON HELP OF FRAME F-Main
DO:
  DEFINE VARIABLE char-val      AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE li            AS INTEGER       NO-UNDO.
  DEFINE VARIABLE lw-focus      AS WIDGET-HANDLE NO-UNDO.
  DEFINE VARIABLE oprRecid      AS RECID         NO-UNDO.
  DEFINE VARIABLE cFieldsValue  AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cFoundValue   AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE recFoundRecID AS RECID         NO-UNDO.  

  lw-focus = FOCUS.

  case lw-focus:name :   
      when "fi_inv-no" then do:
          run windows/l-arinv.w (cocode,"",fi_inv-no:screen-value, output char-val, OUTPUT oprRecid).
          if char-val ne "" then 
              fi_inv-no:screen-value = entry(1,char-val).
      END.
      when "end_inv-no" then do:
          run windows/l-arinv.w (cocode,"",end_inv-no:screen-value, output char-val, OUTPUT oprRecid).
          if char-val ne "" then 
              end_inv-no:screen-value = entry(1,char-val).
      END.
      WHEN "begin_sman" THEN DO:        
          RUN system/openLookup.p (
              INPUT  cocode, 
              INPUT  "",  /* Lookup ID */
              INPUT  29,  /* Subject ID */
              INPUT  "",  /* User ID */
              INPUT  0,   /* Param Value ID */
              OUTPUT cFieldsValue, 
              OUTPUT cFoundValue, 
              OUTPUT recFoundRecID
              ).
          IF cFoundValue NE "" THEN 
              begin_sman:screen-value = cFoundValue.
      END.
      WHEN "end_sman" THEN DO:
          RUN system/openLookup.p (
              INPUT  cocode, 
              INPUT  "",  /* Lookup ID */
              INPUT  29,  /* Subject ID */
              INPUT  "",  /* User ID */
              INPUT  0,   /* Param Value ID */
              OUTPUT cFieldsValue, 
              OUTPUT cFoundValue, 
              OUTPUT recFoundRecID
              ).
          IF cFoundValue NE "" THEN 
              end_sman:SCREEN-VALUE = cFoundValue.
      END.
      when "begin_cust-no" then do:
          run windows/l-cust.w (cocode,begin_cust-no:screen-value, output char-val).
          if char-val ne "" then 
              begin_cust-no:screen-value = entry(1,char-val).
      END.
      when "end_cust-no" then do:
          run windows/l-cust.w (cocode,end_cust-no:screen-value, output char-val).
          if char-val ne "" then 
              end_cust-no:screen-value = entry(1,char-val).
      END.
     
  end case.

  APPLY "entry" TO lw-focus.
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON HELP OF br_table IN FRAME F-Main
DO:
  DEFINE VARIABLE char-val AS cha NO-UNDO.
  DEFINE VARIABLE li AS INTEGER NO-UNDO.
  DEFINE VARIABLE lw-focus AS WIDGET-HANDLE NO-UNDO.
  DEFINE VARIABLE cFieldsValue  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFoundValue   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE recFoundRecID AS RECID     NO-UNDO.  

  lw-focus = FOCUS.

  CASE lw-focus:NAME :   
     WHEN "sman" THEN DO:
         li = FRAME-INDEX.
         RUN system/openLookup.p (
             INPUT  ar-inv.company, 
             INPUT  "", /* Lookup ID */
             INPUT  155,  /* Subject ID */
             INPUT  "", /* User ID */
             INPUT  0,  /* Param Value ID */
             OUTPUT cFieldsValue, 
             OUTPUT cFoundValue, 
             OUTPUT recFoundRecID
             ).
         IF cFoundValue NE "" THEN DO:
           IF li EQ 1 AND ar-invl.sman[1]:screen-value IN BROWSE {&browse-name} NE cFoundValue THEN 
             ar-invl.sman[1]:screen-value = cFoundValue.
           ELSE
           IF li EQ 2 AND ar-invl.sman[2]:screen-value IN BROWSE {&browse-name} NE cFoundValue THEN 
             ar-invl.sman[2]:screen-value = cFoundValue.
           ELSE
           IF li EQ 3 AND ar-invl.sman[3]:screen-value IN BROWSE {&browse-name} NE cFoundValue THEN 
             ar-invl.sman[3]:screen-value = cFoundValue.
         end.
     end.
  end case.

  APPLY "entry" TO lw-focus.
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON RETURN OF br_table IN FRAME F-Main
ANYWHERE
DO:
   APPLY "tab" TO SELF.
   RETURN NO-APPLY.
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
    {brsleave.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-invl.po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.po-no br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF ar-invl.po-no IN BROWSE br_table /* Purchase Order# */
DO:
   IF l-enable-price = NO THEN
   DO:
      APPLY "TAB" TO ar-invl.po-no IN BROWSE {&browse-name}.
      return no-apply.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-invl.unit-pr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.unit-pr br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF ar-invl.unit-pr IN BROWSE br_table /* Unit Price */
DO:
   IF /*op-enable-price*/ l-enable-price = NO THEN
   DO:
      APPLY "TAB" TO ar-invl.unit-pr IN BROWSE {&browse-name}.
      return no-apply.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-invl.sman[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.sman[1] br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ar-invl.sman[1] IN BROWSE br_table /* Slsmn */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-sman (1) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-invl.s-pct[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.s-pct[1] br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ar-invl.s-pct[1] IN BROWSE br_table /* % of Sale */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-s-pct (1) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-invl.sman[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.sman[2] br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ar-invl.sman[2] IN BROWSE br_table /* Slsmn */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-sman (2) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-invl.s-pct[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.s-pct[2] br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ar-invl.s-pct[2] IN BROWSE br_table /* % of Sale */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-s-pct (2) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-invl.sman[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.sman[3] br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ar-invl.sman[3] IN BROWSE br_table /* Slsmn */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-sman (3) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-invl.s-pct[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.s-pct[3] br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ar-invl.s-pct[3] IN BROWSE br_table /* % of Sale */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-s-pct (3) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Go */
DO:
   DEFINE VARIABLE rwRowid AS ROWID NO-UNDO .
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN {&displayed-objects}.
       
      RUN dispatch ("open-query").
     
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no B-table-Win
ON LEAVE OF end_cust-no IN FRAME F-Main /* End Customer# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_inv-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_inv-date B-table-Win
ON LEAVE OF end_inv-date IN FRAME F-Main /* End Inv Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_sman
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_sman B-table-Win
ON LEAVE OF end_sman IN FRAME F-Main /* End Sales#1 */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_inv-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_inv-no B-table-Win
ON LEAVE OF fi_inv-no IN FRAME F-Main /* Begin Invoice# */
DO:
  IF LASTKEY NE -1 THEN DO:
    ASSIGN {&self-name}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fi_inv-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_inv-no B-table-Win
ON VALUE-CHANGED OF fi_inv-no IN FRAME F-Main /* Begin Invoice# */
DO:
  IF LASTKEY NE -1 THEN DO:
    end_inv-no:SCREEN-VALUE = (fi_inv-no:SCREEN-VALUE) .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&Scoped-define SELF-NAME end_inv-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_inv-no B-table-Win
ON LEAVE OF end_inv-no IN FRAME F-Main /* End Invoice# */
DO:
  IF LASTKEY NE -1 THEN DO:
    ASSIGN {&self-name}.

    /*RUN dispatch ("open-query").

    RUN valid-inv-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.*/
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

ASSIGN  begin_inv-date = 01/01/2000
        end_inv-date  = TODAY .

&SCOPED-DEFINE cellColumnDat repstin1.w 
 {methods/browsers/setCellColumns.i}

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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-ledger B-table-Win 
PROCEDURE get-ledger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF PARAM BUFFER io-ledger FOR ar-ledger.

  DEF INPUT PARAM ld AS DEC NO-UNDO.

  RELEASE io-ledger.

  /*IF ld NE 0 THEN */
  FIND io-ledger
      WHERE io-ledger.company  EQ b-ar-inv.company
        AND io-ledger.cust-no  EQ b-ar-inv.cust-no
        AND io-ledger.ref-date EQ b-ar-inv.inv-date
        AND io-ledger.ref-num  EQ "INV# " + STRING(b-ar-inv.inv-no)
      NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-hld-po-no LIKE ar-invl.po-no NO-UNDO.
  DEF VAR ld-diff AS DEC NO-UNDO.
  DEF VAR ld-tax AS DEC NO-UNDO.
  DEF VAR ld-disc AS DEC NO-UNDO.
  DEF VAR ld-inv-qty LIKE ar-invl.inv-qty NO-UNDO.
  DEF VAR ld-inv-qty-2 LIKE ar-invl.inv-qty NO-UNDO.
  DEF VAR v-old-ar-invl-amount AS DEC NO-UNDO.
  DEF VAR v-old-tax-amt AS DEC NO-UNDO.
  DEF VAR v-dscr AS CHAR FORMAT "X(60)" NO-UNDO.
  DEF VAR v-new-amt-wo-disc AS DEC NO-UNDO.
  def var v-tmp-price as dec format ">,>>>,>>9.9999" no-undo.

  /* Code placed here will execute PRIOR to standard behavior. */
  FIND CURRENT b-ar-inv.
  FIND CURRENT b-ar-invl.

  find first itemfg
      {sys/look/itemfgrlW.i}
        and itemfg.i-no eq b-ar-invl.i-no
      no-lock no-error.

  assign
   ld-inv-qty-2 = IF b-ar-invl.misc AND b-ar-invl.inv-qty EQ 0 THEN 1
                  ELSE b-ar-invl.inv-qty
   v-tmp-price = if b-ar-invl.pr-uom begins "L" AND b-ar-invl.pr-uom NE "LB" then
                   if ld-inv-qty-2 lt 0 then -1 else 1
                 else
                 if b-ar-invl.pr-uom eq "CS" then
                    (ld-inv-qty-2 / (if b-ar-invl.cas-cnt ne 0 then b-ar-invl.cas-cnt else
                                    if avail itemfg and itemfg.case-count ne 0
                                                   then itemfg.case-count else
                                                        1))
                 else
                 if b-ar-invl.pr-uom eq "C" then
                    ld-inv-qty-2 / 100
                 else
                 if b-ar-invl.pr-uom eq "M" then
                    ld-inv-qty-2 / 1000
                 else
                    ld-inv-qty-2
     v-old-ar-invl-amount = (v-tmp-price * b-ar-invl.unit-pr) * -1
     v-old-tax-amt = b-ar-inv.tax-amt
     ld-new-diff = 0
     ld-diff     = b-ar-invl.unit-pr
     lv-hld-po-no = b-ar-invl.po-no.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF ar-invl.po-no NE lv-hld-po-no THEN
     FOR EACH oe-boll
        WHERE oe-boll.company EQ ar-invl.company
          AND oe-boll.b-no    EQ ar-invl.b-no
          AND oe-boll.ord-no  EQ ar-invl.ord-no
          AND oe-boll.i-no    EQ ar-invl.i-no
          AND oe-boll.po-no   EQ lv-hld-po-no:
          oe-boll.po-no = ar-invl.po-no.
     END.

  ld-inv-qty = IF ar-invl.misc AND ar-invl.inv-qty EQ 0 THEN 1
               ELSE ar-invl.inv-qty.

  IF ar-invl.bol-no EQ 0 THEN DO:
     IF ar-invl.amt EQ 0 AND ar-invl.misc THEN
        ar-invl.amt = ar-invl.unit-pr * ld-inv-qty.
    
     IF ld-diff NE 0 THEN
        ASSIGN
        ld-diff = ar-invl.unit-pr / ld-diff
        ld-diff = (b-ar-invl.amt * ld-diff) - b-ar-invl.amt.
    
     ELSE ld-diff = ar-invl.amt - b-ar-invl.amt.
  END.

  ELSE DO:

     ld-diff = ld-inv-qty / 1000 * ar-invl.unit-pr.
    
     IF ar-invl.pr-uom BEGINS "L" AND ar-invl.pr-uom NE "LB" THEN
       ld-diff = ar-invl.unit-pr *
                 IF ld-inv-qty LT 0 THEN -1 ELSE 1.
     ELSE
     IF ar-invl.pr-uom EQ "CS" THEN
       ld-diff = ld-inv-qty /
                 (IF ar-invl.cas-cnt NE 0 THEN ar-invl.cas-cnt
                  ELSE
                  IF itemfg.case-count NE 0 THEN
                     itemfg.case-count ELSE 1) *
                 ar-invl.unit-pr.
     ELSE
     IF DYNAMIC-FUNCTION("Conv_IsEAUOM",ar-invl.company, ar-invl.i-no, ar-invl.pr-uom) THEN
       ld-diff = ld-inv-qty * ar-invl.unit-pr.
     ELSE
     FOR EACH uom FIELDS(mult)
         WHERE uom.uom  EQ ar-invl.pr-uom
           AND uom.mult NE 0
         NO-LOCK:
       ld-diff = ld-inv-qty / uom.mult * ar-invl.unit-pr.
    
       LEAVE.
     END.
     ld-diff = ROUND(ld-diff,2).
    
     IF ar-invl.disc NE 0 THEN
        ld-diff = ROUND(ld-diff * (1 - (ar-invl.disc / 100)),2).
    
     ld-diff = ld-diff - b-ar-invl.amt.
  END.

  IF ld-diff EQ ? THEN ld-diff = 0.

  RUN get-ledger (BUFFER ar-ledger, ld-diff).

  IF AVAIL ar-ledger THEN DO:
     SESSION:SET-WAIT-STATE ("general").
    
     ASSIGN
      ld-diff = ROUND(ld-diff,2)
      b-ar-invl.amt    = b-ar-invl.amt + ld-diff
      b-ar-inv.net     = b-ar-inv.net + ld-diff
      b-ar-inv.t-sales = b-ar-inv.net + ld-diff.
    
     IF b-ar-invl.tax AND b-ar-inv.tax-code NE "" THEN DO:
        RUN Tax_Calculate  (
            INPUT  b-ar-inv.company,
            INPUT  b-ar-inv.tax-code,
            INPUT  FALSE,   /* Is this freight */
            INPUT  ld-diff,
            INPUT  b-ar-invl.i-no,
            OUTPUT ld-tax
            ).
    
        b-ar-inv.tax-amt = b-ar-inv.tax-amt + ld-tax.
     END.
    
     ASSIGN
      b-ar-inv.gross = b-ar-inv.gross + ld-diff + ld-tax
                       /*esp changed gross to net in due line*/
      ar-ledger.amt  = - b-ar-inv.net.    
    
     
     IF ld-diff NE 0 /* gdm - 10140906 */
        /* gdm - 07210904 */
        /* IF b-ar-inv.tax-amt = 0 AND */
        /* b-ar-inv.freight = 0 AND */ THEN
        ASSIGN b-ar-inv.due = b-ar-inv.net 
                        + (IF b-ar-inv.frt-pay = "P" 
                            THEN 0 ELSE b-ar-inv.freight)
                        + b-ar-inv.tax-amt - b-ar-inv.paid 
                        - b-ar-inv.disc-taken.
    
     RUN update-cust (ld-diff + ld-tax).
    
     RELEASE itemfg.
     RELEASE fgcat.
    
     IF b-ar-invl.i-no NE "" THEN
     FIND FIRST itemfg NO-LOCK
         {sys/look/itemfgrlW.i}
           AND itemfg.i-no EQ b-ar-invl.i-no
         NO-ERROR.
    
     IF AVAIL itemfg THEN
     FIND FIRST fgcat
         WHERE fgcat.company EQ cocode
           AND fgcat.procat  EQ itemfg.procat
         NO-LOCK NO-ERROR.
    
     FIND FIRST b-cust WHERE
          b-cust.company EQ b-ar-inv.company AND
          b-cust.cust-no EQ b-ar-inv.cust-no
          NO-LOCK NO-ERROR.
    
     ASSIGN
        v-dscr = TRIM(IF AVAIL b-cust THEN b-cust.name ELSE "Cust not on file") +
                 " Inv# " + STRING(b-ar-inv.inv-no,"99999999") + " LINE".
    
     IF ld-diff NE 0 THEN
     DO:
        FIND FIRST gltrans
            WHERE gltrans.company EQ ar-ledger.company
              AND gltrans.actnum  EQ (IF AVAIL fgcat AND fgcat.glacc NE ""
                                      THEN fgcat.glacc ELSE v-ar-sales)    
              AND gltrans.jrnl    EQ "OEINV"
              AND gltrans.tr-date EQ ar-ledger.tr-date
              AND gltrans.period  EQ ar-inv.period
              AND gltrans.trnum   EQ ar-ledger.tr-num
              AND gltrans.tr-dscr EQ v-dscr
              AND gltrans.tr-amt  EQ v-old-ar-invl-amount
            NO-ERROR.
        
        IF NOT AVAIL gltrans THEN
           FIND FIRST gltrans
            WHERE gltrans.company EQ ar-ledger.company
              AND gltrans.actnum  EQ (IF AVAIL fgcat AND fgcat.glacc NE ""
                                      THEN fgcat.glacc ELSE v-ar-sales)    
              AND gltrans.jrnl    EQ "OEINV"
              AND gltrans.tr-date EQ ar-ledger.tr-date
              AND gltrans.period  EQ ar-inv.period
              AND gltrans.trnum   EQ ar-ledger.tr-num
              AND gltrans.tr-dscr EQ v-dscr
            NO-ERROR.
       
        IF AVAIL gltrans THEN
        DO:
           ASSIGN
              ld-inv-qty-2 = IF b-ar-invl.misc AND b-ar-invl.inv-qty EQ 0 THEN 1
                             ELSE b-ar-invl.inv-qty
              v-tmp-price = if b-ar-invl.pr-uom begins "L" AND b-ar-invl.pr-uom NE "LB" then
                               if ld-inv-qty-2 lt 0 then -1 else 1
                            else
                            if b-ar-invl.pr-uom eq "CS" then
                               (ld-inv-qty-2 / (if b-ar-invl.cas-cnt ne 0 then b-ar-invl.cas-cnt else
                                               if avail itemfg and itemfg.case-count ne 0
                                                              then itemfg.case-count else
                                                                   1))
                            else
                            if b-ar-invl.pr-uom eq "C" then
                               ld-inv-qty-2 / 100
                            else
                            if b-ar-invl.pr-uom eq "M" then
                               ld-inv-qty-2 / 1000
                            else
                               ld-inv-qty-2
              v-new-amt-wo-disc = v-tmp-price * b-ar-invl.unit-pr
              ld-new-diff = ld-new-diff + (v-old-ar-invl-amount - (v-new-amt-wo-disc * -1) )
              gltrans.tr-amt = gltrans.tr-amt + (ld-new-diff * -1).
          
           IF AVAIL ar-ledger AND b-ar-invl.disc NE 0 THEN
               
              ASSIGN
              ld-disc = ( v-new-amt-wo-disc - ROUND(v-new-amt-wo-disc * (1 - (b-ar-invl.disc / 100)),2))  -
                        ( (v-old-ar-invl-amount * -1) - (ROUND((v-old-ar-invl-amount * -1) * (1 - (b-ar-invl.disc / 100)),2)))
              ld-new-diff = ld-new-diff - ld-disc.
       
           IF ld-tax NE 0 THEN
             RUN update-gl-tax (b-ar-inv.tax-amt, v-old-tax-amt).
       
           IF ld-disc NE 0 THEN DO:
       
             FIND FIRST gltrans
                 WHERE gltrans.company EQ ar-ledger.company
                   AND gltrans.actnum  EQ v-ar-disc   
                   AND gltrans.jrnl    EQ "OEINV"
                   AND gltrans.tr-date EQ ar-ledger.tr-date
                   AND gltrans.period  EQ ar-inv.period
                   AND gltrans.trnum   EQ ar-ledger.tr-num
                   AND gltrans.tr-dscr EQ "ORDER ENTRY INVOICE DISCOUNT"
                 NO-ERROR.
       
             IF AVAIL gltrans THEN
                gltrans.tr-amt = gltrans.tr-amt + ld-disc.
           END.
          
           RUN update-gl-tot (ld-new-diff).
        END. /*if avail gltrans*/
        ELSE /*arinv*/
        DO:
           FIND FIRST gltrans
            WHERE gltrans.company EQ ar-ledger.company
              AND gltrans.actnum  EQ ar-invl.actnum    
              AND gltrans.jrnl    EQ "ARINV"
              AND gltrans.tr-date EQ ar-ledger.tr-date
              AND gltrans.period  EQ ar-inv.period
              AND gltrans.trnum   EQ ar-ledger.tr-num
              AND gltrans.tr-amt  EQ v-old-ar-invl-amount
              AND gltrans.tr-dscr EQ v-dscr
            NO-ERROR.
           
           IF AVAIL gltrans THEN
           DO:
              ASSIGN
                 ld-new-diff = ld-new-diff + (v-old-ar-invl-amount - (ar-invl.amt * -1))
                 gltrans.tr-amt = gltrans.tr-amt + (ld-new-diff * -1).
           
              IF ld-tax <> 0 THEN RUN update-gl-tax (b-ar-inv.tax-amt,v-old-tax-amt).
              RUN update-gl-tot (ld-new-diff).
           END.
        END.
     END.
  END.

  ELSE
     ar-invl.unit-pr = b-ar-invl.unit-pr.

  FIND CURRENT b-ar-inv NO-LOCK.

  SESSION:SET-WAIT-STATE ("").
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
  RUN reset-inv-no.

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
  /*ASSIGN
   begin_cust-no = ""
/*   fi_gross   = 0
   fi_freight = 0
   fi_tax-amt = 0
   fi_name    = ""*/ .*/

  IF AVAIL ar-inv THEN DO WITH FRAME {&FRAME-NAME}:
   /* ASSIGN
     begin_cust-no = ar-inv.cust-no
     /*fi_gross   = ar-inv.gross
     fi_freight = ar-inv.freight
     fi_tax-amt = ar-inv.tax-amt*/ .*/

    FIND FIRST cust
        WHERE cust.company EQ ar-inv.company
          AND cust.cust-no EQ ar-inv.cust-no
        NO-LOCK NO-ERROR.

    /*fi_name = IF AVAIL cust THEN cust.name ELSE "Not on file...".*/

    /*IF ar-inv.tax-code NE "" THEN ENABLE btn_tax.*/

    /*DISABLE fi_tax-amt.*/
  END.

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
    DEFINE VARIABLE rwRowid AS ROWID NO-UNDO .
  /* Code placed here will execute PRIOR to standard behavior. */
  /*RUN valid-inv-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN error.*/

  /*IF op-enable-price EQ NO THEN
     fi_msg:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Use Credit Memo to adjust invoice pricing with Tax/Freight.".
  ELSE
     fi_msg:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".*/

  /* Dispatch standard ADM method.                             */
 /* RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .*/

    RUN ar/d-repstin.w ("update" ,ROWID(ar-inv), ROWID(ar-invl), OUTPUT rwRowid).

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    /*DISABLE fi_inv-no.
    APPLY "entry" TO ar-invl.po-no IN BROWSE {&browse-name}.*/
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
  
  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-sman (0) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-s-pct (0) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  FIND b-ar-inv WHERE ROWID(b-ar-inv) EQ ROWID(ar-inv) NO-LOCK.
  FIND b-ar-invl WHERE ROWID(b-ar-invl) EQ ROWID(ar-invl) NO-LOCK.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND CURRENT b-ar-inv NO-LOCK.
  FIND CURRENT b-ar-invl NO-LOCK.

  RUN dispatch ("display-fields").

  RUN reset-inv-no.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reset-inv-no B-table-Win 
PROCEDURE reset-inv-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
    /*ENABLE fi_inv-no.
    DISABLE fi_tax-amt btn_tax.
    fi_msg:SCREEN-VALUE = "".
    APPLY "entry" TO fi_inv-no.*/
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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  RUN setCellColumns.
  /* Code placed here will execute AFTER standard behavior.    */
                
        /* FI_moveCol = "Sort".

  DISPLAY FI_moveCol WITH FRAME {&FRAME-NAME}.

   APPLY 'ENTRY':U TO fi_i-no IN FRAME {&FRAME-NAME}.*/

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
    IF lCheckFirst THEN DO:
     fi_inv-no = 9999999 .
     /* Delay opening of query until the user makes a selection */
     lCheckFirst = FALSE .
     RETURN.
    END. 

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

   /* Code placed here will execute AFTER standard behavior.    */



  
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
  {src/adm/template/sndkycas.i "company" "eb" "company"}
  {src/adm/template/sndkycas.i "Carrier" "eb" "Carrier"}

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
  {src/adm/template/snd-list.i "ar-inv"}
  {src/adm/template/snd-list.i "ar-invl"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-cust B-table-Win 
PROCEDURE update-cust :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ld AS DEC NO-UNDO.
  
  FIND FIRST cust
       WHERE cust.company EQ ar-inv.company
         AND cust.cust-no EQ ar-inv.cust-no
       NO-ERROR.

  IF AVAIL cust THEN DO:
    ASSIGN
     cust.sales[b-ar-inv.period] = cust.sales[b-ar-inv.period] + ld
     cust.ytd-sales              = cust.ytd-sales + ld

     cust.acc-bal = cust.acc-bal +
                    IF ar-inv.terms EQ "CASH" THEN 0 ELSE ld
     cust.ord-bal = cust.ord-bal -
                    IF b-ar-inv.terms EQ "CASH" THEN 0 ELSE ld.
              
    IF cust.ord-bal LT 0 THEN cust.ord-bal = 0.

    IF cust.acc-bal GE cust.hibal THEN
      ASSIGN
       cust.hibal      = cust.acc-bal
       cust.hibal-date = b-ar-inv.inv-date.

    FIND CURRENT cust NO-LOCK.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-gl-tax B-table-Win 
PROCEDURE update-gl-tax :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-tax-amt AS DEC NO-UNDO.
  DEF INPUT PARAM ip-old-tax-amt AS DEC NO-UNDO.

  DEF VAR v-dscr AS CHAR NO-UNDO.

  FIND FIRST b-cust WHERE
       b-cust.company EQ ar-inv.company AND
       b-cust.cust-no EQ ar-inv.cust-no
       NO-LOCK NO-ERROR.

  v-dscr = TRIM(IF AVAIL b-cust THEN b-cust.name ELSE "Cust not on file") +
         " Inv# " + STRING(ar-inv.inv-no,"99999999") + " LINE".

  FIND FIRST gltrans
      WHERE gltrans.company EQ ar-ledger.company
        AND gltrans.actnum  EQ v-ar-stax   
        AND gltrans.jrnl    EQ "OEINV"
        AND gltrans.tr-date EQ ar-ledger.tr-date
        AND gltrans.period  EQ ar-inv.period
        AND gltrans.trnum   EQ ar-ledger.tr-num
        AND gltrans.tr-dscr EQ v-dscr
      NO-ERROR.

  IF AVAIL gltrans THEN
     ASSIGN
       ld-new-diff = ld-new-diff + (gltrans.tr-amt + ip-tax-amt)
       gltrans.tr-amt = ip-tax-amt * -1.
  ELSE
  DO:
     FIND FIRST stax
          WHERE stax.company EQ ar-inv.company
            AND stax.tax-group = ar-inv.tax-code
          NO-LOCK NO-ERROR.

     IF AVAIL stax THEN DO:

        DEF VAR tot-tax AS DECIMAL NO-UNDO.
        DEF VAR ws_taxacct AS CHAR NO-UNDO.
        DEF VAR last_one AS INTEGER NO-UNDO.
        DEF VAR v-jd-taxamt AS DECIMAL NO-UNDO.
        DEF VAR v-tax-rate AS DECIMAL NO-UNDO DECIMALS 8.
       
        DEF VAR old-tot-tax AS DECIMAL NO-UNDO.
        DEF VAR v-old-jd-taxamt AS DECIMAL NO-UNDO.

        ASSIGN
           tot-tax = ip-tax-amt
           old-tot-tax = ip-old-tax-amt.

        DO i = 1 TO 3:
           v-tax-rate = v-tax-rate + stax.tax-rate[i].
           IF stax.tax-rate[i] NE 0 THEN last_one = i.
        END.

        DO i = 1 TO 3:
           IF stax.tax-rate[i] NE 0 THEN DO:
              FIND account NO-LOCK
                  WHERE account.company = cocode
                    AND account.actnum = stax.tax-acc[i]
                  NO-ERROR.
              ASSIGN
               ws_taxacct  = IF AVAIL account THEN stax.tax-acc[i] ELSE v-ar-stax
               v-jd-taxamt = ROUND((stax.tax-rate[i] / v-tax-rate) * ip-tax-amt,2)
               v-old-jd-taxamt = ROUND((stax.tax-rate[i] / v-tax-rate) * ip-old-tax-amt,2)
               tot-tax     = tot-tax - v-jd-taxamt
               old-tot-tax = old-tot-tax - v-old-jd-taxamt.

              /* add in any residual amount */
              IF i EQ last_one THEN
                 ASSIGN
                    v-jd-taxamt = v-jd-taxamt + tot-tax
                    v-old-jd-taxamt = v-old-jd-taxamt + old-tot-tax.
             
              /*no currency conversion being done*/

              FIND FIRST gltrans WHERE
                   gltrans.company EQ ar-ledger.company AND
                   gltrans.actnum  EQ ws_taxacct AND
                   gltrans.jrnl    EQ "ARINV" AND
                   gltrans.tr-date EQ ar-ledger.tr-date AND
                   gltrans.period  EQ ar-inv.period AND
                   gltrans.trnum   EQ ar-ledger.tr-num AND
                   gltrans.tr-amt  EQ v-old-jd-taxamt * -1 AND
                   gltrans.tr-dscr EQ v-dscr
                   NO-ERROR.

              IF AVAIL gltrans THEN
                 ASSIGN
                    ld-new-diff = ld-new-diff + gltrans.tr-amt + v-jd-taxamt
                    gltrans.tr-amt = v-jd-taxamt * -1.
           END.
        END.
     END.
     ELSE
     DO:
        FIND FIRST gltrans WHERE
             gltrans.company EQ ar-ledger.company AND
             gltrans.actnum  EQ v-ar-stax AND
             gltrans.jrnl    EQ "ARINV" AND
             gltrans.tr-date EQ ar-ledger.tr-date AND
             gltrans.period  EQ ar-inv.period AND
             gltrans.trnum   EQ ar-ledger.tr-num AND
             gltrans.tr-dscr EQ v-dscr
             NO-ERROR.
        
        IF AVAIL gltrans THEN
           ASSIGN                                      
              ld-new-diff = ld-new-diff + gltrans.tr-amt + ip-tax-amt
              gltrans.tr-amt = ip-tax-amt * -1.
     END.
  END.
  
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
      {&BROWSE-NAME}:COLUMN-MOVABLE = v-col-move
         {&BROWSE-NAME}:COLUMN-RESIZABLE = v-col-move
        v-col-move = NOT v-col-move .
       /* FI_moveCol = IF v-col-move = NO THEN "Move" ELSE "Sort".
     DISPLAY FI_moveCol.*/
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-gl-tot B-table-Win 
PROCEDURE update-gl-tot :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ld AS DEC NO-UNDO.

  FIND FIRST gltrans
      WHERE gltrans.company EQ ar-ledger.company
        AND gltrans.actnum  EQ v-ar-acct   
        AND gltrans.jrnl    EQ "OEINV"
        AND gltrans.tr-date EQ ar-ledger.tr-date
        AND gltrans.period  EQ ar-inv.period
        AND gltrans.trnum   EQ ar-ledger.tr-num
      NO-ERROR.

  IF AVAIL gltrans THEN
     gltrans.tr-amt = gltrans.tr-amt + ld.
  ELSE /*invoice entered from au1*/
  DO:
     FIND FIRST gltrans WHERE
          gltrans.company EQ ar-ledger.company AND
          gltrans.actnum  EQ v-ar-acct AND
          gltrans.jrnl    EQ "ARINV" AND
          gltrans.tr-date EQ ar-ledger.tr-date AND
          gltrans.period  EQ ar-inv.period AND
          gltrans.trnum   EQ ar-ledger.tr-num
          NO-ERROR.

     IF AVAIL gltrans THEN
        gltrans.tr-amt = gltrans.tr-amt + ld.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateRecord B-table-Win 
PROCEDURE pUpdateRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE rwRowid    AS ROWID   NO-UNDO.
    DEFINE VARIABLE rwRowid2   AS ROWID   NO-UNDO.   
    DEFINE VARIABLE lCancelBtn AS LOGICAL NO-UNDO.

    rwRowid2 = ROWID(ar-inv).
    RUN ar/d-repstin.w ("update", ROWID(ar-inv), ROWID(ar-invl), OUTPUT rwRowid, OUTPUT lCancelBtn).
    IF NOT lCancelBtn THEN DO:
        RUN dispatch ("open-query").
        DO WITH FRAME {&FRAME-NAME}:
            REPOSITION {&BROWSE-NAME} TO ROWID rwRowid2.            
        END.        
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateAllRecord B-table-Win 
PROCEDURE pUpdateAllRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE rwRowid AS ROWID NO-UNDO .
   DEFINE VARIABLE lCancelBtn AS LOGICAL NO-UNDO .
   DEFINE BUFFER  bf-ar-invl FOR ar-invl .
   DEFINE BUFFER  bf-ar-inv FOR ar-inv .
   DEFINE VARIABLE cSman1 AS CHARACTER NO-UNDO .
   DEFINE VARIABLE cSman2 AS CHARACTER NO-UNDO .
   DEFINE VARIABLE cSman3 AS CHARACTER NO-UNDO .
   DEFINE VARIABLE dSales1 AS DECIMAL NO-UNDO .
   DEFINE VARIABLE dSales2 AS DECIMAL NO-UNDO .
   DEFINE VARIABLE dSales3 AS DECIMAL NO-UNDO .
   DEFINE VARIABLE dComm1 AS DECIMAL NO-UNDO .
   DEFINE VARIABLE dComm2 AS DECIMAL NO-UNDO .
   DEFINE VARIABLE dComm3 AS DECIMAL NO-UNDO .
   DEFINE VARIABLE rwRowid2 AS ROWID NO-UNDO .
  
   rwRowid2 = ROWID(ar-inv) .

    RUN ar/d-repstin.w ("UpdateAll" ,ROWID(ar-inv), ROWID(ar-invl), OUTPUT rwRowid, OUTPUT lCancelBtn).
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST bf-ar-invl NO-LOCK 
        WHERE rowid(bf-ar-invl) EQ rwRowid NO-ERROR .
    IF AVAIL bf-ar-invl AND NOT lCancelBtn THEN do:
        ASSIGN
        cSman1  =  bf-ar-invl.sman[1]    
        dSales1 =  bf-ar-invl.s-pct[1]   
        dComm1  =  bf-ar-invl.s-comm[1]
        cSman2  =  bf-ar-invl.sman[2]    
        dSales2 =  bf-ar-invl.s-pct[2]  
        dComm2  =  bf-ar-invl.s-comm[2]
        cSman3  =  bf-ar-invl.sman[3]    
        dSales3 =  bf-ar-invl.s-pct[3]   
        dComm3  =  bf-ar-invl.s-comm[3]. 

    FOR EACH bf-ar-inv NO-LOCK
        WHERE bf-ar-inv.company eq cocode 
          and (bf-ar-inv.inv-no GE fi_inv-no ) 
          AND (bf-ar-inv.inv-no LE end_inv-no OR end_inv-no EQ 0 )
          AND (bf-ar-inv.inv-date GE date(begin_inv-date:SCREEN-VALUE)) 
          AND (bf-ar-inv.inv-date LE date(end_inv-date:SCREEN-VALUE) ) 
          AND (bf-ar-inv.cust-no GE begin_cust-no ) 
          AND (bf-ar-inv.cust-no LE end_cust-no OR end_cust-no EQ "")  , 
         EACH bf-ar-invl WHERE bf-ar-invl.x-no eq bf-ar-inv.x-no AND (bf-ar-invl.sman[1] GE begin_sman) 
        AND (bf-ar-invl.sman[1] LE end_sman OR end_sman EQ "") 
         AND ROWID(bf-ar-invl) NE rwRowid EXCLUSIVE-LOCK :
        
        ASSIGN
           bf-ar-invl.sman[1]    = cSman1 
           bf-ar-invl.s-pct[1]   = dSales1
           bf-ar-invl.s-comm[1]  = dComm1 
           bf-ar-invl.sman[2]    = cSman2 
           bf-ar-invl.s-pct[2]   = dSales2
           bf-ar-invl.s-comm[2]  = dComm2 
           bf-ar-invl.sman[3]    = cSman3 
           bf-ar-invl.s-pct[3]   = dSales3
           bf-ar-invl.s-comm[3]  = dComm3 .
   END.
   RELEASE bf-ar-invl .

   RUN dispatch ("open-query").

       REPOSITION {&browse-name} TO ROWID rwRowid2 NO-ERROR  .
   END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pExportRecord B-table-Win 
PROCEDURE pExportRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE rwRowid AS ROWID NO-UNDO .
   DO WITH FRAME {&FRAME-NAME}:
        ASSIGN {&displayed-objects}.

   IF AVAIL ar-inv THEN
       RUN arinq/rd-invexp.w (begin_cust-no:SCREEN-VALUE ,int(fi_inv-no:SCREEN-VALUE),"","","",begin_inv-date:SCREEN-VALUE, end_cust-no:SCREEN-VALUE,int(end_inv-no:SCREEN-VALUE),"","","",end_inv-date:SCREEN-VALUE ,"AU5").
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-inv-no B-table-Win 
PROCEDURE valid-inv-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR lv-msg AS cha NO-UNDO.

   op-enable-price = YES.
   l-enable-price  = YES .

   FIND FIRST b-ar-inv
        WHERE b-ar-inv.company EQ cocode
          AND b-ar-inv.inv-no  EQ fi_inv-no
          AND b-ar-inv.posted  EQ YES
          AND b-ar-inv.terms   NE "CASH"
        NO-LOCK NO-ERROR.   
   IF NOT AVAIL b-ar-inv THEN lv-msg = "Invoice is invalid".
   ELSE
      IF b-ar-inv.tax-amt <> 0 OR 
        (b-ar-inv.frt-pay <> "P" AND b-ar-inv.freight <> 0) THEN
        op-enable-price = NO.
    
    IF lv-msg EQ "" AND op-enable-price EQ YES THEN DO:
      FIND ar-ledger NO-LOCK
          WHERE ar-ledger.company  EQ b-ar-inv.company
            AND ar-ledger.cust-no  EQ b-ar-inv.cust-no
            AND ar-ledger.ref-date EQ b-ar-inv.inv-date
            AND ar-ledger.ref-num  EQ "INV# " + STRING(b-ar-inv.inv-no)
          NO-ERROR.
      IF NOT AVAIL ar-ledger THEN
        lv-msg = "AR Ledger does not exist for this invoice...".
    END.

    IF lv-msg EQ "" AND op-enable-price EQ YES THEN DO:
      FIND FIRST period
          WHERE period.company EQ ar-ledger.company
            AND period.pst     LE ar-ledger.tr-date
            AND period.pend    GE ar-ledger.tr-date
          NO-LOCK NO-ERROR.
      IF NOT AVAIL period THEN
       /* lv-msg = "No period exists for " + STRING(ar-ledger.tr-date).*/
          l-enable-price = NO .
    END.

    IF lv-msg EQ "" AND op-enable-price EQ YES THEN
      IF period.pstat EQ NO OR
         NOT CAN-FIND(FIRST gltrans
                      WHERE gltrans.company EQ ar-ledger.company
                        AND gltrans.trnum   EQ ar-ledger.tr-num) THEN
                    l-enable-price = NO . 
       /*lv-msg = "Sorry, you must reopen year/period "                     +
                STRING(period.yr,"9999") + "/" + STRING(period.pnum,"99") +
                " to change invoice".*/

    IF lv-msg NE "" THEN DO:
      MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fi_inv-no IN FRAME {&FRAME-NAME}.
      RETURN error.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-s-pct B-table-Win 
PROCEDURE valid-s-pct :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-int AS INT NO-UNDO.

  DEF VAR ld-pct AS DEC NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.

   
  DO WITH FRAME {&FRAME-NAME}:
    ld-pct = IF ip-int EQ 1 THEN DEC(ar-invl.s-pct[1]:SCREEN-VALUE IN BROWSE {&browse-name})
             ELSE
             IF ip-int EQ 2 THEN DEC(ar-invl.s-pct[2]:SCREEN-VALUE IN BROWSE {&browse-name})
             ELSE
             IF ip-int EQ 3 THEN DEC(ar-invl.s-pct[3]:SCREEN-VALUE IN BROWSE {&browse-name})
             ELSE (DEC(ar-invl.s-pct[1]:SCREEN-VALUE IN BROWSE {&browse-name}) +
                   DEC(ar-invl.s-pct[2]:SCREEN-VALUE IN BROWSE {&browse-name}) +
                   DEC(ar-invl.s-pct[3]:SCREEN-VALUE IN BROWSE {&browse-name})).

    IF (ar-invl.sman[1]:SCREEN-VALUE IN BROWSE {&browse-name} NE "" OR
        ar-invl.sman[2]:SCREEN-VALUE IN BROWSE {&browse-name} NE "" OR
        ar-invl.sman[3]:SCREEN-VALUE IN BROWSE {&browse-name} NE "")   AND
       ((ip-int EQ 0 AND ld-pct NE 100) OR
        (ip-int NE 0 AND ld-pct GT 100)) THEN DO:
      IF ip-int EQ 0 THEN
        MESSAGE "Item's Sales Rep Commission % of Sales does not equal 100%, continue?" SKIP(1)
                "(Please Note: Yes will result in inaccurate totals on some Sales History Reports)"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE ll.
      ELSE
        MESSAGE "Sales Rep Commission % of Sales is over 100%..."
            VIEW-AS ALERT-BOX ERROR.
      IF NOT ll THEN DO:
        IF ip-int EQ 3 THEN APPLY "entry" TO ar-invl.s-pct[3].
        ELSE
        IF ip-int EQ 2 THEN APPLY "entry" TO ar-invl.s-pct[2].
                       ELSE APPLY "entry" TO ar-invl.s-pct[1].
        RETURN ERROR.
      END.
    END.
  END.
      
  /*DEF INPUT PARAM ip-int AS INT NO-UNDO.

  DEF VAR ld-pct AS DEC NO-UNDO.

   
  DO WITH FRAME {&FRAME-NAME}:
    ld-pct = IF ip-int EQ 1 THEN DEC(ar-invl.s-pct[1]:SCREEN-VALUE IN BROWSE {&browse-name})
             ELSE
             IF ip-int EQ 2 THEN DEC(ar-invl.s-pct[2]:SCREEN-VALUE IN BROWSE {&browse-name})
             ELSE
             IF ip-int EQ 3 THEN DEC(ar-invl.s-pct[3]:SCREEN-VALUE IN BROWSE {&browse-name})
             ELSE (DEC(ar-invl.s-pct[1]:SCREEN-VALUE IN BROWSE {&browse-name}) +
                   DEC(ar-invl.s-pct[2]:SCREEN-VALUE IN BROWSE {&browse-name}) +
                   DEC(ar-invl.s-pct[3]:SCREEN-VALUE IN BROWSE {&browse-name})).

    IF (ar-invl.sman[1]:SCREEN-VALUE IN BROWSE {&browse-name} NE "" OR
        ar-invl.sman[2]:SCREEN-VALUE IN BROWSE {&browse-name} NE "" OR
        ar-invl.sman[3]:SCREEN-VALUE IN BROWSE {&browse-name} NE "")   AND
       ((ip-int EQ 0 AND ld-pct NE 100) OR
        (ip-int NE 0 AND ld-pct GT 100)) THEN DO:
      MESSAGE "% of Sales for all salesmen must total 100..." VIEW-AS ALERT-BOX ERROR.
      IF ip-int EQ 3 THEN APPLY "entry" TO ar-invl.s-pct[3] IN BROWSE {&browse-name}.
      ELSE
      IF ip-int EQ 2 THEN APPLY "entry" TO ar-invl.s-pct[2] IN BROWSE {&browse-name}.
                     ELSE APPLY "entry" TO ar-invl.s-pct[1] IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-sman B-table-Win 
PROCEDURE valid-sman :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-int AS INT NO-UNDO.

  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-sman LIKE sman.sman NO-UNDO.


  li = ip-int.

  IF li EQ 0 THEN
    ASSIGN
     ip-int = 1
     li     = 3.

  DO ip-int = ip-int TO li WITH FRAME {&FRAME-NAME}:
    lv-sman = IF ip-int EQ 3 THEN ar-invl.sman[3]:SCREEN-VALUE IN BROWSE {&browse-name}
              ELSE
              IF ip-int EQ 2 THEN ar-invl.sman[2]:SCREEN-VALUE IN BROWSE {&browse-name}
                             ELSE ar-invl.sman[1]:SCREEN-VALUE IN BROWSE {&browse-name}.
    
    IF lv-sman NE "" THEN DO:
      IF NOT CAN-FIND(FIRST sman
                      WHERE sman.company  EQ cocode
                        AND sman.sman     EQ lv-sman
                        AND sman.inactive EQ NO) THEN DO:
        MESSAGE "Inactive/Invalid Sales Rep, try help..." VIEW-AS ALERT-BOX ERROR.
        IF ip-int EQ 3 THEN APPLY "entry" TO ar-invl.sman[3] IN BROWSE {&browse-name}.
        ELSE
        IF ip-int EQ 2 THEN APPLY "entry" TO ar-invl.sman[2] IN BROWSE {&browse-name}.
                       ELSE APPLY "entry" TO ar-invl.sman[1] IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.
    END.

    ELSE DO:
      IF ip-int EQ 3 THEN
        ASSIGN
         ar-invl.s-pct[3]:SCREEN-VALUE IN BROWSE {&browse-name}  = "0"
         ar-invl.s-comm[3]:SCREEN-VALUE IN BROWSE {&browse-name} = "0".
      ELSE
      IF ip-int EQ 2 THEN
        ASSIGN
         ar-invl.s-pct[2]:SCREEN-VALUE IN BROWSE {&browse-name}  = "0"
         ar-invl.s-comm[2]:SCREEN-VALUE IN BROWSE {&browse-name} = "0".
      ELSE
        ASSIGN
         ar-invl.s-pct[1]:SCREEN-VALUE IN BROWSE {&browse-name}  = "0"
         ar-invl.s-comm[1]:SCREEN-VALUE IN BROWSE {&browse-name} = "0".
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-inv-dscr B-table-Win 
FUNCTION get-inv-dscr RETURNS CHARACTER
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
