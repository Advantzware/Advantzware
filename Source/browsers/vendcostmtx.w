&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  browsers/vendcostmtx.w

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
&SCOPED-DEFINE browseOnly
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{custom/gcompany.i}
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}

DEFINE BUFFER bf-vendItemCost FOR vendItemCost .

&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHAR NO-UNDO.

DEFINE VARIABLE char-hdl                AS CHAR NO-UNDO.
DEFINE VARIABLE columnCount             AS INT  NO-UNDO.
DEFINE VARIABLE idx                     AS INT  NO-UNDO.
DEFINE VARIABLE ilogic                  AS LOG  NO-UNDO.
DEFINE VARIABLE iSecurityLevel          AS INT  NO-UNDO.
DEFINE VARIABLE lActive                 AS LOG  NO-UNDO.
DEFINE VARIABLE ll-first                AS LOG  INIT YES NO-UNDO.
DEFINE VARIABLE ll-show-all             AS LOG  NO-UNDO.
DEFINE VARIABLE ll-sort-asc             AS LOG  INIT YES NO-UNDO.
DEFINE VARIABLE lv-cust-no              AS CHAR NO-UNDO.
DEFINE VARIABLE lv-sort-by              AS CHAR INIT "itemID" NO-UNDO.
DEFINE VARIABLE lv-sort-by-lab          AS CHAR INIT "Item ID" NO-UNDO.
DEFINE VARIABLE lvFirstRowID            AS ROWID NO-UNDO.
DEFINE VARIABLE lvLastRowID             AS ROWID NO-UNDO.
DEFINE VARIABLE phandle                 AS HANDLE NO-UNDO.
DEFINE VARIABLE useColors               AS CHAR NO-UNDO.
DEFINE VARIABLE v-called-setCellColumns AS LOG  NO-UNDO.
DEFINE VARIABLE v-col-move              AS LOG  INIT YES NO-UNDO.
DEFINE VARIABLE cLevel                  AS CHARACTER EXTENT 10 NO-UNDO .
DEFINE VARIABLE dtEffDate               AS DATE FORMAT "99/99/9999" NO-UNDO .
ASSIGN 
    cocode = g_company
    locode = g_loc.

DEFINE VARIABLE cVendItemCostItem# AS CHAR NO-UNDO.
DEFINE VARIABLE cVendItemCostItemType AS CHAR NO-UNDO.
DEFINE VARIABLE cVendItemCostEst# AS CHAR NO-UNDO.
DEFINE VARIABLE cVendItemCostVendor AS CHAR NO-UNDO.
DEFINE VARIABLE cVendItemCostCustomer AS CHAR NO-UNDO.
DEFINE VARIABLE iCount          AS INTEGER NO-UNDO.
DEFINE VARIABLE iVendCostItemID AS INTEGER NO-UNDO.
DEFINE VARIABLE lRecFound       AS LOGICAL NO-UNDO.
DEFINE VARIABLE iRtnInt         AS INTEGER NO-UNDO.

/* this is to get Number of records to load in the browser */
RUN sys/ref/nk1look.p (
    INPUT cocode, 
    INPUT "VendItemBrowse", 
    INPUT "I" /* Integer */, 
    INPUT NO ,
    INPUT NO ,
    INPUT "" /* cust */, 
    INPUT "" /* ship-to*/,
    OUTPUT iRtnInt, 
    OUTPUT lRecFound
    ).
 
FIND FIRST users NO-LOCK WHERE 
    users.user_id EQ USERID(LDBNAME(1)) 
    NO-ERROR.
IF AVAILABLE users THEN ASSIGN 
    iSecurityLevel = users.securityLevel.

&SCOPED-DEFINE key-phrase TRUE

&SCOPED-DEFINE for-each1 ~
    FOR EACH vendItemCost ~
        WHERE {&key-phrase} ~
          AND vendItemCost.company   EQ cocode ~
          AND (vendItemCost.itemType BEGINS cb_itemType OR cb_itemType EQ "ALL") ~
          AND vendItemCost.vendorID  BEGINS fi_vend-no ~
          AND (IF fi_i-no BEGINS '*' THEN vendItemCost.itemID MATCHES (fi_i-no + "*") ~
               ELSE vendItemCost.itemID BEGINS fi_i-no ) ~
          AND (IF TRIM(fi_est-no) BEGINS '*' THEN TRIM(vendItemCost.estimateNo) MATCHES (TRIM(fi_est-no) + "*") ~
               ELSE TRIM(vendItemCost.estimateNo) BEGINS TRIM(fi_est-no)) ~
          AND (IF TRIM(fi_vend-item) BEGINS '*' THEN TRIM(vendItemCost.vendorItemID) MATCHES (TRIM(fi_vend-item) + "*") ~
               ELSE TRIM(vendItemCost.vendorItemID) BEGINS TRIM(fi_vend-item)) ~
          AND (tb_in-est  OR vendItemCost.estimateNo     EQ "") ~
          AND (tb_in-exp  OR (vendItemCost.expirationDate GE TODAY OR vendItemCost.expirationDate = ?)) ~
          AND (tb_fut-eff OR vendItemCost.effectiveDate  LE TODAY) 

&SCOPED-DEFINE for-eachblank ~
    FOR EACH vendItemCost ~
        WHERE vendItemCost.company EQ cocode ~
          AND (tb_in-est  OR vendItemCost.estimateNo     EQ "") ~
          AND (tb_in-exp  OR (vendItemCost.expirationDate GE TODAY OR vendItemCost.expirationDate = ?)) ~
          AND (tb_fut-eff OR vendItemCost.effectiveDate  LE TODAY) ~
          AND {&key-phrase} 

&SCOPED-DEFINE for-each-ExactMatch ~
    FOR EACH vendItemCost ~
        WHERE {&key-phrase} ~
          AND vendItemCost.company   EQ cocode ~
          AND (vendItemCost.itemType BEGINS cb_itemType OR cb_itemType EQ "ALL") ~
          AND (vendItemCost.itemID eq fi_i-no ) ~
          AND (vendItemCost.vendorID  eq fi_vend-no OR fi_vend-no = "") ~
          AND (vendItemCost.vendorItemID  eq fi_vend-item OR fi_vend-item = "") ~
          AND (fi_est-no EQ "" OR TRIM(vendItemCost.estimateNo) eq TRIM(fi_est-no)) ~
          AND (tb_in-est  OR vendItemCost.estimateNo     EQ "") ~
          AND (tb_in-exp  OR (vendItemCost.expirationDate GE TODAY OR vendItemCost.expirationDate = ?)) ~
          AND (tb_fut-eff OR vendItemCost.effectiveDate  LE TODAY)
/*
&SCOPED-DEFINE for-each-ExactMatch ~
    FOR EACH vendItemCost ~
        WHERE {&key-phrase} ~
          AND vendItemCost.company   EQ cocode ~
          AND (vendItemCost.itemID eq fi_i-no )
*/
&SCOPED-DEFINE sortby-log                                                                                                                                  ~
    IF lv-sort-by EQ "itemType"       THEN vendItemCost.itemType ELSE ~
    IF lv-sort-by EQ "itemID"         THEN vendItemCost.itemID ELSE ~
    IF lv-sort-by EQ "vendorID"       THEN vendItemCost.vendorID ELSE ~
    IF lv-sort-by EQ "customerID"     THEN vendItemCost.customerID ELSE ~
    IF lv-sort-by EQ "estimateNo"     THEN vendItemCost.estimateNo ELSE ~
    IF lv-sort-by EQ "effectiveDate"  THEN STRING(YEAR(vendItemCost.effectiveDate), "9999") + STRING(MONTH(vendItemCost.effectiveDate), "99") + STRING(DAY(vendItemCost.effectiveDate), "99") ELSE ~
    IF lv-sort-by EQ "expirationDate" THEN STRING(YEAR(vendItemCost.expirationDate),"9999") + STRING(MONTH(vendItemCost.expirationDate),"99") + STRING(DAY(vendItemCost.expirationDate),"99") ELSE ~
    IF lv-sort-by EQ "vendorItemID" THEN STRING(vendItemCost.vendorItemID) ELSE ""

&SCOPED-DEFINE sortby BY vendItemCost.itemID

&SCOPED-DEFINE sortby-phrase-asc ~
    BY ({&sortby-log}) ~
    {&sortby}

&SCOPED-DEFINE sortby-phrase-desc ~
    BY ({&sortby-log}) DESC ~
    {&sortby}

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
&Scoped-define INTERNAL-TABLES vendItemCost

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table vendItemCost.itemType ~
vendItemCost.itemID vendItemCost.vendorID vendItemCost.customerID ~
vendItemCost.estimateNo get-eff-date() @ dtEffDate ~
vendItemCost.expirationDate vendItemCost.vendorItemID ~
fGetCostLevels() @ cLevel[1] 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH vendItemCost WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH vendItemCost WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table vendItemCost
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table vendItemCost


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tb_exactMatch tb_in-est tb_in-exp ~
cb_itemType fi_i-no fi_vend-no fi_est-no tb_fut-eff fi_vend-item btn_go ~
btn_show Browser-Table 
&Scoped-Define DISPLAYED-OBJECTS tb_exactMatch tb_in-est tb_in-exp ~
cb_itemType fi_i-no fi_vend-no fi_est-no tb_fut-eff fi_vend-item fi_sort-by 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetCostLevels B-table-Win 
FUNCTION fGetCostLevels RETURNS CHARACTER PRIVATE
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-eff-date B-table-Win 
FUNCTION get-eff-date RETURNS DATE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_go 
     LABEL "&Go" 
     SIZE 12 BY 1
     FONT 6.

DEFINE BUTTON btn_show 
     LABEL "&Clear Filter" 
     SIZE 16 BY 1
     FONT 6.

DEFINE VARIABLE cb_itemType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "All","FG","RM" 
     DROP-DOWN-LIST
     SIZE 10 BY 1 TOOLTIP "Select Type Filter" NO-UNDO.

DEFINE VARIABLE fi_est-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Estimate" 
     VIEW-AS FILL-IN 
     SIZE 12.4 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 21.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_sort-by AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34.6 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE fi_vend-item AS CHARACTER FORMAT "X(16)":U 
     LABEL "Vend Item" 
     VIEW-AS FILL-IN 
     SIZE 19.4 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_vend-no AS CHARACTER FORMAT "X(10)":U 
     LABEL "Vendor" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_exactMatch AS LOGICAL INITIAL no 
     LABEL "Exact Match" 
     VIEW-AS TOGGLE-BOX
     SIZE 22.6 BY 1 NO-UNDO.

DEFINE VARIABLE tb_fut-eff AS LOGICAL INITIAL no 
     LABEL "Include Future Effective" 
     VIEW-AS TOGGLE-BOX
     SIZE 29.4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_in-est AS LOGICAL INITIAL yes 
     LABEL "Include Estimate" 
     VIEW-AS TOGGLE-BOX
     SIZE 22.6 BY 1 NO-UNDO.

DEFINE VARIABLE tb_in-exp AS LOGICAL INITIAL no 
     LABEL "Include Expired" 
     VIEW-AS TOGGLE-BOX
     SIZE 22.6 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      vendItemCost SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      vendItemCost.itemType COLUMN-LABEL "Type" FORMAT "x(5)":U
            WIDTH 8 LABEL-BGCOLOR 14
      vendItemCost.itemID FORMAT "x(20)":U LABEL-BGCOLOR 14
      vendItemCost.vendorID FORMAT "x(10)":U WIDTH 13 LABEL-BGCOLOR 14
      vendItemCost.customerID FORMAT "x(10)":U WIDTH 13 LABEL-BGCOLOR 14
      vendItemCost.estimateNo COLUMN-LABEL "Est" FORMAT "x(8)":U
            WIDTH 13 LABEL-BGCOLOR 14
      get-eff-date() @ dtEffDate COLUMN-LABEL "Effective"
      vendItemCost.expirationDate FORMAT "99/99/9999":U LABEL-BGCOLOR 14
      vendItemCost.vendorItemID COLUMN-LABEL "Vendor Item" FORMAT "x(16)":U
            LABEL-BGCOLOR 14
      fGetCostLevels() @ cLevel[1] COLUMN-LABEL "Cost Levels" FORMAT "X(256)":U
            WIDTH 200
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 142.6 BY 15.43
         FONT 2 ROW-HEIGHT-CHARS .76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     tb_exactMatch AT ROW 1.14 COL 24.8
     tb_in-est AT ROW 1.24 COL 79 WIDGET-ID 50
     tb_in-exp AT ROW 1.29 COL 106.6 WIDGET-ID 52
     cb_itemType AT ROW 2.24 COL 6.4 COLON-ALIGNED WIDGET-ID 40
     fi_i-no AT ROW 2.24 COL 22.8 COLON-ALIGNED WIDGET-ID 16
     fi_vend-no AT ROW 2.24 COL 53.6 COLON-ALIGNED WIDGET-ID 2
     fi_est-no AT ROW 2.29 COL 77 COLON-ALIGNED
     tb_fut-eff AT ROW 2.38 COL 106.6 WIDGET-ID 54
     fi_vend-item AT ROW 3.57 COL 53.4 COLON-ALIGNED
     btn_go AT ROW 3.62 COL 1.8 WIDGET-ID 4
     btn_show AT ROW 3.62 COL 15.2 WIDGET-ID 10
     fi_sort-by AT ROW 3.62 COL 75.6 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     Browser-Table AT ROW 4.86 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     "Click on Column Title to Sort" VIEW-AS TEXT
          SIZE 27 BY .95 AT ROW 3.62 COL 113.2 WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 1 
         DEFAULT-BUTTON btn_go WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
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
         HEIGHT             = 19.52
         WIDTH              = 139.
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
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB Browser-Table fi_sort-by F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "2"
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE
       Browser-Table:COLUMN-RESIZABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN fi_sort-by IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       tb_exactMatch:PRIVATE-DATA IN FRAME F-Main     = 
                "parm".

ASSIGN 
       tb_fut-eff:PRIVATE-DATA IN FRAME F-Main     = 
                "parm".

ASSIGN 
       tb_in-est:PRIVATE-DATA IN FRAME F-Main     = 
                "parm".

ASSIGN 
       tb_in-exp:PRIVATE-DATA IN FRAME F-Main     = 
                "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "asi.vendItemCost"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "vendItemCost"
     _FldNameList[1]   > asi.vendItemCost.itemType
"vendItemCost.itemType" "Type" "x(5)" "character" ? ? ? 14 ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.vendItemCost.itemID
"vendItemCost.itemID" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > asi.vendItemCost.vendorID
"vendItemCost.vendorID" ? ? "character" ? ? ? 14 ? ? no ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > asi.vendItemCost.customerID
"vendItemCost.customerID" ? ? "character" ? ? ? 14 ? ? no ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > asi.vendItemCost.estimateNo
"vendItemCost.estimateNo" "Est" ? "character" ? ? ? 14 ? ? no ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"get-eff-date() @ dtEffDate" "Effective" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > asi.vendItemCost.expirationDate
"vendItemCost.expirationDate" ? ? "Date" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > asi.vendItemCost.vendorItemID
"vendItemCost.vendorItemID" "Vendor Item" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"fGetCostLevels() @ cLevel[1]" "Cost Levels" "X(256)" ? ? ? ? ? ? ? no ? no no "200" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
    DEFINE VARIABLE phandle  AS HANDLE NO-UNDO.
    DEFINE VARIABLE char-hdl AS cha    NO-UNDO.
        {methods/run_link.i "container-source" "select-page" "(2)"}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
    /*RUN startSearch.*/
    DEFINE VARIABLE lh-column     AS HANDLE NO-UNDO.
    DEFINE VARIABLE lv-column-nam AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE lv-column-lab AS CHARACTER   NO-UNDO.

    ASSIGN
        lh-column     = {&BROWSE-NAME}:CURRENT-COLUMN 
        lv-column-nam = lh-column:NAME
        lv-column-lab = lh-column:LABEL.

    IF lv-sort-by EQ lv-column-nam THEN ll-sort-asc = NOT ll-sort-asc.
    ELSE
        ASSIGN
            lv-sort-by     = lv-column-nam
            lv-sort-by-lab = lv-column-lab.

    APPLY 'END-SEARCH' TO {&BROWSE-NAME}.
    RUN dispatch ("open-query").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
        /* This ADM trigger code must be preserved in order to notify other
           objects when the browser's current row changes. */
        {src/adm/template/brschnge.i}
    {methods/template/local/setvalue.i}

   RUN dept-pan-image-proc.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Go */
DO:
  
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN
                fi_vend-no                                               
                cb_itemType                                                 
                fi_i-no  
                fi_est-no
                ll-first = NO 
                fi_vend-item
                .                                             
     
            RUN dispatch ("open-query").
    
            GET FIRST Browser-Table .
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_show B-table-Win
ON CHOOSE OF btn_show IN FRAME F-Main /* Show All */
DO:
        DO WITH FRAME {&FRAME-NAME}:
            ll-show-all = YES.
            APPLY "choose" TO btn_go.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_est-no B-table-Win
ON HELP OF fi_est-no IN FRAME F-Main /* Estimate */
DO:
    DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

    RUN windows/l-est.w (g_company,g_loc,"", OUTPUT char-val).
    IF char-val <> "" THEN DO:
        FIND FIRST eb NO-LOCK WHERE RECID(eb) = INT(char-val) NO-ERROR.
        IF AVAIL eb THEN 
            fi_est-no:screen-value = eb.est-no.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-no B-table-Win
ON HELP OF fi_i-no IN FRAME F-Main /* Item */
DO:

  DEFINE VARIABLE cMainField  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cAllFields  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE recRecordID AS RECID     NO-UNDO.

  IF cb_itemType:SCREEN-VALUE EQ "FG" THEN do:
      RUN system/openlookup.p (g_company, "i-no", 0, "", 0, OUTPUT cAllFields, OUTPUT cMainField, OUTPUT recRecordID).
      IF cMainField <> "" THEN fi_i-no:SCREEN-VALUE = cMainField. 
  END.
  ELSE IF cb_itemType:SCREEN-VALUE EQ "RM" THEN DO:
      RUN system/openlookup.p (g_company, "item", 0, "", 0, OUTPUT cAllFields, OUTPUT cMainField, OUTPUT recRecordID).
      IF cMainField <> "" THEN fi_i-no:SCREEN-VALUE = cMainField. 
  END.
  ELSE DO:
      MESSAGE 
        "Please Select Item Type: FG/RM"
      VIEW-AS ALERT-BOX WARNING.
      APPLY "ENTRY":U TO cb_itemType.
  END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_vend-no B-table-Win
ON HELP OF fi_vend-no IN FRAME F-Main /* Vendor */
DO:

  DEFINE VARIABLE cMainField AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cAllFields AS CHARACTER NO-UNDO.
  DEFINE VARIABLE recRecordID AS RECID    NO-UNDO.

  RUN system/openlookup.p (g_company, "vend-no", 0, "", 0, OUTPUT cAllFields, OUTPUT cMainField, OUTPUT recRecordID).
          IF cMainField <> "" THEN fi_vend-no:SCREEN-VALUE = cMainField. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_exactMatch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_exactMatch B-table-Win
ON VALUE-CHANGED OF tb_exactMatch IN FRAME F-Main /* Exact Match */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_fut-eff
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fut-eff B-table-Win
ON VALUE-CHANGED OF tb_fut-eff IN FRAME F-Main /* Include Future Effective */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_in-est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_in-est B-table-Win
ON VALUE-CHANGED OF tb_in-est IN FRAME F-Main /* Include Estimate */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_in-exp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_in-exp B-table-Win
ON VALUE-CHANGED OF tb_in-exp IN FRAME F-Main /* Include Expired */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{methods/ctrl-a_browser.i}
{sys/inc/f3help.i}
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

&SCOPED-DEFINE cellColumnDat browsers-vendcostmtx

{methods/browsers/setCellColumns.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-item B-table-Win 
PROCEDURE add-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dept-pan-image-proc B-table-Win 
PROCEDURE dept-pan-image-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-spec AS LOG NO-UNDO.
   DEF VAR char-hdl AS CHAR NO-UNDO.

   FIND FIRST notes WHERE notes.rec_key = vendItemCost.rec_key
       NO-LOCK NO-ERROR.

   IF AVAIL notes THEN
      v-spec = TRUE.
   ELSE v-spec = FALSE.

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'spec-target':U, OUTPUT char-hdl).

   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN dept-pen-image IN WIDGET-HANDLE(char-hdl) (INPUT v-spec).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-xl B-table-Win 
PROCEDURE export-xl :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE first-util AS CHARACTER NO-UNDO.
    DEFINE VARIABLE last-util  AS CHARACTER NO-UNDO.
  
    IF AVAIL vendItemCost THEN
       RUN po/VendCostExp.w(vendItemCost.itemType,vendItemCost.itemID,vendItemCost.vendorID,vendItemCost.customerID,vendItemCost.estimateNo,vendItemCost.effectiveDate,vendItemCost.expirationDate) .
   ELSE
       RUN po/VendCostExp.w("","","","","","","") .

    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE first-query B-table-Win 
PROCEDURE first-query :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    IF ll-first THEN 
    DO:
        RUN set-defaults.
        RUN query-first.
    END.
    ELSE
        RUN query-go.

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
        cellColumn[idx] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(idx).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getSourceAttributes B-table-Win 
PROCEDURE getSourceAttributes :
/*------------------------------------------------------------------------------
         Purpose:
         Notes:
------------------------------------------------------------------------------*/
    RUN GET-ATTRIBUTE IN adm-broker-hdl ('OneVendItemCost'). 
    cVendItemCostItem# = RETURN-VALUE.
    RUN GET-ATTRIBUTE IN adm-broker-hdl ('OneVendItemCostEst#').    
    cVendItemCostEst# = RETURN-VALUE.       
    RUN get-attribute IN adm-broker-hdl ('OneVendItemCostType' ).
    cVendItemCostItemType = RETURN-VALUE.
    RUN get-attribute IN adm-broker-hdl ('OneVendItemCostVendor' ).
    cVendItemCostVendor = RETURN-VALUE.
    RUN get-attribute IN adm-broker-hdl ('OneVendItemCostCustomer' ).  
    cVendItemCostCustomer = RETURN-VALUE.      

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE import-excel B-table-Win 
PROCEDURE import-excel :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    APPLY 'ENTRY':U TO BROWSE {&BROWSE-NAME}.

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
  
    DO WITH FRAME {&FRAME-NAME}:
        fi_sort-by:SCREEN-VALUE = TRIM(lv-sort-by-lab)               + " " +
            TRIM(STRING(ll-sort-asc,"As/Des")) + "cending".
        cb_itemType:SCREEN-VALUE = cb_itemType .                   
    END.   
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-get-first B-table-Win 
PROCEDURE local-get-first :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'get-first':U ) .

/* Code placed here will execute AFTER standard behavior.    */
{methods/template/local/setvalue.i}

 RUN dept-pan-image-proc.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-get-last B-table-Win 
PROCEDURE local-get-last :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'get-last':U ) .

/* Code placed here will execute AFTER standard behavior.    */
{methods/template/local/setvalue.i}
RUN dept-pan-image-proc.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-get-next B-table-Win 
PROCEDURE local-get-next :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'get-next':U ) .

/* Code placed here will execute AFTER standard behavior.    */
 {methods/template/local/setvalue.i}
RUN dept-pan-image-proc.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-get-prev B-table-Win 
PROCEDURE local-get-prev :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'get-prev':U ) .

/* Code placed here will execute AFTER standard behavior.    */
{methods/template/local/setvalue.i}
RUN dept-pan-image-proc.

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
   
/*    cb_itemType:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cb_itemType:ENTRY(1) .*/

    APPLY 'ENTRY':U TO fi_i-no IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE li AS INTEGER NO-UNDO.    
    iCount=0.
    
    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

    /* Code placed here will execute AFTER standard behavior.    */

    IF ll-show-all THEN 
    DO:
        RUN set-defaults.

        &SCOPED-DEFINE open-query                   ~
            OPEN QUERY {&browse-name}               ~
                {&for-eachblank}  NO-LOCK

        IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                   ELSE {&open-query} {&sortby-phrase-desc}.
    END.
    ELSE RUN first-query.

    IF AVAILABLE {&first-table-in-query-{&browse-name}} THEN 
    DO:
        RUN dispatch ("display-fields").
        RUN dispatch ("row-changed").

    END.
    IF ll-first THEN DO:
        RUN GET-ATTRIBUTE IN adm-broker-hdl ('OneVendItemCost'). 
        cVendItemCostItem# = RETURN-VALUE.
        RUN GET-ATTRIBUTE IN adm-broker-hdl ('OneVendItemCostEst#').        
        IF (cVendItemCostItem# NE "" AND cVendItemCostItem# NE ?) or
           (RETURN-VALUE <> "" AND RETURN-VALUE <> ?) THEN DO:    
                             
           RUN openqueryOne (cVendItemCostItem#).                  
        END.     
    END.     
    ll-show-all = NO .
  
    APPLY "value-changed" TO BROWSE {&browse-name}.
  

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
    IF AVAILABLE vendItemCost THEN APPLY "value-changed" TO BROWSE {&browse-name}.

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
            Browser-Table:COLUMN-MOVABLE   = v-col-move
            Browser-Table:COLUMN-RESIZABLE = v-col-move
            v-col-move                     = NOT v-col-move.
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
    DEFINE INPUT PARAMETER ipNavType AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opNavType AS CHARACTER NO-UNDO.
 
    IF ipNavType NE '' THEN
        CASE ipNavType:
            WHEN 'F' THEN RUN dispatch ('get-first':U).
            WHEN 'L' THEN RUN dispatch ('get-last':U).
            WHEN 'N' THEN RUN dispatch ('get-next':U).
            WHEN 'P' THEN RUN dispatch ('get-prev':U).
            WHEN 'G' THEN RUN lookup-eb.
        END CASE.
    
    IF ROWID(vendItemCost) EQ lvLastRowID THEN
        opNavType = 'L'.
      
    IF ROWID(vendItemCost) EQ lvFirstRowID THEN
        opNavType = IF opNavType EQ 'L' THEN 'B' ELSE 'F'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openQueryOne B-table-Win 
PROCEDURE openQueryOne :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcValue AS CHARACTER NO-UNDO.
    iCount = 0.
    tb_exactMatch = YES.
     
    RUN get-attribute IN adm-broker-hdl ('OneVendItemCostVendor').
/*    fi_vend-no = IF RETURN-VALUE <> ? THEN RETURN-VALUE ELSE "".*/
    
    RUN get-attribute IN adm-broker-hdl ('OneVendItemCostType').
    IF RETURN-VALUE NE ? THEN
    ASSIGN
        cb_itemType = RETURN-VALUE  
        cb_itemType:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cb_itemType
        .                    
    RUN get-attribute IN adm-broker-hdl ('OneVendItemCostEst#').
    fi_est-no = IF RETURN-VALUE EQ ? THEN "" ELSE return-value.
    IF fi_est-no NE "" THEN ASSIGN tb_in-est = YES.
        
    ASSIGN fi_i-no = IF ipcValue EQ ? THEN "" ELSE ipcValue.
/*    DISPLAY fi_i-no fi_vend-no cb_itemType fi_est-no WITH FRAME {&frame-name}.*/
/*    RUN query-go.*/

    {&for-each-ExactMatch} NO-LOCK
     USE-INDEX vendItemCostID:
         ASSIGN
             iCount          = iCount + 1
             iVendCostItemID = VendItemCost.VendItemCostID
             .
        IF iCount GE iRtnInt THEN
            LEAVE.
     END.
     &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
            {&for-each-ExactMatch}                          ~
            AND VendItemCost.VendItemCostID LE iVendCostItemID NO-LOCK ~
            USE-INDEX vendItemCostID

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
                    
     RUN dispatch ('display-fields').    
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE query-first B-table-Win 
PROCEDURE query-first :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE li AS INTEGER NO-UNDO.
    iCount = 0.
    
    {&for-each1} NO-LOCK
    USE-INDEX vendItemCostID:
        ASSIGN
            iCount          = iCount + 1
            iVendCostItemID = VendItemCost.VendItemCostID
            .
        IF iCount GE iRtnInt THEN
            LEAVE.
    END.

    &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
            {&for-each1}                          ~
            AND VendItemCost.VendItemCostID LE iVendCostItemID NO-LOCK ~
            USE-INDEX vendItemCostID
            
    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE query-go B-table-Win 
PROCEDURE query-go :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE li AS INTEGER NO-UNDO.
    iCount = 0.
    DO WITH FRAME {&frame-name} :
      ASSIGN fi_i-no tb_exactMatch tb_in-est. 
    END.
    
    IF tb_exactMatch THEN DO:
    
        {&for-each-ExactMatch} NO-LOCK
         USE-INDEX vendItemCostID:
             ASSIGN
                 iCount          = iCount + 1
                  iVendCostItemID = VendItemCost.VendItemCostID
                 .
            IF iCount GE iRtnInt THEN
                LEAVE.
         END.
         &SCOPED-DEFINE open-query                   ~
             OPEN QUERY {&browse-name}               ~
                 {&for-each-ExactMatch}                          ~
                  AND VendItemCost.VendItemCostID LE iVendCostItemID NO-LOCK ~
                  USE-INDEX vendItemCostID
    
          IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                        ELSE {&open-query} {&sortby-phrase-desc}. 
     END.
     ELSE DO: 
         {&for-each1} NO-LOCK
          USE-INDEX vendItemCostID:
              ASSIGN
                  iCount          = iCount + 1
                  iVendCostItemID = VendItemCost.VendItemCostID
                  .
              IF iCount GE iRtnInt THEN
                  LEAVE.
          END.

          &SCOPED-DEFINE open-query                   ~
              OPEN QUERY {&browse-name}               ~
                  {&for-each1}                          ~
                   AND VendItemCost.VendItemCostID LE iVendCostItemID NO-LOCK ~
                   USE-INDEX vendItemCostID
    
          IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                        ELSE {&open-query} {&sortby-phrase-desc}.
          
      END.
   
/*    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.  */
/*                    ELSE {&open-query} {&sortby-phrase-desc}.*/
  

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
 
    RUN dispatch IN this-procedure ("open-query").
  
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.

    RUN dispatch IN this-procedure ("row-changed").
    APPLY "value-changed" TO BROWSE {&browse-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query2 B-table-Win 
PROCEDURE repo-query2 :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

    DEFINE VARIABLE li AS INTEGER NO-UNDO.

    RUN set-defaults.
    
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.

    RUN dispatch IN this-procedure ("row-changed").
 
    APPLY "value-changed" TO BROWSE {&browse-name}.

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
  {src/adm/template/snd-list.i "vendItemCost"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-defaults B-table-Win 
PROCEDURE set-defaults :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            fi_vend-no:SCREEN-VALUE  = ""
            cb_itemType:SCREEN-VALUE = "ALL"
            fi_i-no:SCREEN-VALUE     = ""
            fi_est-no:SCREEN-VALUE   = "" 
            fi_vend-no
            cb_itemType
            fi_i-no
            fi_est-no
            tb_exactMatch = NO
            tb_exactMatch:SCREEN-VALUE = "no"
            tb_in-est = YES
            tb_in-est:SCREEN-VALUE = "Yes"
            fi_vend-item:SCREEN-VALUE = ""
            .     
    END.

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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setBrowseFocus B-table-Win 
PROCEDURE setBrowseFocus :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    APPLY 'ENTRY':U TO BROWSE {&BROWSE-NAME}.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE value-changed-proc B-table-Win 
PROCEDURE value-changed-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
       IF AVAIL vendItemCost THEN do:
           RUN dept-pan-image-proc.
       END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetCostLevels B-table-Win 
FUNCTION fGetCostLevels RETURNS CHARACTER PRIVATE
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cCostLevels AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dQuantity   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iIndex      AS INTEGER NO-UNDO.
    
    cCostLevels = IF vendItemCost.useQuantityFromBase THEN "From (" + vendItemCost.vendorUOM + "):"
                  ELSE "Up To (" + vendItemCost.vendorUOM + "):".
    iIndex = 1. 
    FOR EACH vendItemCostLevel NO-LOCK
        WHERE vendItemCostLevel.vendItemCostID EQ vendItemCost.vendItemCostID 
        BY vendItemCostLevel.quantityBase: 
        IF iIndex LE 6 THEN DO:
            IF iIndex LE 5 THEN DO:  
                dQuantity = IF vendItemCost.useQuantityFromBase THEN vendItemCostLevel.quantityFrom ELSE vendItemCostLevel.quantityTo.
                     
                cCostLevels =  cCostLevels + (IF iIndex EQ 1 THEN "" ELSE ", ") + TRIM(STRING(dQuantity,">>>>>>>>>>9.99<<<<"))
                               + (IF vendItemCostLevel.useForBestCost THEN " (B)" ELSE "")
                               + " - $" + TRIM(STRING(vendItemCostLevel.costPerUOM)) .                      
            END.            
            IF iIndex EQ 6 THEN 
                cCostLevels = cCostLevels + ", + ..." .               
            iIndex = iIndex + 1.           
        END.
    END.
    RETURN cCostLevels. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-eff-date B-table-Win 
FUNCTION get-eff-date RETURNS DATE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dtReturn AS DATE NO-UNDO .
  
  IF vendItemCost.effectiveDate LT 01/01/1900 THEN
      ASSIGN dtReturn = 01/01/1900 .
  ELSE dtReturn = vendItemCost.effectiveDate .
  
  RETURN dtReturn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

