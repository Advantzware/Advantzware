&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  oe\b-ordlt.w

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

&SCOPED-DEFINE yellowColumnsName oe-ordl
&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/var.i new shared }
{oe/relemail.i NEW}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEFINE NEW SHARED BUFFER xoe-ord FOR oe-ord.
DEFINE NEW SHARED VARIABLE save_id AS RECID NO-UNDO.  /* RECORD ID FOR ORDER LINE */
DEFINE NEW SHARED VARIABLE v-i-item LIKE oe-ordl.i-no NO-UNDO. /* INPUT ITEM */
DEFINE NEW SHARED VARIABLE v-i-qty LIKE oe-ordl.qty NO-UNDO. /* INPUT QUANTITY */
DEFINE NEW SHARED VARIABLE price-ent AS LOG NO-UNDO.
DEFINE NEW SHARED VARIABLE fil_id AS RECID NO-UNDO. 
DEFINE NEW SHARED VARIABLE nufile AS LOG NO-UNDO.
DEFINE NEW SHARED VARIABLE v-qty-mod AS LOG NO-UNDO.
DEFINE NEW SHARED VARIABLE v-create-job AS LOG NO-UNDO.
DEFINE VARIABLE lv-ordl-recid AS RECID NO-UNDO.
DEFINE VARIABLE lv-change-prom-date AS LOG NO-UNDO.  /* flag for updating oe-ordl.prom-date*/
DEFINE VARIABLE lv-uom-list AS cha INIT "M,EA,L,CS,C" NO-UNDO.
DEFINE VARIABLE cDueDateChangeRsn AS CHARACTER NO-UNDO.

/* ==== FOR REPRICE ===*/
DEFINE NEW SHARED VARIABLE v-procat LIKE oe-prmtx.procat NO-UNDO. /* ITEM CATEGORY */
DEFINE NEW SHARED VARIABLE v-price-lev AS INTEGER NO-UNDO.

DEFINE NEW SHARED BUFFER xest    FOR est.
DEFINE NEW SHARED BUFFER xef     FOR ef.
DEFINE NEW SHARED BUFFER xeb FOR eb.

DEFINE BUFFER b-job-hdr FOR job-hdr.

{ce/print4.i "new shared"}
{ce/print42.i "new shared"}

DEFINE NEW SHARED VARIABLE lv-qty AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE qty AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE v-shared-rel AS INTEGER NO-UNDO.

DEFINE VARIABLE ll-canceled AS LOG NO-UNDO.
DEFINE VARIABLE v-out-rowid-list AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-rowid-list AS CHARACTER NO-UNDO.

/* =-=---- for oe-comm.p ---- */
DEFINE SHARED VARIABLE v-misc AS LOG INIT NO NO-UNDO.
DEFINE SHARED VARIABLE v-fr-tax LIKE oe-ctrl.f-tax NO-UNDO.
/* =   for release/bol =====*/
DEFINE NEW SHARED BUFFER xoe-ordl FOR oe-ordl.
DEFINE NEW SHARED VARIABLE out-recid AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE relh-recid AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE v-auto AS LOG NO-UNDO.
DEFINE VARIABLE lv-passwd AS cha NO-UNDO.
DEFINE VARIABLE ld-price LIKE oe-ordl.price NO-UNDO.
DEFINE VARIABLE ld-pr-uom LIKE oe-ordl.pr-uom NO-UNDO.
DEFINE VARIABLE ld-ext-price LIKE oe-ordl.t-price NO-UNDO.
DEFINE VARIABLE oeBolPrompt-char AS CHARACTER NO-UNDO .
DEFINE VARIABLE oeBolPrompt-log AS LOGICAL NO-UNDO .
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO .
DEFINE VARIABLE clvtext AS CHARACTER NO-UNDO .

&SCOPED-DEFINE SORTBY-PHRASE BY oe-ordl.set-hdr-line BY oe-ordl.line

{fg/fullset.i NEW}

DEFINE TEMP-TABLE w-rel NO-UNDO
    FIELD w-rowid AS ROWID
    FIELD w-scode LIKE oe-rell.s-code
    FIELD fob AS CHARACTER.

DEFINE TEMP-TABLE tt-oe-ordl NO-UNDO LIKE oe-ordl.

DEFINE BUFFER b-fob FOR reftable.
    
{oe/tt-item-qty-price.i}

RUN sys/ref/nk1look.p (INPUT cocode, "OEBOLPrompt", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
     oeBolPrompt-log = LOGICAL(cRtnChar) NO-ERROR.

RUN sys/ref/nk1look.p (INPUT cocode, "OEBOLPrompt", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    oeBolPrompt-char = cRtnChar NO-ERROR. 

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
&Scoped-define EXTERNAL-TABLES oe-ord
&Scoped-define FIRST-EXTERNAL-TABLE oe-ord


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-ord.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES oe-ordl

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table oe-ordl.line oe-ordl.est-no ~
oe-ordl.i-no oe-ordl.qty oe-ordl.i-name oe-ordl.part-no ~
get-price-disc () @ ld-price get-pr-uom() @ ld-pr-uom oe-ordl.tax ~
oe-ordl.po-no oe-ordl.req-date oe-ordl.job-no oe-ordl.job-no2 ~
oe-ordl.vend-no oe-ordl.disc get-extended-price() @ ld-ext-price ~
oe-ordl.spare-char-3 oe-ordl.spare-char-4 ~
dueDateChangeRsn() @ cDueDateChangeRsn ~
dueDateChangeUser() @ oe-ordl.spare-char-5 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH oe-ordl OF oe-ord WHERE ~{&KEY-PHRASE} ~
      AND oe-ordl.line GT 0 AND ~
ASI.oe-ordl.line LT 99999999 NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH oe-ordl OF oe-ord WHERE ~{&KEY-PHRASE} ~
      AND oe-ordl.line GT 0 AND ~
ASI.oe-ordl.line LT 99999999 NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table oe-ordl
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table oe-ordl


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 auto_find ~
Btn_Clear_Find browse-order 
&Scoped-Define DISPLAYED-OBJECTS auto_find browse-order 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD dueDateChangeRsn B-table-Win 
FUNCTION dueDateChangeRsn RETURNS CHARACTER
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD dueDateChangeUser B-table-Win 
FUNCTION dueDateChangeUser RETURNS CHARACTER
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-extended-price B-table-Win 
FUNCTION get-extended-price RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-pr-uom B-table-Win 
FUNCTION get-pr-uom RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-price-disc B-table-Win 
FUNCTION get-price-disc RETURNS DECIMAL
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

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 52 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      oe-ordl
    FIELDS(oe-ordl.line
      oe-ordl.est-no
      oe-ordl.i-no
      oe-ordl.qty
      oe-ordl.i-name
      oe-ordl.part-no
      oe-ordl.tax
      oe-ordl.po-no
      oe-ordl.req-date
      oe-ordl.job-no
      oe-ordl.job-no2
      oe-ordl.vend-no
      oe-ordl.disc
      oe-ordl.spare-char-3
      oe-ordl.spare-char-4
      oe-ordl.spare-char-5) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      oe-ordl.line FORMAT ">>99":U WIDTH 6 LABEL-BGCOLOR 14
      oe-ordl.est-no FORMAT "x(8)":U WIDTH 12 LABEL-BGCOLOR 14
      oe-ordl.i-no FORMAT "x(15)":U LABEL-BGCOLOR 14  WIDTH 21 
      oe-ordl.qty FORMAT "->>,>>>,>>9":U LABEL-BGCOLOR 14
      oe-ordl.i-name FORMAT "x(30)":U LABEL-BGCOLOR 14
      oe-ordl.part-no COLUMN-LABEL "Part #" FORMAT "x(15)":U LABEL-BGCOLOR 14
      get-price-disc () @ ld-price COLUMN-LABEL "Sell Price" FORMAT "->>,>>>,>>9.99<<<<":U
            WIDTH 14.4
      get-pr-uom() @ ld-pr-uom COLUMN-LABEL "UOM" FORMAT "X(4)":U
      oe-ordl.tax COLUMN-LABEL "Tax" FORMAT "Y/N":U LABEL-BGCOLOR 14
      oe-ordl.po-no FORMAT "x(15)":U LABEL-BGCOLOR 14
      oe-ordl.req-date COLUMN-LABEL "Due Date" FORMAT "99/99/9999":U
            LABEL-BGCOLOR 14
      oe-ordl.job-no FORMAT "x(6)":U LABEL-BGCOLOR 14
      oe-ordl.job-no2 COLUMN-LABEL "" FORMAT ">9":U
      oe-ordl.vend-no FORMAT "x(8)":U LABEL-BGCOLOR 14
      oe-ordl.disc FORMAT "(>>>,>>9.99)":U LABEL-BGCOLOR 14
      get-extended-price() @ ld-ext-price COLUMN-LABEL "Total Price" FORMAT "->>,>>>,>>9.99":U
      oe-ordl.spare-char-3 COLUMN-LABEL "Dt Change Reason" FORMAT "x(20)":U
      oe-ordl.spare-char-4 COLUMN-LABEL "Prom Dt User" FORMAT "x(8)":U
      dueDateChangeRsn() @ cDueDateChangeRsn COLUMN-LABEL "Due Dt Chg Rsn"
      dueDateChangeUser() @ oe-ordl.spare-char-5 COLUMN-LABEL "Due Dt Usr" FORMAT "x(12)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 12.14
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     auto_find AT ROW 13.38 COL 69 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 13.38 COL 132 HELP
          "CLEAR AUTO FIND Value"
     browse-order AT ROW 13.43 COL 7 HELP
          "Select Browser Sort Order" NO-LABELS
     fi_sortby AT ROW 14.81 COL 19 COLON-ALIGNED NO-LABELS WIDGET-ID 2
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 13.38 COL 2
     RECT-4 AT ROW 13.14 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   External Tables: ASI.oe-ord
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
         HEIGHT             = 19.48
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
/* BROWSE-TAB Browser-Table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fi_sortby:HIDDEN IN FRAME F-Main           = TRUE
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.oe-ordl OF ASI.oe-ord"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,"
     _Where[1]         = "ASI.oe-ordl.line GT 0 AND
ASI.oe-ordl.line LT 99999999"
     _FldNameList[1]   > ASI.oe-ordl.line
"oe-ordl.line" ? ">>99" "integer" ? ? ? 14 ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.oe-ordl.est-no
"oe-ordl.est-no" ? "x(8)" "character" ? ? ? 14 ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.oe-ordl.i-no
"oe-ordl.i-no" ? ? "character" ? ? ? 14 ? ? no ? no no "21" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.oe-ordl.qty
"oe-ordl.qty" ? "->>,>>>,>>9" "decimal" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.oe-ordl.i-name
"oe-ordl.i-name" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.oe-ordl.part-no
"oe-ordl.part-no" "Part #" ? "character" ? ? ? 14 ? ? no "" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"get-price-disc () @ ld-price" "Sell Price" "->>,>>>,>>9.99<<<<" ? ? ? ? ? ? ? no ? no no "14.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"get-pr-uom() @ ld-pr-uom" "UOM" "X(4)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.oe-ordl.tax
"oe-ordl.tax" "Tax" ? "logical" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.oe-ordl.po-no
"oe-ordl.po-no" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.oe-ordl.req-date
"oe-ordl.req-date" "Due Date" ? "date" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.oe-ordl.job-no
"oe-ordl.job-no" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.oe-ordl.job-no2
"oe-ordl.job-no2" "" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.oe-ordl.vend-no
"oe-ordl.vend-no" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.oe-ordl.disc
"oe-ordl.disc" ? ? "decimal" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"get-extended-price() @ ld-ext-price" "Total Price" "->>,>>>,>>9.99" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > ASI.oe-ordl.spare-char-3
"oe-ordl.spare-char-3" "Dt Change Reason" "x(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ASI.oe-ordl.spare-char-4
"oe-ordl.spare-char-4" "Prom Dt User" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"dueDateChangeRsn() @ cDueDateChangeRsn" "Due Dt Chg Rsn" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"dueDateChangeUser() @ oe-ordl.spare-char-5" "Due Dt Usr" "x(12)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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

    DEFINE VARIABLE char-hdl AS cha NO-UNDO.
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"update-target",OUTPUT char-hdl).
    
    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN 
        RUN update-item IN WIDGET-HANDLE(char-hdl).
    
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"dtl-view-target",OUTPUT char-hdl).

    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO: 
        RUN oe/d-oeitem.w (RECID(oe-ordl), oe-ordl.ord-no, "View",
                      INPUT TABLE tt-item-qty-price,
                      OUTPUT v-rowid-list,
                      OUTPUT ll-canceled).
    END.
    
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
   RUN startSearch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  DEFINE VARIABLE v-fgitem LIKE itemfg.i-no NO-UNDO.
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  {methods/template/local/setvalue.i}
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-source",OUTPUT char-hdl).
  RUN pass-set-item-rec IN WIDGET-HANDLE(char-hdl) (INPUT RECID(oe-ord), INPUT RECID(oe-ordl))
      NO-ERROR.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}   
{custom/yellowColumns.i}
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
  {src/adm/template/row-list.i "oe-ord"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-ord"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bol-item B-table-Win 
PROCEDURE bol-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE choice AS LOG NO-UNDO.
DEFINE BUFFER bf-rel FOR oe-rel.
DEFINE VARIABLE v-stat AS cha NO-UNDO.
DEFINE VARIABLE v-all-items AS LOG NO-UNDO.
DEFINE VARIABLE v-first AS LOG NO-UNDO.
DEFINE VARIABLE lv-save-recid AS RECID NO-UNDO.
DEFINE VARIABLE v-chkflg AS LOG INIT NO NO-UNDO.
DEFINE VARIABLE v-all-i AS LOG NO-UNDO.
DEFINE VARIABLE v-rel-type AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-notes FOR notes .

FIND xoe-ord WHERE xoe-ord.company = g_company AND
                   xoe-ord.ord-no = oe-ordl.ord-no NO-LOCK.
FIND FIRST oe-ctrl WHERE oe-ctrl.company = xoe-ord.company NO-LOCK .

{sys/inc/addrelse.i}
{sys/inc/oereordr.i}

RUN check-release NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.

/* Check if the only release is an invoice only */

v-all-i = YES.
FOR EACH bf-rel
      WHERE bf-rel.company EQ xoe-ord.company
        AND bf-rel.ord-no  EQ xoe-ord.ord-no
        AND bf-rel.link-no EQ 0
      NO-LOCK:
    v-rel-type = bf-rel.s-code.
    IF v-rel-type NE "I" THEN
        v-all-i = NO.    
END.

FIND FIRST cust NO-LOCK 
        WHERE cust.company EQ oe-ordl.company
          AND cust.cust-no EQ oe-ordl.cust-no NO-ERROR.

ASSIGN clvtext = "Notes: " .
IF oeBolPrompt-log AND AVAILABLE cust THEN DO:
    
        FOR EACH notes NO-LOCK WHERE notes.rec_key = cust.rec_key AND
                              LOOKUP(notes.note_code,oeBolPrompt-char) <> 0 :
            clvtext = clvtext + notes.note_text + "  " + CHR(13)  .
        END.
    
    IF clvtext NE "Notes: " THEN
    MESSAGE clvtext VIEW-AS ALERT-BOX INFORMATION BUTTONS OK  .
END.

/* gdm - 02020902 */
FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name EQ "RELCREDT" NO-ERROR.
IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN DO:
    
    
    IF AVAILABLE cust THEN RUN oe/CRcheck.p (INPUT ROWID(cust),
                                         INPUT YES,
                                         OUTPUT v-chkflg).
    IF v-chkflg AND NOT v-all-i THEN DO:

        MESSAGE 
            "Can't create BOL, there are unpaid invoices."
            "Please create actual release first."
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

        RETURN NO-APPLY.     
    END.

END.

lv-save-recid = RECID(oe-ordl).     

FOR EACH bf-rel
      WHERE bf-rel.company EQ xoe-ord.company
        AND bf-rel.ord-no  EQ xoe-ord.ord-no
        AND bf-rel.link-no EQ 0
      NO-LOCK:
    
  RUN oe/rel-stat.p (ROWID(bf-rel), OUTPUT v-stat).
       
  IF INDEX("AB",v-stat) EQ 0 THEN DO:
     choice = YES.
     LEAVE.
  END.
END.

IF NOT choice THEN RELEASE bf-rel.

choice = AVAILABLE bf-rel AND index("CDZ",xoe-ord.stat) EQ 0.

IF choice THEN DO:
  SESSION:SET-WAIT-STATE("general").

  FIND xoe-ordl OF xoe-ord NO-LOCK NO-ERROR.
  IF AMBIGUOUS xoe-ordl THEN
    MESSAGE "All Items on Order?" VIEW-AS ALERT-BOX QUESTION
            BUTTON YES-NO UPDATE choice.

  v-all-items = choice.

  EMPTY TEMP-TABLE w-rel.

  FOR EACH bf-rel
      WHERE bf-rel.company EQ xoe-ord.company
        AND bf-rel.ord-no  EQ xoe-ord.ord-no
        AND bf-rel.link-no EQ 0
      NO-LOCK,
      FIRST xoe-ordl
      WHERE xoe-ordl.company EQ bf-rel.company
        AND xoe-ordl.ord-no  EQ bf-rel.ord-no
        AND xoe-ordl.i-no    EQ bf-rel.i-no
        AND xoe-ordl.line    EQ bf-rel.line
        AND (RECID(xoe-ordl) EQ RECID(oe-ordl) OR v-all-items)
      NO-LOCK:
     
    RUN oe/rel-stat.p (ROWID(bf-rel), OUTPUT v-stat).

    IF NOT CAN-DO("A,B",v-stat) THEN DO:

       CREATE w-rel.
       ASSIGN
        w-rel.w-rowid = ROWID(bf-rel)
        w-rel.w-scode = IF bf-rel.s-code EQ "I" THEN
                           bf-rel.s-code
                        ELSE
                           "B".






/*       FIND FIRST b-fob WHERE                                 */
/*            b-fob.reftable EQ "oe-rel.lot-no" AND             */
/*            b-fob.company  EQ STRING(bf-rel.r-no,"9999999999")*/
/*            NO-LOCK NO-ERROR.                                 */
/*                                                              */
/*       IF AVAILABLE b-fob THEN                                */
/*       DO:                                                    */
/*           w-rel.fob = b-fob.dscr.                            */
/*           RELEASE b-fob.                                     */
/*       END.                                                   */
         ASSIGN w-rel.fob = bf-rel.fob-code.
       RELEASE w-rel.
    END.
  END.

  FOR EACH w-rel,
      FIRST bf-rel WHERE ROWID(bf-rel) EQ w-rowid NO-LOCK
      BREAK BY w-rel.w-scode
            BY bf-rel.rel-date
            BY bf-rel.ship-id
            BY w-rel.fob:
                   
    IF FIRST-OF(w-rel.fob) THEN DO:
       choice = v-do-def.

       MESSAGE "Create " +
               TRIM(STRING(w-scode EQ "I","Invoice/BOL")) +
               " for Release Date-" + TRIM(STRING(bf-rel.rel-date)) +
               " and ShipID-" +  TRIM(bf-rel.ship-id) + 
               " and FOB-" + TRIM(w-rel.fob) + " ?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice.
    END.
    
    IF NOT choice THEN
       DELETE w-rel.
  END.
  
  FOR EACH w-rel,
      FIRST bf-rel WHERE ROWID(bf-rel) EQ w-rowid NO-LOCK,
      FIRST xoe-ordl
      WHERE xoe-ordl.company EQ bf-rel.company
        AND xoe-ordl.ord-no  EQ bf-rel.ord-no
        AND xoe-ordl.i-no    EQ bf-rel.i-no
        AND xoe-ordl.line    EQ bf-rel.line
      BREAK BY w-rel.w-scode
            BY bf-rel.rel-date
            BY bf-rel.ship-id
            BY w-rel.fob:

    ASSIGN
     out-recid = RECID(bf-rel)
     v-auto    = YES.

    RUN oe/relbol.p (RECID(xoe-ordl)).

    IF LAST-OF(w-rel.fob) THEN RUN oe/do-bol.p(INPUT NOT w-rel.w-scode EQ "I").

    DELETE w-rel.
  END.


  IF oeBolPrompt-log AND AVAILABLE xoe-ord THEN DO:
      IF NOT AVAIL cust THEN
          FIND FIRST cust NO-LOCK 
          WHERE cust.company EQ xoe-ord.company
          AND cust.cust-no EQ xoe-ord.cust-no NO-ERROR.
      IF AVAIL cust THEN
          FOR EACH notes NO-LOCK WHERE notes.rec_key = cust.rec_key AND
                                LOOKUP(notes.note_code,oeBolPrompt-char) <> 0 :
             FIND FIRST oe-boll NO-LOCK
                 WHERE oe-boll.company  EQ xoe-ord.company
                 AND oe-boll.ord-no   EQ xoe-ord.ord-no
                 NO-ERROR.
             IF AVAILABLE oe-boll THEN 
                 FIND FIRST oe-bolh NO-LOCK WHERE oe-bolh.b-no EQ oe-boll.b-no NO-ERROR.
             IF AVAILABLE oe-bolh THEN DO:
                 CREATE bf-notes .
                 BUFFER-COPY notes EXCEPT rec_key TO bf-notes .
                 ASSIGN bf-notes.rec_key = oe-bolh.rec_key .
             END.
          END.
  END. /*oeBolPrompt-log  */
  

  DEFINE VARIABLE char-hdl AS cha NO-UNDO.
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"bolrel-target",OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
     RUN reopen-query IN WIDGET-HANDLE(char-hdl).

  SESSION:SET-WAIT-STATE("").
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
DEFINE VARIABLE lv-msg AS CHARACTER NO-UNDO.
DEFINE VARIABLE lHoldOK AS LOGICAL NO-UNDO.

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ g_company NO-LOCK NO-ERROR.
IF AVAILABLE oe-ctrl THEN
    lHoldOK = oe-ctrl.p-pick.

IF lv-msg EQ "" AND oe-ord.stat EQ "H" AND NOT lHoldOK THEN
  lv-msg = "orders on Hold".

IF lv-msg EQ "" AND oe-ord.stat EQ "W" THEN
  lv-msg = "unapproved web orders".

IF lv-msg EQ "" AND NOT oe-ord.opened THEN
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clear-tt B-table-Win 
PROCEDURE clear-tt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  EMPTY TEMP-TABLE tt-oe-ordl.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE close-reopen B-table-Win 
PROCEDURE close-reopen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ll AS LOG NO-UNDO.
  DEFINE VARIABLE ll-close AS LOG NO-UNDO.
  DEFINE VARIABLE ll-close-header AS LOG NO-UNDO.
  DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
  IF AVAILABLE oe-ordl THEN DO:
    ll-close = oe-ordl.stat NE "C".
    /* Check if all order lines would be closed, if so, close order */
    FIND FIRST bf-oe-ordl WHERE bf-oe-ordl.company EQ oe-ordl.company
                            AND bf-oe-ordl.ord-no  EQ oe-ordl.ord-no
                            AND ROWID(bf-oe-ordl) NE ROWID(oe-ordl)
                            AND bf-oe-ordl.stat NE "C"
                          NO-LOCK NO-ERROR.
    IF NOT AVAIL(bf-oe-ordl) AND ll-close THEN DO:
        RUN close-reopen-order.
    END.
    ELSE DO:
        MESSAGE "Are you sure you want to " +
                TRIM(STRING(ll-close,"close/reopen")) +
                " this Order Line Item?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll.
    
        IF ll THEN DO:
          RUN oe/closelin.p (ROWID(oe-ordl), ll-close).
          RUN reposit-item (RECID(oe-ord), RECID(oe-ordl)).
        END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE close-reopen-order B-table-Win 
PROCEDURE close-reopen-order :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       If all items have been deleted, delete the header
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
  DEFINE VARIABLE phandle AS WIDGET-HANDLE NO-UNDO.
  IF AVAIL(oe-ordl) THEN DO:

      RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
  
  
      phandle = SESSION:FIRST-PROCEDURE.
      DO WHILE VALID-HANDLE(phandle):
        IF INDEX(phandle:FILE-NAME,'b-ordinq.') NE 0 THEN DO:          
          LEAVE.
        END.
        phandle = phandle:NEXT-SIBLING.    
      END.
      IF NOT VALID-HANDLE(phandle) THEN
          RETURN.

      RUN browse-rowid IN phandle (OUTPUT lv-rowid).
      FIND oe-ord WHERE oe-ord.company = oe-ordl.company
                    AND oe-ord.ord-no  = oe-ordl.ord-no
                  NO-LOCK NO-ERROR.
      RUN oe/d-clsoe.w (ROWID(oe-ord)).
    
      RUN reopen-query1 IN phandle (lv-rowid).

  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-bol B-table-Win 
PROCEDURE create-bol :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN bol-item.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-item B-table-Win 
PROCEDURE delete-item :
/*------------------------------------------------------------------------------
  Purpose:     from oe/oe-ordld.p 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-ask-delete AS LOG NO-UNDO.
  DEFINE INPUT PARAMETER ip-comps-deleted AS LOG NO-UNDO.

  DEFINE VARIABLE lv-item-recid AS RECID NO-UNDO.
  DEFINE VARIABLE ll-dumb AS LOG NO-UNDO.
  DEFINE VARIABLE lv-continue AS LOG NO-UNDO.
  DEFINE BUFFER bf-ordl FOR oe-ordl.
  
  DEFINE VARIABLE v-delete AS LOG NO-UNDO.
  DEFINE VARIABLE choice AS LOG NO-UNDO.
  DEFINE BUFFER xoe-ordl FOR oe-ordl.
  DEFINE BUFFER xfg-set FOR fg-set.
  DEFINE BUFFER xitemfg FOR itemfg.
  DEFINE VARIABLE v-tax-rate AS DECIMAL FORMAT "->,>>9.99<<<" NO-UNDO.
  DEFINE VARIABLE v-frt-tax-rate LIKE v-tax-rate NO-UNDO.
  DEFINE VARIABLE tmp-tax LIKE oe-ord.tax NO-UNDO INIT 0 .
  DEFINE VARIABLE tmp-ordm-amt LIKE oe-ordm.amt NO-UNDO INIT 0.
  DEFINE VARIABLE v-continue AS LOG NO-UNDO.
  DEFINE VARIABLE v-blank-fg-on-est AS INTEGER NO-UNDO.
  DEFINE VARIABLE x AS INTEGER NO-UNDO.
  DEFINE VARIABLE ll-tandem AS LOG NO-UNDO.
  DEFINE VARIABLE ll AS LOG NO-UNDO.
  DEFINE VARIABLE lv-est-no LIKE oe-ordl.est-no NO-UNDO.
  DEFINE VARIABLE ll-valid-eb AS LOG NO-UNDO.
  DEFINE VARIABLE ll-more-ord AS LOG NO-UNDO.
  DEFINE VARIABLE v-del-item-est AS LOG NO-UNDO.

  DEFINE VARIABLE lMatrixExists AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE dPrice AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cUom AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE dTotalPrice AS DECIMAL     NO-UNDO.

  DEFINE BUFFER tmp-xoe-ordl FOR oe-ordl.
  DEFINE BUFFER tmp-xoe-ordm FOR oe-ordm.
    
  IF AVAILABLE oe-ordl AND oe-ordl.rel-stat THEN DO:
     MESSAGE "Previous Quantities have been released for this item..." SKIP
             VIEW-AS ALERT-BOX ERROR.
             RETURN.
  END.

  IF AVAILABLE oe-ordl AND oe-ordl.po-no-po <> 0 THEN DO:
       MESSAGE "Cannot delete, purchase order for board exists..." VIEW-AS ALERT-BOX ERROR.
       RETURN.
  END.

  IF (oe-ordl.est-type EQ 3 OR
      oe-ordl.est-type EQ 4 OR
      oe-ordl.est-type EQ 8) AND
      ip-ask-delete THEN DO:

      MESSAGE "Deleting Item will Purge Item from Estimate,  Continue?"
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-del-item-est.

      IF NOT v-del-item-est THEN
         RETURN.
  END.

  {sys/inc/oedelete.i}

  {sys/inc/oecomb.i}

  RUN clear-tt.

  ASSIGN
   lv-est-no   = FILL(" ",8 - LENGTH(TRIM(oe-ordl.est-no))) +
                 TRIM(oe-ordl.est-no)
   ll-valid-eb = CAN-FIND(FIRST eb
                           WHERE eb.company  EQ oe-ordl.company
                             AND eb.est-no   EQ lv-est-no
                             AND eb.stock-no EQ oe-ordl.i-no)
   ll-more-ord = CAN-FIND(FIRST bf-ordl
                          WHERE bf-ordl.company EQ oe-ordl.company
                            AND bf-ordl.ord-no  EQ oe-ordl.ord-no
                            AND ROWID(bf-ordl)  NE ROWID(oe-ordl)).

  IF ip-ask-delete THEN
    IF AVAILABLE oe-ordl                    AND
       ((oe-ordl.est-type NE 3 AND
         oe-ordl.est-type NE 4 AND
         oe-ordl.est-type NE 8)     OR
        NOT ll-valid-eb OR oedelete)    THEN DO:  
      ll = YES.

      /* security for combo estimage task#01180506*/
      FIND FIRST est
          WHERE est.company EQ oe-ord.company
            AND est.est-no  EQ lv-est-no
          NO-LOCK NO-ERROR.
      IF AVAILABLE est AND (est.est-type EQ 4 OR est.est-type EQ 8) THEN DO:
        RUN ce/com/istandem.p (ROWID(est), OUTPUT ll-tandem) . /* check the estimate whether it's tandem or combo - same est-type 4 */           
        IF NOT ll-tandem AND lv-passwd NE v-oecomb-val THEN DO:
          IF v-oecomb THEN DO:
            RUN custom/d-passwd.w (OUTPUT lv-passwd).
            ll = lv-passwd EQ v-oecomb-val.
          END.
        END.
      END.

      IF ll THEN DO:
        ll = NO.
        /*{custom/askdel.i} is for local-delete-record */
        MESSAGE "Delete Currently Selected Record(s)?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll.            
      END.

      IF NOT ll THEN RETURN.
    END.

    ELSE DO:
      MESSAGE "Deletion not allowed, you must delete the entire order..."
          VIEW-AS ALERT-BOX ERROR.
      RETURN.
    END.
  
  lv-item-recid = RECID(oe-ordl).
  FIND bf-ordl WHERE RECID(bf-ordl) EQ lv-item-recid NO-LOCK NO-ERROR.
  RUN oe/oe-ordd.p  (lv-item-recid, OUTPUT lv-continue).  /* deleting bol,rel */
  IF NOT lv-continue THEN RETURN.

  FOR EACH oe-rel WHERE oe-rel.company EQ oe-ordl.company
                    AND oe-rel.ord-no  EQ oe-ordl.ord-no
                    AND oe-rel.i-no    EQ oe-ordl.i-no
                    AND oe-rel.line    EQ oe-ordl.line:
        DELETE oe-rel.
  END.

  tmp-ordm-amt = 0.
  IF oe-ord.est-no EQ ""                              AND
     oe-ordl.est-no NE ""                             AND
     (oe-ordl.est-type LT 3 OR oe-ordl.est-type GT 4) AND
     oe-ordl.est-type NE 8                            THEN
  FOR EACH oe-ordm WHERE oe-ordm.company = oe-ordl.company
                     AND oe-ordm.ord-no = oe-ordl.ord-no 
                     AND oe-ordm.est-no = oe-ordl.est-no:
      IF oe-ordm.bill = "Y" THEN tmp-ordm-amt = tmp-ordm-amt + oe-ordm.amt.               
      DELETE oe-ordm.
  END.
      
  IF oe-ordl.job-no <> "" THEN DO:
     IF ll-valid-eb AND
        CAN-FIND(FIRST job
                 WHERE job.company = oe-ordl.company
                   AND job.job-no = oe-ordl.job-no
                   AND job.job-no2 = oe-ordl.job-no2
                   AND (job.stat = "C" OR job.stat = "W" OR job.stat = "Z")
                 USE-INDEX job-no)                   
     THEN DO:
        MESSAGE "Item cannot be deleted, Job has been processed or closed. You must close the order. "
                VIEW-AS ALERT-BOX ERROR.
        RETURN.
     END.
     FIND FIRST job WHERE job.company = oe-ordl.company AND
                          job.job-no = oe-ordl.job-no AND
                          job.job-no2 = oe-ordl.job-no2 
                          USE-INDEX job-no NO-LOCK NO-ERROR.
     IF CAN-FIND(FIRST job-hdr
                 WHERE job-hdr.company EQ cocode
                   AND job-hdr.est-no  EQ oe-ordl.est-no
                   AND job-hdr.job-no  EQ oe-ordl.job-no
                   AND job-hdr.job-no2 EQ oe-ordl.job-no2
                   AND job-hdr.ord-no  EQ oe-ordl.ord-no
                   AND job-hdr.i-no    EQ oe-ordl.i-no) THEN DO:
       FOR EACH job-hdr WHERE job-hdr.company EQ cocode
                        AND job-hdr.est-no   EQ oe-ordl.est-no
                        AND job-hdr.job-no  EQ oe-ordl.job-no
                        AND job-hdr.job-no2 EQ oe-ordl.job-no2
                        AND job-hdr.ord-no  EQ oe-ordl.ord-no
                        AND job-hdr.i-no    EQ oe-ordl.i-no  /* use-index enum */ :
          IF AVAILABLE job AND job.stat NE "P" THEN DO:
             FIND FIRST itemfg WHERE itemfg.company EQ cocode
                                 AND itemfg.i-no    EQ oe-ordl.i-no NO-ERROR.
             /*if avail itemfg then do:
                itemfg.q-ono = itemfg.q-ono - job-hdr.qty.
                if itemfg.q-ono lt 0 then itemfg.q-ono = 0.
                itemfg.q-avail = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc.              
                run fg/comp-upd.p (recid(itemfg), job-hdr.qty * -1, "q-ono", job-hdr.est-no).
             end.*/
          END.
          
          IF AVAILABLE job-hdr THEN DO:
             {util/dljobkey.i}
          END.
          DELETE job-hdr.
       END.
     END.

     RELEASE job.

     FOR EACH job WHERE job.company EQ cocode
                    AND job.job-no  EQ oe-ordl.job-no
                    AND job.job-no2 EQ oe-ordl.job-no2
                    AND NOT CAN-FIND(FIRST job-hdr
                                     WHERE job-hdr.company EQ cocode
                                       AND job-hdr.job     EQ job.job
                                       AND job-hdr.job-no  EQ job.job-no
                                       AND job-hdr.job-no2 EQ job.job-no2
                                     USE-INDEX job-no):
 
        RUN jc/jc-dall.p (RECID(job)).                    
 
        FOR EACH job-mat WHERE job-mat.company  EQ cocode
                           AND job-mat.job      EQ job.job
                           AND job-mat.job-no   EQ job.job-no
                           AND job-mat.job-no2  EQ job.job-no2
                          USE-INDEX seq-idx:
            DELETE job-mat.
        END.
        FOR EACH mat-act WHERE mat-act.company  EQ cocode
                           AND mat-act.job      EQ job.job
                           AND mat-act.job-no   EQ job.job-no
                           AND mat-act.job-no2  EQ job.job-no2
                           USE-INDEX job:
           DELETE mat-act.
        END.
        FOR EACH job-mch WHERE job-mch.company  EQ cocode
                           AND job-mch.job      EQ job.job
                           AND job-mch.job-no   EQ job.job-no
                           AND job-mch.job-no2  EQ job.job-no2
                         USE-INDEX seq-idx:
           DELETE job-mch.
        END.
        FOR EACH job-prep WHERE job-prep.company  EQ cocode
                            AND job-prep.job      EQ job.job
                            AND job-prep.job-no   EQ job.job-no
                            AND job-prep.job-no2  EQ job.job-no2:
           DELETE job-prep.
        END.
        FOR EACH job-farm
            WHERE job-farm.company EQ job.company
              AND job-farm.job-no  EQ job.job-no
              AND job-farm.job-no2 EQ job.job-no2
            EXCLUSIVE:
          DELETE job-farm.
        END.
        FOR EACH job-farm-rctd
            WHERE job-farm-rctd.company EQ job.company
              AND job-farm-rctd.job-no  EQ job.job-no
              AND job-farm-rctd.job-no2 EQ job.job-no2
            EXCLUSIVE:
          DELETE job-farm-rctd.
        END.
        FOR EACH misc-act WHERE misc-act.company  EQ cocode
                            AND misc-act.job      EQ job.job
                            AND misc-act.job-no   EQ job.job-no
                            AND misc-act.job-no2  EQ job.job-no2
                          USE-INDEX misc-idx:
           DELETE misc-act.
        END.
            
        IF job.exported THEN DO:
           job.stat = "X".
           RUN jc/kiwiexp2.p (RECID(job)).
        END.

        DELETE job.
     END. /* for each job where not can-find job-hdr for oe-ordl */
  END. /* oe-ordl.job-no <> "" */

  v-delete = NO.
  FIND FIRST itemfg WHERE itemfg.company EQ cocode
                      AND itemfg.i-no    EQ oe-ordl.i-no NO-ERROR.
  IF AVAILABLE itemfg THEN DO:
     IF NOT ip-comps-deleted THEN DO:
       IF oe-ord.type NE "T" THEN
         RUN fg/comp-upd.p (RECID(itemfg), oe-ordl.qty * -1,"q-alloc", 0).                          
       IF itemfg.isaset THEN DO:
         RUN fg/fullset.p (ROWID(itemfg)).

         FOR EACH tt-fg-set,
             FIRST xitemfg
             WHERE xitemfg.company EQ tt-fg-set.company
               AND xitemfg.i-no    EQ tt-fg-set.part-no:

           ASSIGN
            xitemfg.q-ptd     = xitemfg.q-ptd -
                                (oe-ordl.qty * tt-fg-set.part-qty-dec)
            xitemfg.q-ord-ytd = xitemfg.q-ord-ytd -
                                (oe-ordl.qty * tt-fg-set.part-qty-dec).
           IF AVAILABLE oe-ord THEN DO:
               RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT oe-ord.loc).
               FIND FIRST itemfg-loc 
                   WHERE itemfg-loc.company EQ itemfg.company
                     AND itemfg-loc.i-no    EQ itemfg.i-no
                     AND itemfg-loc.loc     EQ oe-ord.loc
                   EXCLUSIVE-LOCK NO-ERROR.
               IF AVAILABLE itemfg-loc THEN
                   ASSIGN
                    itemfg-loc.q-ptd     = itemfg-loc.q-ptd -
                                        (oe-ordl.qty * tt-fg-set.part-qty-dec)
                    itemfg-loc.q-ord-ytd = itemfg-loc.q-ord-ytd -
                                        (oe-ordl.qty * tt-fg-set.part-qty-dec).
               FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
           END.
         END.

         IF v-delete THEN
         FOR EACH fg-set
             WHERE fg-set.company EQ itemfg.company
               AND fg-set.set-no  EQ itemfg.i-no,
             FIRST xitemfg
             WHERE xitemfg.company EQ fg-set.company
               AND xitemfg.i-no    EQ fg-set.part-no:
           FIND FIRST xoe-ordl
               WHERE xoe-ordl.company EQ fg-set.company
                 AND xoe-ordl.i-no    EQ fg-set.part-no
               USE-INDEX item NO-LOCK NO-ERROR.
           IF NOT AVAILABLE xoe-ordl THEN DO:
             FIND FIRST xfg-set
                 WHERE xfg-set.company EQ fg-set.company
                   AND xfg-set.part-no EQ fg-set.part-no
                   AND ROWID(xfg-set)  NE ROWID(fg-set)
                 USE-INDEX part-no NO-LOCK NO-ERROR.
             IF NOT AVAILABLE fg-set THEN DELETE xitemfg.
           END. /* not avail xoe-ordl */ 
           DELETE fg-set.
         END.
       END.
     END.

     FIND xoe-ord WHERE xoe-ord.company EQ cocode
                       AND xoe-ord.ord-no  EQ oe-ordl.ord-no  NO-ERROR.
     IF AVAILABLE xoe-ord THEN DO:
           xoe-ord.t-weight = xoe-ord.t-weight -
                              (oe-ordl.qty / 100 * itemfg.weight-100).
           IF xoe-ord.t-weight LT 0 THEN xoe-ord.t-weight = 0.
           RUN oe/oe-frtcl.p.
     END.

     IF v-delete THEN DELETE itemfg.
     ELSE DO:
           IF oe-ord.type NE "T" THEN
             itemfg.q-alloc = itemfg.q-alloc - oe-ordl.qty.
           IF itemfg.q-alloc LT 0 THEN ASSIGN itemfg.q-alloc = 0.
           ASSIGN itemfg.q-avail   = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc
                  itemfg.q-ptd     = itemfg.q-ptd - oe-ordl.qty
                  itemfg.q-ord-ytd = itemfg.q-ord-ytd - oe-ordl.qty.
     END.

     FIND FIRST itemfg WHERE itemfg.company EQ cocode
                            AND itemfg.i-no    EQ oe-ordl.i-no NO-LOCK NO-ERROR.
     IF NOT AVAILABLE itemfg THEN v-blank-fg-on-est = 2.
     IF v-blank-fg-on-est      EQ 2                              OR
           (itemfg.avg-cost       EQ 0 AND itemfg.last-cost EQ 0 AND
            itemfg.total-std-cost EQ 0 AND itemfg.beg-bal   EQ 0 AND
            itemfg.q-onh          EQ 0 AND itemfg.q-ono     EQ 0 AND
            itemfg.q-alloc        EQ 0 AND itemfg.q-back    EQ 0 AND
            itemfg.q-avail        EQ 0)                             THEN
     FOR EACH est WHERE est.company EQ xoe-ord.company
                    AND est.est-no  EQ fill(" ",8 - length(TRIM(oe-ordl.est-no))) +
                        trim(oe-ordl.est-no):

       FIND LAST xoe-ord WHERE xoe-ord.company EQ cocode AND
                               xoe-ord.est-no  EQ oe-ordl.est-no AND
                               xoe-ord.ord-no  NE oe-ordl.ord-no
                               NO-LOCK NO-ERROR.
       IF AVAILABLE xoe-ord THEN
         ASSIGN
          est.ord-date = xoe-ord.ord-date
          est.ord-no   = xoe-ord.ord-no.
       ELSE DO:
         IF est.ord-date EQ oe-ord.ord-date THEN est.ord-date = ?.
         IF est.ord-no EQ oe-ord.ord-no THEN est.ord-no = 0.
       END.

       FOR EACH eb WHERE eb.company = est.company
                      AND eb.est-no  EQ est.est-no
                      AND eb.stock-no   NE ""
                      AND (eb.stock-no  EQ oe-ordl.i-no OR
                           est.est-type EQ 2 OR est.est-type EQ 6):
          
            IF v-blank-fg-on-est EQ 0 AND 
               oe-ordl.est-type NE 3  AND
               oe-ordl.est-type NE 4  AND
               oe-ordl.est-type NE 8  THEN DO:
              choice = YES.
              MESSAGE "Remove FG Item# from the Estimate?"
                 VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice.
              v-blank-fg-on-est = int(choice) + 1.
            END.
            IF v-blank-fg-on-est EQ 2 THEN eb.stock-no = "".
       END.
     END.    
  END. /* avail itemfg */

  FIND xoe-ord WHERE xoe-ord.company EQ cocode
                       AND xoe-ord.ord-no  EQ oe-ordl.ord-no  NO-ERROR.

  IF oe-ordl.est-type EQ 1 OR oe-ordl.est-type EQ 5 THEN DO:
     FIND FIRST job WHERE job.company  EQ cocode
                      AND job.job-no   EQ oe-ordl.job-no
                      AND job.job-no2  EQ oe-ordl.job-no2
                      AND job.stat     EQ "P"
                      USE-INDEX stat-idx NO-ERROR.
     IF AVAILABLE job THEN DO:
        FIND FIRST job-hdr WHERE job-hdr.company  EQ cocode
                             AND job-hdr.loc      EQ locode
                             AND job-hdr.est-no    EQ oe-ordl.est-no
                             AND job-hdr.job-no   EQ oe-ordl.job-no
                             AND job-hdr.job-no2  EQ oe-ordl.job-no2
                             AND job-hdr.ord-no   EQ oe-ordl.ord-no NO-ERROR.
        IF AVAILABLE job-hdr THEN DO:
           {util/dljobkey.i}        
           DELETE job-hdr.
        END.          
        RUN jc/jc-dall.p (RECID(job)).          
        DELETE job.
     END.
  END.
  ELSE IF oe-ordl.est-type EQ 2 OR oe-ordl.est-type EQ 6 THEN DO:
       FIND FIRST job-hdr WHERE job-hdr.company EQ cocode
                            AND job-hdr.loc     EQ locode
                            AND job-hdr.est-no   EQ oe-ordl.est-no
                            AND job-hdr.job-no  EQ oe-ordl.job-no
                            AND job-hdr.job-no2 EQ oe-ordl.job-no2
                            AND job-hdr.ord-no  EQ oe-ordl.ord-no  NO-ERROR.
       IF AVAILABLE job-hdr THEN DELETE job-hdr.
  END.
  ELSE
  IF (oe-ordl.est-type EQ 3 OR
      oe-ordl.est-type EQ 4 OR
      oe-ordl.est-type EQ 8)    AND
     ip-ask-delete              THEN DO:

    {ce/tan-del.i}
    FIND CURRENT bf-ordl NO-ERROR.
    IF AVAILABLE bf-ordl THEN DELETE bf-ordl.
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN order-from-est IN WIDGET-HANDLE(char-hdl) (?).
  END.

  FIND xoe-ord WHERE xoe-ord.company EQ cocode
                 AND xoe-ord.ord-no  EQ oe-ord.ord-no NO-ERROR.
  FIND FIRST cust
      {sys/ref/custW.i}
        AND cust.cust-no EQ xoe-ord.cust-no
      USE-INDEX cust NO-LOCK NO-ERROR.

  IF cust.auto-reprice THEN RUN oe/oe-rpric.p.
/*  IF cust.auto-reprice THEN 
      RUN oe/StockItemPrice.p(
          INPUT oe-ordl.company,
          INPUT ROWID(oe-ord),
          INPUT ROWID(oe-ordl),
          INPUT YES, /*do the update*/
          INPUT NO, /*price inputted*/
          INPUT oe-ordl.i-no, /*for determining auto-reprice class*/
          INPUT oe-ordl.qty,
          OUTPUT lMatrixExists,
          OUTPUT dPrice,
          OUTPUT cUom,
          OUTPUT dTotalPrice). */

  RUN oe/oe-comm.p.

  FIND xoe-ord WHERE RECID(xoe-ord) EQ fil_id NO-ERROR.
       /** change order status to (u)pdated **/
  IF AVAILABLE xoe-ord THEN DO:
    IF xoe-ord.stat NE "N" AND
       xoe-ord.stat NE "H" AND
       xoe-ord.stat NE "A" THEN xoe-ord.stat = "U".
    xoe-ord.t-freight = xoe-ord.t-freight - oe-ordl.t-freight.
  END.
  FIND xoe-ord WHERE RECID(xoe-ord) = fil_id NO-LOCK NO-ERROR.

  IF ll-more-ord THEN DO:
    ll-dumb = BROWSE {&browse-name}:DELETE-CURRENT-ROW().
    
    FIND bf-ordl WHERE RECID(bf-ordl) EQ lv-item-recid NO-ERROR.
    IF AVAILABLE bf-ordl THEN DELETE bf-ordl.

    RUN dispatch ('row-changed').
  END.

  ELSE DO:
    CREATE tt-oe-ordl.
    ASSIGN
     tt-oe-ordl.company = oe-ord.company
     tt-oe-ordl.ord-no  = oe-ord.ord-no
     tt-oe-ordl.line    = 0.

    FIND bf-ordl WHERE RECID(bf-ordl) EQ lv-item-recid NO-ERROR.
    BUFFER-COPY tt-oe-ordl EXCEPT rec_key TO bf-ordl.

    RUN dispatch ("open-query").
  END.

  RUN oe/calcordt.p (ROWID(oe-ord)).
  FIND FIRST cust NO-LOCK
      WHERE cust.company EQ cocode
      AND cust.cust-no EQ oe-ord.cust-no
      USE-INDEX cust  NO-ERROR.
  IF AVAIL cust AND cust.active NE "X" AND AVAIL oe-ord AND oe-ord.TYPE NE "T" THEN
    RUN oe/creditck.p (ROWID(oe-ord), YES).

  RUN refresh-releases.

  SESSION:SET-WAIT-STATE("").
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-line-est B-table-Win 
PROCEDURE get-line-est :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER op-est-no AS cha NO-UNDO.
  
  op-est-no = IF AVAILABLE oe-ordl THEN oe-ordl.est-no ELSE oe-ord.est-no.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-rowid2 B-table-Win 
PROCEDURE get-rowid2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER op-rowid AS ROWID NO-UNDO.
  
  IF AVAILABLE oe-ordl THEN op-rowid = ROWID(oe-ordl).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadTag B-table-Win 
PROCEDURE loadTag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cOrder AS CHARACTER NO-UNDO .
  DEFINE BUFFER bff-itemfg FOR itemfg .
  IF NOT AVAILABLE oe-ord THEN RETURN.
  ASSIGN cOrder = string(oe-ordl.ord-no) .
  FIND FIRST po-ordl NO-LOCK
       WHERE po-ordl.company EQ cocode
         AND po-ordl.po-no = oe-ordl.po-no-po  NO-ERROR.
        IF AVAIL po-ordl THEN DO:
          FIND FIRST bff-itemfg NO-LOCK
               WHERE bff-itemfg.company EQ cocode
                 AND bff-itemfg.i-no EQ oe-ordl.i-no
              NO-ERROR.
          IF AVAIL bff-itemfg AND bff-itemfg.pur-man AND NOT bff-itemfg.isaset THEN
              ASSIGN cOrder = string(oe-ordl.po-no-po) .
        END.
  
  RUN custom/setUserPrint.p (INPUT oe-ordl.company,
                           INPUT 'r-loadtg.',
                           INPUT 'begin_ord-no,end_ord-no,begin_job,end_job,begin_job2,end_job2,begin_i-no,end_i-no',
                           INPUT STRING(cOrder) + ',' 
                                + STRING(cOrder) + ',' 
                                + STRING(oe-ordl.job-no) + ',' 
                                + STRING(oe-ordl.job-no) + ',' 
                                + STRING(oe-ordl.job-no2) + ',' 
                                + STRING(oe-ordl.job-no2) + ',' 
                                + STRING(oe-ordl.i-no) + ','
                                + STRING(oe-ordl.i-no)).
  
  RUN Get_Procedure IN Persistent-Handle ('r-loadtg.',OUTPUT run-proc,YES).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable B-table-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY "value-changed" TO BROWSE {&browse-name}.

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN refresh-releases.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available B-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE v-fgitem LIKE itemfg.i-no NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE printTicket B-table-Win 
PROCEDURE printTicket :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT AVAILABLE oe-ordl THEN RETURN.

  DEFINE VARIABLE v-reprint AS LOG NO-UNDO.

  IF oe-ordl.job-no NE "" THEN
     FIND FIRST b-job-hdr WHERE
          b-job-hdr.company EQ oe-ordl.company AND
          b-job-hdr.job-no EQ oe-ordl.job-no AND
          b-job-hdr.job-no2 EQ oe-ordl.job-no2
          NO-LOCK NO-ERROR.

  IF AVAILABLE b-job-hdr THEN
  DO:
     v-reprint = b-job-hdr.ftick-prnt.
     RELEASE b-job-hdr.

     RUN custom/setUserPrint.p (oe-ordl.company,'job_.',
                             'begin_job1,begin_job2,end_job1,end_job2,tb_reprint,fl-jobord',
                             oe-ordl.job-no + ',' + STRING(oe-ordl.job-no2) + ',' +
                             oe-ordl.job-no + ',' + STRING(oe-ordl.job-no2) + ',' +
                             STRING(v-reprint) + ',' + STRING(oe-ordl.ord-no)). /* gdm - 07130906*/
     RUN Get_Procedure IN Persistent-Handle ('job_.',OUTPUT run-proc,YES).

  END.
  ELSE DO:
     v-reprint = oe-ordl.ftick-prnt.
     MESSAGE "No job to print a job ticket." VIEW-AS ALERT-BOX ERROR.
     RETURN NO-APPLY.
  END.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print_RelTicket B-table-Win 
PROCEDURE print_RelTicket :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF NOT AVAILABLE oe-ordl THEN RETURN.

  RUN custom/setUserPrint.p (oe-ordl.company,'oe_relh_.',
                             'begin_ord-no,end_ord-no,tb_printed',
                             STRING(oe-ordl.ord-no) + ',' +
                             string(oe-ordl.ord-no) + ',' + "NO" ).

  RUN Get_Procedure IN Persistent-Handle ('oe-relh_.',OUTPUT run-proc,YES).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE record-updated B-table-Win 
PROCEDURE record-updated :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

  DEFINE VARIABLE CHAR-hdl AS cha NO-UNDO.


  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source", OUTPUT char-hdl). /* viewer */
  RUN get-link-handle IN adm-broker-hdl (WIDGET-HANDLE(char-hdl),"record-source", OUTPUT char-hdl). /* browser */

  RUN repo-query IN WIDGET-HANDLE(char-hdl) (ip-rowid).

  IF NOT AVAILABLE oe-ordl OR ROWID(oe-ordl) NE ip-rowid THEN
    REPOSITION {&browse-name} TO ROWID ip-rowid.

  RUN dispatch ('open-query').

  RUN dispatch ('row-changed').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refresh-releases B-table-Win 
PROCEDURE refresh-releases :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE hProc AS HANDLE NO-UNDO.
hProc = SESSION:FIRST-PROCEDURE.
DO WHILE VALID-HANDLE(hProc):
    IF INDEX(hProc:FILE-NAME, "b-ordrel") GT 0 THEN
        LEAVE. /* found it. */
    hProc = hProc:NEXT-SIBLING.
END.

IF VALID-HANDLE(hProc) THEN DO:
    RUN local-open-query IN hProc.
END.

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

DEFINE BUFFER bf-rel FOR oe-rel .

SESSION:SET-WAIT-STATE ('general').
FIND FIRST oe-ctrl WHERE oe-ctrl.company = g_company NO-LOCK NO-ERROR.
{sys/inc/addrelse.i}
DEFINE VARIABLE v-stat AS cha NO-UNDO.

RUN oe/CheckAckPrint.p(ROWID(oe-ord)).
RUN check-release NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.

fil_id = RECID(oe-ordl).  
RUN oe/autorel.p .

SESSION:SET-WAIT-STATE ('').  

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
  DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

  DEFINE VARIABLE CHAR-hdl AS cha NO-UNDO.

  DEFINE BUFFER bf-ordl FOR oe-ordl.
  DEFINE BUFFER bf-ord FOR oe-ord.


  RUN dispatch ('open-query').

  IF ip-rowid EQ ? THEN
  FIND LAST bf-ordl
      WHERE bf-ordl.company EQ oe-ord.company
        AND bf-ordl.ord-no  EQ oe-ord.ord-no
      NO-LOCK NO-ERROR.

  ELSE FIND bf-ordl WHERE ROWID(bf-ordl) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAILABLE bf-ordl THEN DO:
    ip-rowid = ROWID(bf-ordl).
    REPOSITION {&browse-name} TO ROWID ROWID(bf-ordl).
    RUN dispatch ('row-changed').
  END.

  ELSE
  DO TRANSACTION:
    FIND bf-ord WHERE ROWID(bf-ord) EQ ROWID(oe-ord) NO-ERROR.
    IF AVAILABLE bf-ord THEN DELETE bf-ord.
  END.

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source", OUTPUT char-hdl). /* viewer */
  RUN get-link-handle IN adm-broker-hdl (WIDGET-HANDLE(char-hdl),"record-source", OUTPUT char-hdl). /* browser */

  RUN reopen-query1 IN WIDGET-HANDLE(char-hdl) (ip-rowid).

  APPLY "value-changed" TO BROWSE {&browse-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reposit-item B-table-Win 
PROCEDURE reposit-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-recid-hd AS RECID NO-UNDO.
  DEFINE INPUT PARAMETER ip-recid-line AS RECID NO-UNDO.

  DEFINE BUFFER b-oe-ordl FOR oe-ordl.

  IF ip-recid-hd <> ? AND ip-recid-line <> ? THEN DO:
     FIND b-oe-ordl WHERE RECID(b-oe-ordl) = ip-recid-line NO-LOCK NO-ERROR.
     IF AVAILABLE b-oe-ordl THEN DO:
        REPOSITION {&browse-name} TO RECID ip-recid-line NO-ERROR.
        RUN dispatch ('row-changed').
        APPLY "VALUE-CHANGED" TO BROWSE {&browse-name}.
     END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select-his B-table-Win 
PROCEDURE select-his :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF AVAILABLE oe-ordl THEN DO:
    FIND xoe-ord WHERE xoe-ord.company = oe-ordl.company AND
                       xoe-ord.ord-no = oe-ordl.ord-no NO-LOCK NO-ERROR.

    FIND FIRST cust {sys/ref/custW.i} AND
                    cust.cust-no EQ xoe-ord.cust-no
                    use-index cust no-lock no-error.
                  
    DEFINE VARIABLE char-hdl AS cha NO-UNDO.
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-source",OUTPUT char-hdl).
    RUN init-history IN WIDGET-HANDLE(char-hdl) (THIS-PROCEDURE).

    RUN dispatch ('open-query').
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select-price B-table-Win 
PROCEDURE select-price :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE VARIABLE lv-tmp-recid AS RECID NO-UNDO.
 DEFINE VARIABLE ld-prev-t-price LIKE oe-ordl.t-price NO-UNDO.

 
 FIND xoe-ord WHERE xoe-ord.company = oe-ordl.company AND
                     xoe-ord.ord-no = oe-ordl.ord-no NO-LOCK NO-ERROR.

  ASSIGN v-i-qty = 0
         v-price-lev = 0.
  /* Get appropriate level */
  RUN oe/oe-level.p(BUFFER xoe-ord, OUTPUT v-price-lev).
 
  REPEAT:
       MESSAGE "What Level should the Items be Repriced At?" UPDATE v-price-lev .
       IF v-price-lev LE 0 OR v-price-lev GE 11 THEN DO:
         MESSAGE "Level must be Between 1 and 10.  Please ReEnter." VIEW-AS ALERT-BOX ERROR.
         NEXT.
       END.
       LEAVE.
   END.
   ASSIGN
    lv-tmp-recid    = RECID(oe-ordl)
    ld-prev-t-price = oe-ordl.t-price.
   
   RUN oe/oe-repr1.p(BUFFER xoe-ord, v-price-lev).
   {&open-query-{&browse-name}}
   REPOSITION {&browse-name} TO RECID lv-tmp-recid.

   RUN oe/calcordt.p (ROWID(oe-ord)).
   FIND FIRST cust NO-LOCK 
          WHERE cust.company EQ cocode
          AND cust.cust-no EQ oe-ord.cust-no NO-ERROR.
   IF (ld-prev-t-price NE oe-ordl.t-price) 
       AND AVAIL cust AND cust.active NE "X" AND AVAIL oe-ord AND oe-ord.TYPE NE "T" THEN RUN oe/creditck.p (ROWID(oe-ord),YES).
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select-stat B-table-Win 
PROCEDURE select-stat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND xoe-ord WHERE xoe-ord.company = oe-ordl.company AND
                     xoe-ord.ord-no = oe-ordl.ord-no NO-LOCK NO-ERROR.

  FIND FIRST cust {sys/ref/custW.i} AND
                  cust.cust-no EQ xoe-ord.cust-no
                  use-index cust no-lock no-error.
                  
  /*run oe/d-credit.w./* (cocode,cust.cust-no).*/ */
  DEFINE VARIABLE char-hdl AS cha NO-UNDO.
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-source",OUTPUT char-hdl).
  RUN init-credit-inq IN WIDGET-HANDLE(char-hdl) (THIS-PROCEDURE).

  RUN dispatch ('open-query').
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select_notes B-table-Win 
PROCEDURE select_notes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* This will display "PDC" records - Promise Date change */
IF AVAILABLE oe-ordl THEN
  RUN windows/datenote.w (INPUT oe-ordl.rec_key, INPUT PROGRAM-NAME(1), "PDC,DDC", "P,D" ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-max B-table-Win 
PROCEDURE get-max :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* This will display "PDC" records - Promise Date change */
    RUN dispatch ('initialize').                           
   
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-oeordl B-table-Win 
PROCEDURE send-oeordl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER v-oeordl AS ROWID.
IF AVAIL(oe-ordl) THEN
  v-oeordl = ROWID(oe-ordl).
ELSE
  v-oeordl = ?.
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
  {src/adm/template/snd-list.i "oe-ord"}
  {src/adm/template/snd-list.i "oe-ordl"}

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION dueDateChangeRsn B-table-Win 
FUNCTION dueDateChangeRsn RETURNS CHARACTER
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
                DEFINE VARIABLE result AS CHARACTER NO-UNDO.
        IF AVAILABLE oe-ordl AND oe-ordl.spare-char-5 GT "" THEN 
          RESULT = ENTRY(2, oe-ordl.spare-char-5). 
                RETURN result.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION dueDateChangeUser B-table-Win 
FUNCTION dueDateChangeUser RETURNS CHARACTER
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
                DEFINE VARIABLE result AS CHARACTER NO-UNDO.
        IF AVAILABLE oe-ordl AND oe-ordl.spare-char-5 GT "" THEN 
          RESULT = ENTRY(1, oe-ordl.spare-char-5). 
                RETURN result.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-extended-price B-table-Win 
FUNCTION get-extended-price RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE BUFFER b-oe-ordl FOR oe-ordl.

  DEFINE VARIABLE ld AS DECIMAL NO-UNDO.

  FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) NO-LOCK.

  ld = b-oe-ordl.t-price.

  IF b-oe-ordl.stat EQ "C" OR b-oe-ordl.opened EQ NO THEN
     FOR EACH ar-invl FIELDS(inv-no amt) WHERE
         ar-invl.company EQ cocode AND
         ar-invl.ord-no EQ b-oe-ordl.ord-no AND
         ar-invl.i-no EQ b-oe-ordl.i-no
         NO-LOCK
         BY ar-invl.inv-no DESCENDING:

         ld = ar-invl.amt.
         LEAVE.
     END.

  RETURN ld.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-pr-uom B-table-Win 
FUNCTION get-pr-uom RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEFINE BUFFER b-oe-ordl FOR oe-ordl.
   DEFINE VARIABLE lv-uom AS CHARACTER NO-UNDO.
  
   FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) NO-LOCK.
  
   lv-uom = b-oe-ordl.pr-uom.
  
   IF b-oe-ordl.stat EQ "C" OR b-oe-ordl.opened EQ NO THEN
      FOR EACH ar-invl FIELDS(inv-no pr-uom) WHERE
          ar-invl.company EQ cocode AND
          ar-invl.ord-no EQ b-oe-ordl.ord-no AND
          ar-invl.i-no EQ b-oe-ordl.i-no
          NO-LOCK
          BY ar-invl.inv-no DESCENDING:
  
          lv-uom = ar-invl.pr-uom.
          LEAVE.
      END.
  
   RETURN lv-uom.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-price-disc B-table-Win 
FUNCTION get-price-disc RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE BUFFER b-oe-ordl FOR oe-ordl.

  DEFINE VARIABLE ld AS DECIMAL NO-UNDO.

  FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) NO-LOCK.

  ld = b-oe-ordl.price * (1 - (b-oe-ordl.disc / 100)).

  IF b-oe-ordl.stat EQ "C" OR b-oe-ordl.opened EQ NO THEN
     FOR EACH ar-invl FIELDS(inv-no unit-pr disc) WHERE
         ar-invl.company EQ cocode AND
         ar-invl.ord-no EQ b-oe-ordl.ord-no AND
         ar-invl.i-no EQ b-oe-ordl.i-no
         NO-LOCK
         BY ar-invl.inv-no DESCENDING:

         ld = ar-invl.unit-pr * (1 - (ar-invl.disc / 100)).
         LEAVE.
     END.

  RETURN ld.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

