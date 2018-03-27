&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          emptrack         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  browsers/b-vwusage.w

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
&SCOPED-DEFINE yellowColumnsName vend-whse-trans
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}

DEF TEMP-TABLE w-rowid FIELD w-rowid AS CHAR.
DEFINE TEMP-TABLE tt-bol LIKE oe-boll.

DEF VAR ld-cost         AS DECI NO-UNDO.
DEF VAR lv-uom          AS CHAR NO-UNDO.
DEF VAR ls-prev-po      AS CHAR NO-UNDO.
DEF VAR hd-post         AS WIDGET-HANDLE NO-UNDO.
DEF VAR hd-post-child   AS WIDGET-HANDLE NO-UNDO.
DEF VAR ll-help-run     AS LOG NO-UNDO. /* set on browse help, reset row-entry */
DEF VAR v-char-val      AS CHAR  NO-UNDO.
DEF VAR v-lvwbol        AS LOG INIT NO NO-UNDO.
DEF VAR v-bol-no-valid  AS LOGICAL NO-UNDO.

&SCOPED-DEFINE item-key-phrase TRUE

ASSIGN
 cocode = g_company.

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
&Scoped-define INTERNAL-TABLES vend-whse-trans

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table vend-whse-trans.r-no ~
vend-whse-trans.vend-bol-no vend-whse-trans.cust-part-no ~
vend-whse-trans.trans-qty vend-whse-trans.plant-tot-oh-qty ~
vend-whse-trans.trans-date vend-whse-trans.vendor-code ~
vend-whse-trans.vendor-plant-code vend-whse-trans.vendor-dept-code ~
vend-whse-trans.fg-item-no vend-whse-trans.vend-ord-no ~
vend-whse-trans.item-line-no vend-whse-trans.item-po-no ~
vend-whse-trans.sell-price vend-whse-trans.vend-job-no ~
vend-whse-trans.vend-job-no2 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table ~
vend-whse-trans.vend-bol-no vend-whse-trans.trans-qty ~
vend-whse-trans.trans-date vend-whse-trans.vendor-plant-code ~
vend-whse-trans.vendor-dept-code 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table vend-whse-trans
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table vend-whse-trans
&Scoped-define QUERY-STRING-Browser-Table FOR EACH vend-whse-trans WHERE ~{&KEY-PHRASE} ~
      AND vend-whse-trans.trans-type = "R" SHARE-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH vend-whse-trans WHERE ~{&KEY-PHRASE} ~
      AND vend-whse-trans.trans-type = "R" SHARE-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table vend-whse-trans
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table vend-whse-trans


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 browse-order auto_find ~
Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
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
     SIZE 116 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 95 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 143.8 BY 2.62.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      vend-whse-trans SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table SHARE-LOCK NO-WAIT DISPLAY
      vend-whse-trans.r-no COLUMN-LABEL "Seq#" FORMAT ">>>>>>>9":U
            LABEL-BGCOLOR 14
      vend-whse-trans.vend-bol-no COLUMN-LABEL "Suppliers!BOL No" FORMAT ">>>>>>>9":U
            WIDTH 13.2 LABEL-BGCOLOR 14
      vend-whse-trans.cust-part-no FORMAT "x(12)":U WIDTH 14 LABEL-BGCOLOR 14
      vend-whse-trans.trans-qty COLUMN-LABEL "Receipt!Quantity" FORMAT "->>,>>>,>>9":U
            LABEL-BGCOLOR 14
      vend-whse-trans.plant-tot-oh-qty FORMAT "->>,>>>,>>9":U WIDTH 15.6
            LABEL-BGCOLOR 14
      vend-whse-trans.trans-date COLUMN-LABEL "Customers!Receipt Date" FORMAT "99/99/99":U
            WIDTH 17.8 LABEL-BGCOLOR 14
      vend-whse-trans.vendor-code COLUMN-LABEL "Custmers!A/P Code" FORMAT "x(8)":U
            WIDTH 11.2 LABEL-BGCOLOR 14
      vend-whse-trans.vendor-plant-code FORMAT "x(8)":U WIDTH 13.2
            LABEL-BGCOLOR 14
      vend-whse-trans.vendor-dept-code FORMAT "x(8)":U WIDTH 13.2
            LABEL-BGCOLOR 14
      vend-whse-trans.fg-item-no FORMAT "x(15)":U LABEL-BGCOLOR 14
      vend-whse-trans.vend-ord-no FORMAT ">>>>>9":U WIDTH 12.6
            LABEL-BGCOLOR 14
      vend-whse-trans.item-line-no FORMAT "99":U WIDTH 7.2
      vend-whse-trans.item-po-no FORMAT "x(15)":U WIDTH 13.2 LABEL-BGCOLOR 14
      vend-whse-trans.sell-price FORMAT ">,>>>,>>9.99":U WIDTH 20
            LABEL-BGCOLOR 14
      vend-whse-trans.vend-job-no COLUMN-LABEL "Suppliers!Job#" FORMAT "x(6)":U
            WIDTH 13.2 LABEL-BGCOLOR 14
      vend-whse-trans.vend-job-no2 FORMAT ">9":U
  ENABLE
      vend-whse-trans.vend-bol-no
      vend-whse-trans.trans-qty HELP "Quantity Consummed"
      vend-whse-trans.trans-date HELP "Date Boxes Were Consummed"
      vend-whse-trans.vendor-plant-code
      vend-whse-trans.vendor-dept-code
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 144 BY 15.24
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 16.48 COL 8 HELP
          "Select Browser Sort Order" NO-LABEL
     fi_sortby AT ROW 16.48 COL 102.6 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     auto_find AT ROW 17.67 COL 12 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 17.67 COL 130.6 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 16.48 COL 3
     RECT-4 AT ROW 16.24 COL 1.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


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
         HEIGHT             = 18.1
         WIDTH              = 145.4.
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
       Browser-Table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 4
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE
       Browser-Table:COLUMN-RESIZABLE IN FRAME F-Main       = TRUE.

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
     _TblList          = "vend-whse-trans"
     _Options          = "SHARE-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "vend-whse-trans.trans-type = ""R"""
     _FldNameList[1]   > vend-whse-trans.r-no
"vend-whse-trans.r-no" "Seq#" ? "integer" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > vend-whse-trans.vend-bol-no
"vend-whse-trans.vend-bol-no" "Suppliers!BOL No" ? "integer" ? ? ? 14 ? ? yes ? no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > vend-whse-trans.cust-part-no
"vend-whse-trans.cust-part-no" ? ? "character" ? ? ? 14 ? ? no "Customers Part Number" no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > vend-whse-trans.trans-qty
"vend-whse-trans.trans-qty" "Receipt!Quantity" "->>,>>>,>>9" "decimal" ? ? ? 14 ? ? yes "Quantity Consummed" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > vend-whse-trans.plant-tot-oh-qty
"vend-whse-trans.plant-tot-oh-qty" ? "->>,>>>,>>9" "decimal" ? ? ? 14 ? ? no "On Hand Quantity at Customers Plant" no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > vend-whse-trans.trans-date
"vend-whse-trans.trans-date" "Customers!Receipt Date" ? "date" ? ? ? 14 ? ? yes "Date Boxes Were Consummed" no no "17.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > vend-whse-trans.vendor-code
"vend-whse-trans.vendor-code" "Custmers!A/P Code" ? "character" ? ? ? 14 ? ? no "A/P Code for Supplier in Customers Vendors File" no no "11.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > vend-whse-trans.vendor-plant-code
"vend-whse-trans.vendor-plant-code" ? ? "character" ? ? ? 14 ? ? yes ? no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > vend-whse-trans.vendor-dept-code
"vend-whse-trans.vendor-dept-code" ? ? "character" ? ? ? 14 ? ? yes ? no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > vend-whse-trans.fg-item-no
"vend-whse-trans.fg-item-no" ? ? "character" ? ? ? 14 ? ? no "Suppliers Finished Goods Part Number" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > vend-whse-trans.vend-ord-no
"vend-whse-trans.vend-ord-no" ? ? "integer" ? ? ? 14 ? ? no "Suppliers Order Number for Customers Purchase Order" no no "12.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > vend-whse-trans.item-line-no
"vend-whse-trans.item-line-no" ? ? "integer" ? ? ? ? ? ? no "Line Number for Customers Part" no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > vend-whse-trans.item-po-no
"vend-whse-trans.item-po-no" ? "x(15)" "character" ? ? ? 14 ? ? no "Purchase Order Number for Customers Part" no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > vend-whse-trans.sell-price
"vend-whse-trans.sell-price" ? ">,>>>,>>9.99" "decimal" ? ? ? 14 ? ? no "Cost of Customers Part Number On PO" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > vend-whse-trans.vend-job-no
"vend-whse-trans.vend-job-no" "Suppliers!Job#" ? "character" ? ? ? 14 ? ? no "Suppliers Job Number for Customers Purchase Order" no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   = vend-whse-trans.vend-job-no2
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
   
   RUN new-state in phandle ('update-begin':U).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON HELP OF Browser-Table IN FRAME F-Main
DO:
   DEF VAR lv-handle AS WIDGET-HANDLE NO-UNDO.
   DEF VAR hlp-recid AS RECID NO-UNDO.
   DEF VAR lv-rowids AS CHAR NO-UNDO.
   DEF VAR li        AS INT NO-UNDO.

   DEF BUFFER b-oe-ordl FOR oe-ordl.
   DEF BUFFER b-itemfg FOR itemfg.

   CASE FOCUS:NAME:
/*       WHEN "fg-item-no" THEN DO:                                                                             */
/*          RUN windows/l-itemfg.w (cocode, "", FOCUS:SCREEN-VALUE, OUTPUT v-char-val).                         */
/*          IF v-char-val <> "" THEN DO:                                                                        */
/*             ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,v-char-val).                                                 */
/*             FIND FIRST b-itemfg WHERE b-itemfg.company = cocode                                              */
/*                                   AND b-itemfg.i-no = ENTRY(1,v-char-val) NO-LOCK NO-ERROR.                  */
/*             IF AVAILABLE(b-itemfg) THEN DO:                                                                  */
/*                ASSIGN vend-whse-trans.cust-part-no:SCREEN-VALUE IN BROWSE {&browse-name} = b-itemfg.part-no. */
/*             END.                                                                                             */
/*          END.                                                                                                */
/*          RETURN NO-APPLY.                                                                                    */
/*       END.                                                                                                   */
/*       WHEN "cust-part-no" THEN DO:                                                                           */
/*          RUN windows/l-itemfg.w (cocode, "", FOCUS:SCREEN-VALUE, OUTPUT v-char-val).                         */
/*          IF v-char-val <> "" THEN DO:                                                                        */
/*             ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,v-char-val).                                                 */
/*             FIND FIRST b-itemfg WHERE b-itemfg.company = cocode                                              */
/*                                   AND b-itemfg.part-no = ENTRY(1,v-char-val) NO-LOCK NO-ERROR.               */
/*             IF AVAILABLE(b-itemfg) THEN DO:                                                                  */
/*                ASSIGN vend-whse-trans.fg-item-no:SCREEN-VALUE IN BROWSE {&browse-name} = b-itemfg.i-no.      */
/*             END.                                                                                             */
/*          END.                                                                                                */
/*          RETURN NO-APPLY.                                                                                    */
/*       END.                                                                                                   */
/*       WHEN "item-po-no" then do:                                                                             */
/*          RUN windows/l-cuspo2.w (g_company,"",FOCUS:SCREEN-VALUE, OUTPUT v-char-val, OUTPUT hlp-recid).      */
/*          IF v-char-val <> "" THEN DO:                                                                        */
/*             ASSIGN                                                                                           */
/*                FOCUS:SCREEN-VALUE = ENTRY(1,v-char-val)                                                      */
/*                vend-whse-trans.fg-item-no:SCREEN-VALUE = ENTRY(2,v-char-val)                                 */
/*                vend-whse-trans.vend-ord-no:SCREEN-VALUE = ENTRY(3,v-char-val).                               */
/*             FIND FIRST b-itemfg WHERE b-itemfg.company = cocode                                              */
/*                                   AND b-itemfg.i-no = ENTRY(2,v-char-val) NO-LOCK NO-ERROR.                  */
/*             IF AVAILABLE(b-itemfg) THEN DO:                                                                  */
/*                ASSIGN vend-whse-trans.cust-part-no:SCREEN-VALUE IN BROWSE {&browse-name} = b-itemfg.part-no. */
/*                FIND FIRST b-oe-ordl WHERE b-oe-ordl.company = cocode                                         */
/*                                       AND b-oe-ordl.i-no = ENTRY(2,v-char-val)                               */
/*                                       AND b-oe-ordl.ord-no = INT(ENTRY(3,v-char-val)) NO-LOCK NO-ERROR.      */
/*                IF AVAILABLE(b-oe-ordl) THEN                                                                  */
/*                   ASSIGN                                                                                     */
/*                      vend-whse-trans.vend-job-no:SCREEN-VALUE  = b-oe-ordl.job-no                            */
/*                      vend-whse-trans.vend-job-no2:SCREEN-VALUE = STRING(b-oe-ordl.job-no2)                   */
/*                      vend-whse-trans.item-line-no:SCREEN-VALUE = STRING(b-oe-ordl.line)                      */
/*                      vend-whse-trans.sell-price:SCREEN-VALUE   = STRING(b-oe-ordl.price).                    */
/*             END.                                                                                             */
/*          END.                                                                                                */
/*          RETURN NO-APPLY.                                                                                    */
/*       END.                                                                                                   */
      WHEN "vendor-plant-code" THEN DO:
         RUN custitem/l-plntid.w (cocode,FOCUS:SCREEN-VALUE, OUTPUT v-char-val).
         IF v-char-val <> "" THEN DO:
            ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,v-char-val).
            ASSIGN vend-whse-trans.vendor-dept-code:SCREEN-VALUE IN BROWSE {&browse-name} = ENTRY(2,v-char-val).
         END.
         RETURN NO-APPLY.
      END.
      WHEN "vend-bol-no" THEN DO:      
         IF adm-new-record THEN DO:
            RUN custitem/l-vwbol.w(INPUT cocode,
                                   INPUT "",
                                   OUTPUT lv-rowids).
   
            IF lv-rowids NE "" THEN DO:
               RUN dispatch('cancel-records').     
               RUN dispatch('end-update').

               /*RUN dispatch ('open-query').*/

               v-lvwbol = YES.
               
               FOR EACH w-rowid:
                  DELETE w-rowid.
               END.
   
               DO li = 1 TO NUM-ENTRIES(lv-rowids):
                  IF ENTRY(li,lv-rowids) NE "" THEN DO:
                     CREATE w-rowid.
                     w-rowid = ENTRY(li,lv-rowids).
                  END.
               END.
   
               DO WHILE AVAIL vend-whse-trans AND CAN-FIND(FIRST w-rowid):
                  FIND FIRST w-rowid WHERE w-rowid EQ STRING(ROWID(vend-whse-trans)) NO-ERROR.
                  IF AVAIL w-rowid THEN DELETE w-rowid.
                  RUN dispatch ('get-next').
               END.
/*                RUN redisplay-header.                                                           */
            END.
         END.
         RETURN NO-APPLY.
      END.
   END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
  
  ll-help-run = no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
  /*{src/adm/template/brsleave.i}*/
  {est/brsleave.i}  /* same as src but update will be same as add record*/

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
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  {methods/template/local/setvalue.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend-whse-trans.vend-bol-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-whse-trans.vend-bol-no Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF vend-whse-trans.vend-bol-no IN BROWSE Browser-Table /* Suppliers!BOL No */
DO:
  IF NOT adm-new-record THEN DO:
     APPLY 'tab' TO SELF.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-whse-trans.vend-bol-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF vend-whse-trans.vend-bol-no IN BROWSE Browser-Table /* Suppliers!BOL No */
DO:
   IF LASTKEY NE -1 THEN DO:
      RUN val-vend-bol-no NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend-whse-trans.cust-part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-whse-trans.cust-part-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF vend-whse-trans.cust-part-no IN BROWSE Browser-Table /* Customers!Part# */
DO:
/*      IF LASTKEY NE -1 THEN DO:                    */
/*       RUN val-cust-part-no NO-ERROR.              */
/*       IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY. */
/*    END.                                           */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend-whse-trans.trans-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-whse-trans.trans-date Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF vend-whse-trans.trans-date IN BROWSE Browser-Table /* Customers!Receipt Date */
DO:
/*    IF LASTKEY NE -1 THEN DO:                      */
/*       RUN val-usage-date.                         */
/*       IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY. */
/*    END.                                           */
       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend-whse-trans.vendor-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-whse-trans.vendor-code Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF vend-whse-trans.vendor-code IN BROWSE Browser-Table /* Custmers!A/P Code */
DO:
   IF LASTKEY NE -1 THEN DO:
      RUN val-vendor-code.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend-whse-trans.vendor-plant-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-whse-trans.vendor-plant-code Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF vend-whse-trans.vendor-plant-code IN BROWSE Browser-Table /* Customers!Plant ID */
DO:
   IF LASTKEY NE -1 THEN DO:
      RUN val-plant-id NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend-whse-trans.vendor-dept-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-whse-trans.vendor-dept-code Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF vend-whse-trans.vendor-dept-code IN BROWSE Browser-Table /* Customers!Dept Code */
DO:
    IF LASTKEY NE -1 THEN DO:
      RUN val-vend-plant-dept-code NO-ERROR.        
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY. 
    END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend-whse-trans.fg-item-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-whse-trans.fg-item-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF vend-whse-trans.fg-item-no IN BROWSE Browser-Table /* Suppliers!FG Item */
DO:
   IF LASTKEY NE -1 THEN DO:
      RUN val-fg-item-no NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend-whse-trans.vend-ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-whse-trans.vend-ord-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF vend-whse-trans.vend-ord-no IN BROWSE Browser-Table /* Suppliers!Order# */
DO:
     IF LASTKEY NE -1 THEN DO:
      RUN val-vend-ord-no NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend-whse-trans.item-line-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-whse-trans.item-line-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF vend-whse-trans.item-line-no IN BROWSE Browser-Table /* Line# */
DO:
/*    IF LASTKEY NE -1 THEN DO:                      */
/*       RUN val-po-line-no NO-ERROR.                */
/*       IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY. */
/*    END.                                           */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend-whse-trans.item-po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-whse-trans.item-po-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF vend-whse-trans.item-po-no IN BROWSE Browser-Table /* Customers!PO# */
DO:
   IF LASTKEY NE -1 THEN DO:
      RUN val-item-po-no  NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend-whse-trans.sell-price
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-whse-trans.sell-price Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF vend-whse-trans.sell-price IN BROWSE Browser-Table /* Suppliers Item!Sell Price */
DO:
    IF LASTKEY NE -1 THEN DO:
      RUN val-sell-price NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend-whse-trans.vend-job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-whse-trans.vend-job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF vend-whse-trans.vend-job-no IN BROWSE Browser-Table /* Suppliers!Job# */
DO:
    IF LASTKEY NE -1 THEN DO:
      RUN val-job-no NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend-whse-trans.vend-job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-whse-trans.vend-job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF vend-whse-trans.vend-job-no2 IN BROWSE Browser-Table
DO:
     IF LASTKEY NE -1 THEN DO:
      RUN val-job-no-2 NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{custom/yellowColumns.i}
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).
DO WITH FRAME {&FRAME-NAME}:
  {custom/usrprint.i}
  auto_find:SCREEN-VALUE = "".
END.    
&ENDIF

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fill-bol B-table-Win 
PROCEDURE fill-bol :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-oe-boll FOR oe-boll.
DEF BUFFER b-vend-whse-item FOR vend-whse-item.
DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER b-vend-whse-trans FOR vend-whse-trans.
DEF BUFFER b-vend-whse-trans-hist FOR vend-whse-trans-hist.
/* gdm - 11160904 */
DEF BUFFER b-oe-bolh FOR oe-bolh.

DEF VAR v-cnt AS INT NO-UNDO.
DEF VAR v-rowid AS ROWID NO-UNDO.
DEF VAR v-r-no LIKE vend-whse-trans.r-no NO-UNDO.

/* gdm - 11160904 */
FIND FIRST b-oe-bolh WHERE b-oe-bolh.company = cocode                          
                       AND b-oe-bolh.bol-no  = INT(vend-whse-trans.vend-bol-no:SCREEN-VALUE IN BROWSE {&browse-name}) 
                       AND b-oe-bolh.posted  = YES NO-LOCK NO-ERROR.
/* gdm - 11160904 end*/

FOR EACH b-oe-boll WHERE b-oe-boll.company = cocode
/*                      AND b-oe-boll.opened =  FALSE */
                     AND b-oe-boll.bol-no = b-oe-bolh.bol-no 
                     AND CAN-FIND (b-vend-whse-item 
                                    WHERE  b-vend-whse-item.company     = b-oe-boll.company
                                      AND b-vend-whse-item.fg-item-no  = b-oe-boll.i-no)
                    NO-LOCK:

   FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company     = b-oe-boll.company
                                 AND b-vend-whse-item.fg-item-no  = TRIM(b-oe-boll.i-no) NO-LOCK NO-ERROR.
   IF AVAIL b-vend-whse-item THEN
   FIND FIRST tt-bol WHERE tt-bol.company = b-vend-whse-item.company
                       AND tt-bol.bol-no  = b-oe-boll.bol-no
                       AND tt-bol.i-no    = b-vend-whse-item.fg-item-no NO-ERROR.
      IF NOT AVAILABLE(tt-bol) THEN DO:
         CREATE tt-bol.
         BUFFER-COPY b-oe-boll EXCEPT b-oe-boll.qty TO tt-bol NO-ERROR.
         v-cnt = v-cnt + 1.
      END.
      tt-bol.qty = tt-bol.qty + b-oe-boll.qty.
END.

IF AVAILABLE(tt-bol) AND v-cnt = 1 THEN DO:
   
   FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company     = tt-bol.company
                                 AND b-vend-whse-item.fg-item-no  = tt-bol.i-no NO-LOCK NO-ERROR.
   ASSIGN
      vend-whse-trans.fg-item-no:SCREEN-VALUE IN BROWSE {&browse-name}    = CAPS(tt-bol.i-no)
      vend-whse-trans.item-line-no:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(tt-bol.line)
      vend-whse-trans.item-po-no:SCREEN-VALUE IN BROWSE {&browse-name}    = CAPS(tt-bol.po-no)
      vend-whse-trans.trans-qty:SCREEN-VALUE IN BROWSE {&browse-name}     = STRING(tt-bol.qty)
      vend-whse-trans.trans-date:SCREEN-VALUE IN BROWSE {&browse-name}    = STRING(tt-bol.bol-date)                                                                      
      vend-whse-trans.vend-job-no:SCREEN-VALUE IN BROWSE {&browse-name}   = FILL(" ",6 - LENGTH(TRIM(tt-bol.job-no))) + TRIM(tt-bol.job-no)
      vend-whse-trans.vend-job-no2:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(tt-bol.job-no2) 
      vend-whse-trans.vend-ord-no:SCREEN-VALUE IN BROWSE {&browse-name}   = STRING(tt-bol.ord-no)
      vend-whse-trans.vendor-code:SCREEN-VALUE IN BROWSE {&browse-name}        = CAPS(b-vend-whse-item.vendor-code)
      vend-whse-trans.vendor-dept-code:SCREEN-VALUE IN BROWSE {&browse-name}   = CAPS(b-vend-whse-item.vendor-dept-code)
      vend-whse-trans.vendor-plant-code:SCREEN-VALUE IN BROWSE {&browse-name}  = CAPS(b-vend-whse-item.vendor-plant-code)
      vend-whse-trans.cust-part-no:SCREEN-VALUE IN BROWSE {&browse-name}       = CAPS(b-vend-whse-item.cust-part-no)
      vend-whse-trans.plant-tot-oh-qty:SCREEN-VALUE IN BROWSE {&browse-name}   = STRING(b-vend-whse-item.plant-tot-oh-qty + tt-bol.qty).
   
   FIND FIRST b-oe-ordl WHERE b-oe-ordl.company  EQ tt-bol.company
                          AND b-oe-ordl.i-no     EQ tt-bol.i-no
                          AND b-oe-ordl.job-no   EQ tt-bol.job-no
                          AND b-oe-ordl.ord-no   EQ tt-bol.ord-no
                          AND b-oe-ordl.line     EQ tt-bol.LINE NO-LOCK NO-ERROR.
   IF AVAILABLE(b-oe-ordl) THEN
      vend-whse-trans.sell-price:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(b-oe-ordl.t-price).
END.
ELSE DO:
   
   RUN local-cancel-record.

   FOR EACH tt-bol:
      v-r-no = 0.
      FIND LAST b-vend-whse-trans USE-INDEX r-no NO-LOCK NO-ERROR.
      IF AVAIL b-vend-whse-trans AND b-vend-whse-trans.r-no > v-r-no THEN v-r-no = b-vend-whse-trans.r-no.
      
      FIND LAST b-vend-whse-trans-hist USE-INDEX r-no NO-LOCK NO-ERROR.
      IF AVAIL b-vend-whse-trans-hist AND b-vend-whse-trans-hist.r-no GT v-r-no THEN v-r-no = b-vend-whse-trans-hist.r-no.
      
      
      DO WHILE TRUE:
         v-r-no = v-r-no + 1.
      
         FIND FIRST b-vend-whse-trans-hist WHERE b-vend-whse-trans-hist.r-no = v-r-no USE-INDEX r-no NO-LOCK NO-ERROR.
         IF AVAIL b-vend-whse-trans-hist THEN NEXT.
      
         FIND FIRST b-vend-whse-trans WHERE b-vend-whse-trans.r-no = v-r-no USE-INDEX r-no NO-LOCK NO-ERROR.
         IF AVAIL b-vend-whse-trans THEN NEXT.
      
         LEAVE.
      END.
      
      CREATE vend-whse-trans. 
      ASSIGN
         vend-whse-trans.company       = tt-bol.company
         vend-whse-trans.r-no          = v-r-no
         vend-whse-trans.trans-type    = "R"
         vend-whse-trans.trans-date    = tt-bol.bol-date
         vend-whse-trans.create-date   = TODAY
         vend-whse-trans.create-time   = TIME
         vend-whse-trans.create-userid = USERID("nosweat")
         vend-whse-trans.rec_key       = STRING(YEAR(TODAY), "9999") + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99") + STRING(TIME)
         vend-whse-trans.upd-date      = TODAY
         vend-whse-trans.upd-time      = TIME
         vend-whse-trans.upd-userid    = USERID("nosweat")
         vend-whse-trans.cust-no       = CAPS(tt-bol.cust-no)         
         vend-whse-trans.fg-item-no    = CAPS(tt-bol.i-no)
         vend-whse-trans.item-line-no  = tt-bol.line
         vend-whse-trans.item-po-no    = tt-bol.po-no
         vend-whse-trans.trans-qty     = tt-bol.qty
         vend-whse-trans.vend-bol-no   = tt-bol.bol-no 
         vend-whse-trans.vend-job-no   = tt-bol.job-no
         vend-whse-trans.vend-job-no2  = tt-bol.job-no2 
         vend-whse-trans.vend-ord-no   = tt-bol.ord-no.
   
      FIND FIRST b-oe-ordl WHERE b-oe-ordl.company  EQ tt-bol.company
                             AND b-oe-ordl.i-no     EQ tt-bol.i-no
                             AND b-oe-ordl.job-no   EQ tt-bol.job-no
                             AND b-oe-ordl.ord-no   EQ tt-bol.ord-no
                             AND b-oe-ordl.line     EQ tt-bol.LINE NO-LOCK NO-ERROR.
      IF AVAILABLE(b-oe-ordl) THEN
         vend-whse-trans.sell-price = b-oe-ordl.t-price.
      
      FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company = tt-bol.company
                                    AND b-vend-whse-item.fg-item = tt-bol.i-no NO-LOCK NO-ERROR.
      IF AVAILABLE(b-vend-whse-item) THEN DO:
      ASSIGN   
         vend-whse-trans.vendor-code        = CAPS(b-vend-whse-item.vendor-code)
         vend-whse-trans.vendor-dept-code   = CAPS(b-vend-whse-item.vendor-dept-code)
         vend-whse-trans.vendor-plant-code  = CAPS(b-vend-whse-item.vendor-plant-code)
         vend-whse-trans.cust-part-no       = CAPS(b-vend-whse-item.cust-part-no)
         vend-whse-trans.plant-tot-oh-qty   = b-vend-whse-item.plant-tot-oh-qty + vend-whse-trans.trans-qty.
      END.
      v-rowid = ROWID(vend-whse-trans).
   END.
   RUN repo-query(INPUT v-rowid).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE is-in-update B-table-Win 
PROCEDURE is-in-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF OUTPUT PARAM op-in-update AS LOG NO-UNDO.

   op-in-update = IF adm-brs-in-update OR adm-new-record THEN YES ELSE NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement B-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-vend-whse-item FOR vend-whse-item.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
      vend-whse-trans.upd-date     = TODAY
      vend-whse-trans.upd-time     = TIME
      vend-whse-trans.upd-userid   = USERID("nosweat").

     FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company = cocode 
                                 AND b-vend-whse-item.vendor-code = vend-whse-trans.vendor-code:SCREEN-VALUE IN BROWSE {&browse-name} 
                                 AND b-vend-whse-item.vendor-plant-code = vend-whse-trans.vendor-plant-code:SCREEN-VALUE IN BROWSE {&browse-name}
                                 AND b-vend-whse-item.fg-item-no = vend-whse-trans.cust-part-no:SCREEN-VALUE IN BROWSE {&browse-name}
                                 AND b-vend-whse-item.cust-part-no = vend-whse-trans.cust-part-no:SCREEN-VALUE IN BROWSE {&browse-name} 
                                 AND b-vend-whse-item.vendor-dept-code = vend-whse-trans.vendor-dept-code:SCREEN-VALUE IN BROWSE {&browse-name} NO-ERROR.
   IF AVAILABLE(b-vend-whse-item) THEN DO:
      ASSIGN
         vend-whse-trans.plant-tot-oh-qty = b-vend-whse-item.plant-tot-oh-qty + DECI(vend-whse-trans.trans-qty:SCREEN-VALUE IN BROWSE {&browse-name}).
   END.
   ASSIGN BROWSE {&browse-name} vend-whse-trans.cust-part-no.
   ASSIGN BROWSE {&browse-name} vend-whse-trans.fg-item-no.
   ASSIGN BROWSE {&browse-name} vend-whse-trans.item-line-no.
   ASSIGN BROWSE {&browse-name} vend-whse-trans.item-po-no.
   ASSIGN BROWSE {&browse-name} vend-whse-trans.trans-qty.
   ASSIGN BROWSE {&browse-name} vend-whse-trans.vend-job-no.
   ASSIGN BROWSE {&browse-name} vend-whse-trans.vend-job-no2.
   ASSIGN BROWSE {&browse-name} vend-whse-trans.vend-ord-no.
   ASSIGN BROWSE {&browse-name} vend-whse-trans.vendor-code.
   ASSIGN BROWSE {&browse-name} vend-whse-trans.vendor-dept-code.
   ASSIGN BROWSE {&browse-name} vend-whse-trans.vendor-plant-code.

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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-r-no LIKE vend-whse-trans.r-no NO-UNDO.

DEF BUFFER b-vend-whse-trans FOR vend-whse-trans.

IF v-lvwbol = YES THEN
   RETURN NO-APPLY.

/* Code placed here will execute PRIOR to standard behavior. */
v-r-no = 0.
FIND LAST b-vend-whse-trans USE-INDEX r-no NO-LOCK NO-ERROR.
IF AVAIL b-vend-whse-trans AND b-vend-whse-trans.r-no > v-r-no THEN v-r-no = b-vend-whse-trans.r-no.

FIND LAST vend-whse-trans-hist USE-INDEX r-no NO-LOCK NO-ERROR.
IF AVAIL vend-whse-trans-hist AND vend-whse-trans-hist.r-no GT v-r-no THEN v-r-no = vend-whse-trans-hist.r-no.

/* Dispatch standard ADM method.                             */
RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

/* Code placed here will execute AFTER standard behavior.    */
DO WHILE TRUE:
   v-r-no = v-r-no + 1.

   FIND FIRST vend-whse-trans-hist WHERE vend-whse-trans-hist.r-no = v-r-no USE-INDEX r-no NO-LOCK NO-ERROR.
   IF AVAIL vend-whse-trans-hist THEN NEXT.

   FIND FIRST b-vend-whse-trans WHERE b-vend-whse-trans.r-no = v-r-no USE-INDEX r-no NO-LOCK NO-ERROR.
   IF AVAIL b-vend-whse-trans THEN NEXT.

   LEAVE.
END.

ASSIGN
   vend-whse-trans.company = g_company
   vend-whse-trans.r-no    = v-r-no
   vend-whse-trans.trans-type = "R".

IF adm-adding-record THEN DO:
   ASSIGN
      vend-whse-trans.create-date     = TODAY
      vend-whse-trans.create-time     = TIME
      vend-whse-trans.create-userid   = USERID("nosweat")
      vend-whse-trans.rec_key         = STRING(YEAR(TODAY), "9999") + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99") + STRING(TIME).

   DISPLAY vend-whse-trans.r-no WITH BROWSE {&browse-name}.
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

  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields B-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  if valid-handle(hd-post-child) then  hd-post-child:sensitive = yes.
            /* value assigned from local-enable-fields*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var out-hd-lst as cha no-undo.
  def var ii as int no-undo.
  def var hd-next as widget-handle no-undo.
  DEF VAR li AS INT NO-UNDO.

   
  /* Code placed here will execute PRIOR to standard behavior. */

  DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY 'cursor-left' TO {&BROWSE-NAME}.
    END.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  DO WITH FRAME {&FRAME-NAME}:
    APPLY "entry" TO vend-whse-trans.vend-bol-no IN BROWSE {&browse-name}.
  END.
 

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

   /*RUN val-vend-bol-no NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN val-plant-id NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN val-vend-plant-dept-code NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY. */

   /* Dispatch standard ADM method.                             */
   RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

   /* Code placed here will execute AFTER standard behavior.    */

   RUN repo-query (ROWID(vend-whse-trans)).

   DO WITH FRAME {&FRAME-NAME}:
      DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
         APPLY 'cursor-left' TO {&BROWSE-NAME}.
      END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE redisplay-header B-table-Win 
PROCEDURE redisplay-header :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
 RUN reopen-query IN WIDGET-HANDLE(char-hdl).
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
  RUN dispatch ("open-query").
  RUN redisplay-header.
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
    RUN clear_auto_find.
    RUN change-order (browse-order:SCREEN-VALUE).
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
  END.

  RUN dispatch ('row-changed').

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
  {src/adm/template/snd-list.i "vend-whse-trans"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val-plant-id B-table-Win 
PROCEDURE val-plant-id :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

   IF NOT CAN-FIND(FIRST vend-plant WHERE vend-plant.company = cocode
                                      AND vend-plant.vendor-code = vend-whse-trans.vendor-code:SCREEN-VALUE IN BROWSE {&browse-name}
                                      AND vend-plant.plant-id    = vend-whse-trans.vendor-plant-code:SCREEN-VALUE IN BROWSE {&browse-name}) THEN DO:
      MESSAGE "Invalid Customers Plant ID.      " 
         VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO vend-whse-trans.vendor-plant-code IN BROWSE {&browse-name}.
      RETURN ERROR.
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val-usage-date B-table-Win 
PROCEDURE val-usage-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
   IF vend-whse-trans.trans-date:SCREEN-VALUE IN BROWSE {&browse-name} = "" THEN DO:
      MESSAGE "Invalid Usage Date     " VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO vend-whse-trans.trans-date IN BROWSE {&browse-name}.
      RETURN ERROR.
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val-vend-bol-no B-table-Win 
PROCEDURE val-vend-bol-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-oe-boll FOR oe-boll.
DEF BUFFER b-vend-whse-item FOR vend-whse-item.

/* gdm - 11160904 */
DEF BUFFER b-oe-bolh FOR oe-bolh.

DO WITH FRAME {&FRAME-NAME}:
/* gdm - 11160904
   FIND FIRST b-oe-boll WHERE b-oe-boll.company = cocode
                          AND b-oe-boll.opened  = NO 
                          AND b-oe-boll.bol-no  = INT(vend-whse-trans.vend-bol-no:SCREEN-VALUE IN BROWSE {&browse-name}) NO-LOCK NO-ERROR.

   IF NOT AVAILABLE(b-oe-boll) THEN DO:
*/
   FIND FIRST b-oe-bolh WHERE b-oe-bolh.company = cocode                          
                          AND b-oe-bolh.bol-no  = INT(vend-whse-trans.vend-bol-no:SCREEN-VALUE IN BROWSE {&browse-name}) 
                          AND b-oe-bolh.posted  = YES NO-LOCK NO-ERROR.

   IF NOT AVAILABLE(b-oe-bolh) THEN DO:
      MESSAGE "Invalid Suppliers BOL Number        " 
         VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO vend-whse-trans.vend-bol-no IN BROWSE {&browse-name}.
      RETURN ERROR.
   END.
   ELSE DO:
    /* gdm - 11160904 */
      FIND FIRST b-oe-boll NO-LOCK 
        WHERE b-oe-boll.company EQ cocode
          AND b-oe-boll.bol-no EQ b-oe-bolh.bol-no 
          AND CAN-FIND (b-vend-whse-item 
                         WHERE  b-vend-whse-item.company     = b-oe-boll.company
                           AND b-vend-whse-item.fg-item-no  = b-oe-boll.i-no
                       ) NO-ERROR.      
      
    /* gdm - 11160904 */       

      FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company     = b-oe-boll.company
                                    AND b-vend-whse-item.fg-item-no  = b-oe-boll.i-no NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(b-vend-whse-item) THEN DO:
         MESSAGE 'FG Item Number "' b-oe-boll.i-no '" for BOL Number "' b-oe-boll.bol-no '" does not exist in the Vendor Management Item File' 
         VIEW-AS ALERT-BOX ERROR.
         APPLY "ENTRY" TO vend-whse-trans.vend-bol-no IN BROWSE {&browse-name}.
         RETURN ERROR.
      END.
      ELSE
         IF adm-adding-record THEN
            RUN fill-bol.
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val-vend-plant-dept-code B-table-Win 
PROCEDURE val-vend-plant-dept-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

   IF NOT CAN-FIND(FIRST vend-plant WHERE
      vend-plant.company = cocode AND
      vend-plant.vendor-code = vend-whse-trans.vendor-code:SCREEN-VALUE IN BROWSE {&browse-name} AND
      vend-plant.plant-id = vend-whse-trans.vendor-plant-code:SCREEN-VALUE IN BROWSE {&browse-name} AND
      vend-plant.vendor-dept-code = vend-whse-trans.vendor-dept-code:SCREEN-VALUE IN BROWSE {&browse-name}) THEN DO:
      MESSAGE "Invalid Customers Plant Dept Code." 
         VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO vend-whse-trans.vendor-dept-code IN BROWSE {&browse-name}.
      RETURN ERROR.
   END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

