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

DEF VAR ld-cost       AS DECI NO-UNDO.
DEF VAR lv-uom        AS CHAR NO-UNDO.
DEF VAR ls-prev-po    AS CHAR NO-UNDO.
DEF VAR hd-post       AS WIDGET-HANDLE NO-UNDO.
DEF VAR hd-post-child AS WIDGET-HANDLE NO-UNDO.
DEF VAR ll-help-run   AS LOGICAL NO-UNDO. /* set on browse help, reset row-entry */
DEF VAR v-lcuspo2     AS LOGICAL NO-UNDO.

ASSIGN
   cocode = g_company.

&SCOPED-DEFINE item-key-phrase TRUE

DEF TEMP-TABLE tt-vend-whse-trans LIKE vend-whse-trans
   FIELD valid    AS LOGICAL INIT TRUE
   FIELD row-no   AS INTEGER.

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
vend-whse-trans.trans-date vend-whse-trans.trans-qty ~
vend-whse-trans.plant-tot-oh-qty vend-whse-trans.item-po-no ~
vend-whse-trans.cust-part-no vend-whse-trans.fg-item-no ~
vend-whse-trans.vendor-code vend-whse-trans.vendor-plant-code ~
vend-whse-trans.vendor-dept-code vend-whse-trans.vend-ord-no ~
vend-whse-trans.item-line-no vend-whse-trans.vend-job-no ~
vend-whse-trans.vend-job-no2 vend-whse-trans.sell-price 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table ~
vend-whse-trans.trans-date vend-whse-trans.trans-qty ~
vend-whse-trans.item-po-no vend-whse-trans.cust-part-no ~
vend-whse-trans.vendor-plant-code vend-whse-trans.vend-ord-no ~
vend-whse-trans.vend-job-no vend-whse-trans.vend-job-no2 ~
vend-whse-trans.sell-price 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table vend-whse-trans
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table vend-whse-trans
&Scoped-define QUERY-STRING-Browser-Table FOR EACH vend-whse-trans WHERE ~{&KEY-PHRASE} ~
      AND vend-whse-trans.trans-type = "U" SHARE-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH vend-whse-trans WHERE ~{&KEY-PHRASE} ~
      AND vend-whse-trans.trans-type = "U" SHARE-LOCK ~
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
     SIZE 115 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42.2 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 91 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 144 BY 2.62.

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
      vend-whse-trans.trans-date COLUMN-LABEL "Customers!Usage Date" FORMAT "99/99/99":U
            WIDTH 15.8 LABEL-BGCOLOR 14
      vend-whse-trans.trans-qty COLUMN-LABEL "Quantity!Used" FORMAT "->>,>>>,>>9":U
            WIDTH 13.2 LABEL-BGCOLOR 14
      vend-whse-trans.plant-tot-oh-qty FORMAT "->>,>>>,>>9":U WIDTH 15.6
            LABEL-BGCOLOR 14
      vend-whse-trans.item-po-no FORMAT "x(15)":U WIDTH 14.8 LABEL-BGCOLOR 14
      vend-whse-trans.cust-part-no FORMAT "x(20)":U WIDTH 14 LABEL-BGCOLOR 14
      vend-whse-trans.fg-item-no FORMAT "x(20)":U LABEL-BGCOLOR 14
      vend-whse-trans.vendor-code FORMAT "x(8)":U WIDTH 12.6 LABEL-BGCOLOR 14
      vend-whse-trans.vendor-plant-code FORMAT "x(8)":U WIDTH 13.2
            LABEL-BGCOLOR 14
      vend-whse-trans.vendor-dept-code FORMAT "x(8)":U WIDTH 13.2
            LABEL-BGCOLOR 14
      vend-whse-trans.vend-ord-no FORMAT ">>>>>9":U WIDTH 12.6
            LABEL-BGCOLOR 14
      vend-whse-trans.item-line-no FORMAT "99":U WIDTH 7.2
      vend-whse-trans.vend-job-no COLUMN-LABEL "Suppliers!Job#" FORMAT "x(6)":U
            WIDTH 13.2 LABEL-BGCOLOR 14
      vend-whse-trans.vend-job-no2 FORMAT ">9":U
      vend-whse-trans.sell-price FORMAT ">,>>>,>>9.99<<<<":U WIDTH 20
            LABEL-BGCOLOR 14
  ENABLE
      vend-whse-trans.trans-date HELP "Date Boxes Were Consummed"
      vend-whse-trans.trans-qty HELP "Quantity Consummed"
      vend-whse-trans.item-po-no HELP "Purchase Order Number for Customers Part"
      vend-whse-trans.cust-part-no HELP "Customers Part Number"
      vend-whse-trans.vendor-plant-code
      vend-whse-trans.vend-ord-no HELP "Suppliers Order Number for Customers Purchase Order"
      vend-whse-trans.vend-job-no HELP "Suppliers Job Number for Customers Purchase Order"
      vend-whse-trans.vend-job-no2
      vend-whse-trans.sell-price HELP "Cost of Customers Part Number On PO"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 144 BY 15.24
         FONT 2 ROW-HEIGHT-CHARS .67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 16.48 COL 8 HELP
          "Select Browser Sort Order" NO-LABEL
     fi_sortby AT ROW 16.48 COL 98 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     auto_find AT ROW 17.67 COL 12 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 17.67 COL 129.4 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 16.38 COL 3
     RECT-4 AT ROW 16.24 COL 1
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
         HEIGHT             = 17.91
         WIDTH              = 144.8.
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
     _Where[1]         = "vend-whse-trans.trans-type = ""U"""
     _FldNameList[1]   > vend-whse-trans.r-no
"vend-whse-trans.r-no" "Seq#" ? "integer" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > vend-whse-trans.trans-date
"vend-whse-trans.trans-date" "Customers!Usage Date" ? "date" ? ? ? 14 ? ? yes "Date Boxes Were Consummed" no no "15.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > vend-whse-trans.trans-qty
"vend-whse-trans.trans-qty" "Quantity!Used" "->>,>>>,>>9" "decimal" ? ? ? 14 ? ? yes "Quantity Consummed" no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > vend-whse-trans.plant-tot-oh-qty
"vend-whse-trans.plant-tot-oh-qty" ? "->>,>>>,>>9" "decimal" ? ? ? 14 ? ? no "On Hand Quantity at Customers Plant" no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > vend-whse-trans.item-po-no
"vend-whse-trans.item-po-no" ? "x(15)" "character" ? ? ? 14 ? ? yes "Purchase Order Number for Customers Part" no no "14.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > vend-whse-trans.cust-part-no
"vend-whse-trans.cust-part-no" ? "x(20)" "character" ? ? ? 14 ? ? yes "Customers Part Number" no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > vend-whse-trans.fg-item-no
"vend-whse-trans.fg-item-no" ? "x(20)" "character" ? ? ? 14 ? ? no "Suppliers Finished Goods Part Number" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > vend-whse-trans.vendor-code
"vend-whse-trans.vendor-code" ? ? "character" ? ? ? 14 ? ? no "A/P Code for Supplier in Customers Vendors File" no no "12.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > vend-whse-trans.vendor-plant-code
"vend-whse-trans.vendor-plant-code" ? ? "character" ? ? ? 14 ? ? yes ? no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > vend-whse-trans.vendor-dept-code
"vend-whse-trans.vendor-dept-code" ? ? "character" ? ? ? 14 ? ? no ? no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > vend-whse-trans.vend-ord-no
"vend-whse-trans.vend-ord-no" ? ? "integer" ? ? ? 14 ? ? yes "Suppliers Order Number for Customers Purchase Order" no no "12.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > vend-whse-trans.item-line-no
"vend-whse-trans.item-line-no" ? ? "integer" ? ? ? ? ? ? no "Line Number for Customers Part" no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > vend-whse-trans.vend-job-no
"vend-whse-trans.vend-job-no" "Suppliers!Job#" ? "character" ? ? ? 14 ? ? yes "Suppliers Job Number for Customers Purchase Order" no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > vend-whse-trans.vend-job-no2
"vend-whse-trans.vend-job-no2" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > vend-whse-trans.sell-price
"vend-whse-trans.sell-price" ? ? "decimal" ? ? ? 14 ? ? yes "Cost of Customers Part Number On PO" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
   DEF VAR lv-handle    AS WIDGET-HANDLE NO-UNDO.
   DEF VAR lv-rowids    AS CHAR NO-UNDO.
   DEF VAR v-char-val   AS CHAR NO-UNDO.
   DEF VAR li           AS INTEGER NO-UNDO.
   
   DEF BUFFER b-oe-ordl FOR oe-ordl.
   DEF BUFFER b-vend-whse-item FOR vend-whse-item.
   DEF BUFFER b-vend-whse-trans FOR vend-whse-trans.

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
      WHEN "cust-part-no" THEN DO:
         RUN custitem/l-vwitem.w (cocode,FOCUS:SCREEN-VALUE, OUTPUT v-char-val).
         IF v-char-val <> "" THEN DO:
            ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,v-char-val).
            FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company = cocode 
                                          AND b-vend-whse-item.cust-part-no      = ENTRY(1,v-char-val) 
                                          AND b-vend-whse-item.fg-item-no        = ENTRY(2,v-char-val) 
                                          AND b-vend-whse-item.vendor-plant-code = ENTRY(3,v-char-val) 
                                          AND b-vend-whse-item.vendor-dept-code  = ENTRY(4,v-char-val) NO-LOCK NO-ERROR.
            IF AVAILABLE(b-vend-whse-item) THEN DO:
               ASSIGN 
                  vend-whse-trans.fg-item-no:SCREEN-VALUE IN BROWSE {&browse-name} = b-vend-whse-item.fg-item-no
                  vend-whse-trans.vendor-plant-code:SCREEN-VALUE IN BROWSE {&browse-name} = b-vend-whse-item.vendor-plant-code
                  vend-whse-trans.vendor-dept-code:SCREEN-VALUE IN BROWSE {&browse-name} = b-vend-whse-item.vendor-dept-code
                  vend-whse-trans.vendor-code:SCREEN-VALUE IN BROWSE {&browse-name} = b-vend-whse-item.vendor-code.
            END.
         END.
         RETURN NO-APPLY.
      END.
      WHEN "item-po-no" then do:
         RUN windows/l-cuspo2.w (INPUT cocode,
                                 INPUT FOCUS:SCREEN-VALUE, 
                                 OUTPUT v-char-val).
         IF v-char-val <> "" THEN DO:
            RUN fill-from-po-no(INPUT v-char-val).
         END.
         RETURN NO-APPLY.
      END.
      WHEN "vendor-plant-code" THEN DO:
         RUN custitem/l-pltid2.w (cocode,vend-whse-trans.vendor-code:SCREEN-VALUE IN BROWSE {&browse-name},vend-whse-trans.fg-item-no:SCREEN-VALUE IN BROWSE {&browse-name}, OUTPUT v-char-val).
         IF v-char-val <> "" THEN DO:
            ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,v-char-val).
            ASSIGN vend-whse-trans.vendor-dept-code:SCREEN-VALUE = ENTRY(2,v-char-val). 
         END.
         RETURN NO-APPLY.
      END.
      WHEN "vend-ord-no" THEN DO:
         RUN custitem/l-itmord.w(cocode, FOCUS:SCREEN-VALUE,vend-whse-trans.fg-item-no:SCREEN-VALUE IN BROWSE {&browse-name}, OUTPUT v-char-val).
         IF v-char-val <> "" THEN DO:
            ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,v-char-val).
            ASSIGN vend-whse-trans.item-line-no:SCREEN-VALUE = ENTRY(2,v-char-val). 
            
            FIND FIRST b-oe-ordl WHERE b-oe-ordl.company  = cocode
                                   AND b-oe-ordl.i-no     = vend-whse-trans.fg-item-no:SCREEN-VALUE IN BROWSE {&browse-name}
                                   AND b-oe-ordl.ord-no   = INT(vend-whse-trans.vend-ord-no:SCREEN-VALUE IN BROWSE {&browse-name})
                                   AND b-oe-ordl.line     = INT(vend-whse-trans.item-line-no:SCREEN-VALUE IN BROWSE {&browse-name}) NO-LOCK NO-ERROR.
               IF AVAILABLE(b-oe-ordl) THEN DO:
                  ASSIGN
                     vend-whse-trans.vend-job-no:SCREEN-VALUE IN BROWSE {&browse-name} = b-oe-ordl.job-no  
                     vend-whse-trans.vend-job-no2:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(b-oe-ordl.job-no2) 
                     vend-whse-trans.sell-price:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(b-oe-ordl.t-price).    
               END.
         END.
         RETURN NO-APPLY.
      END.
/*       WHEN "vendor-code" THEN DO:                                                 */
/*          RUN custitem/l-vwxref.w (cocode, FOCUS:SCREEN-VALUE, OUTPUT v-char-val). */
/*          IF v-char-val <> "" THEN DO:                                             */
/*             ASSIGN                                                                */
/*                FOCUS:SCREEN-VALUE = ENTRY(2,v-char-val).                          */
/*          END.                                                                     */
/*          RETURN NO-APPLY.                                                         */
/*       END.                                                                        */
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


&Scoped-define SELF-NAME vend-whse-trans.trans-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-whse-trans.trans-date Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF vend-whse-trans.trans-date IN BROWSE Browser-Table /* Customers!Usage Date */
DO:
   IF LASTKEY NE -1 THEN DO:
      RUN val-usage-date NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend-whse-trans.trans-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-whse-trans.trans-qty Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF vend-whse-trans.trans-qty IN BROWSE Browser-Table /* Quantity!Used */
DO:
    IF LASTKEY NE -1 THEN DO:
      RUN val-usage-qty NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend-whse-trans.item-po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-whse-trans.item-po-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF vend-whse-trans.item-po-no IN BROWSE Browser-Table /* Customers!PO# */
DO:
   IF LASTKEY NE -1 THEN DO:
      RUN val-item-po-no NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
         RETURN NO-APPLY.
      ELSE DO:
         RUN check-no-of-lines-on-po.
      END.

   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend-whse-trans.cust-part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-whse-trans.cust-part-no Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF vend-whse-trans.cust-part-no IN BROWSE Browser-Table /* Customers!Part# */
DO:
       IF vend-whse-trans.item-po-no:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN DO:
         APPLY "TAB" TO SELF.
         RETURN NO-APPLY.
      END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-whse-trans.cust-part-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF vend-whse-trans.cust-part-no IN BROWSE Browser-Table /* Customers!Part# */
DO:
     IF LASTKEY NE -1 THEN DO:
      RUN val-cust-part-no NO-ERROR.
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


&Scoped-define SELF-NAME vend-whse-trans.vendor-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-whse-trans.vendor-code Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF vend-whse-trans.vendor-code IN BROWSE Browser-Table /* Customers!A/P Code */
DO:
   IF LASTKEY NE -1 THEN DO:
      RUN val-vendor-code NO-ERROR.
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
      RUN val-vendor-plant-code NO-ERROR.
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
      RUN val-vendor-dept-code NO-ERROR.
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
      IF ERROR-STATUS:ERROR THEN 
         RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend-whse-trans.item-line-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-whse-trans.item-line-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF vend-whse-trans.item-line-no IN BROWSE Browser-Table /* Line# */
DO:
   IF LASTKEY NE -1 THEN DO:
      RUN val-item-line-no NO-ERROR.
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


&Scoped-define SELF-NAME vend-whse-trans.sell-price
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-whse-trans.sell-price Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF vend-whse-trans.sell-price IN BROWSE Browser-Table /* Suppliers Item!Sell Price */
DO:
/*     IF LASTKEY NE -1 THEN DO:                     */
/*       RUN val-sell-price NO-ERROR.                */
/*       IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY. */
/*    END.                                           */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-no-of-lines-on-po B-table-Win 
PROCEDURE check-no-of-lines-on-po :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-oe-rel FOR oe-rel.
DEF BUFFER b-vend-whse-item FOR vend-whse-item.

DEF VAR v-cnt      AS INTEGER NO-UNDO.
DEF VAR v-char-val AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
   IF vend-whse-trans.item-po-no:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN DO:
      FOR EACH b-oe-rel NO-LOCK WHERE b-oe-rel.company = cocode
                                  AND (b-oe-rel.stat    = "C" OR b-oe-rel.stat    = "Z")
                                  AND b-oe-rel.po-no   = vend-whse-trans.item-po-no:SCREEN-VALUE IN BROWSE {&browse-name}:
         FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company = cocode
                                       AND b-vend-whse-item.cust-no = b-oe-rel.cust-no
                                       AND b-vend-whse-item.fg-item = b-oe-rel.i-no NO-LOCK NO-ERROR.
         IF NOT AVAILABLE(b-vend-whse-item) THEN
            FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company = cocode
                                          AND b-vend-whse-item.fg-item = b-oe-rel.i-no NO-LOCK NO-ERROR.
         IF AVAILABLE(b-vend-whse-item) THEN
            v-cnt = v-cnt + 1.
      END.
      
      IF v-cnt > 1 THEN DO:
         RUN windows/l-cuspo2.w (INPUT cocode,
                                 INPUT vend-whse-trans.item-po-no:SCREEN-VALUE IN BROWSE {&browse-name}, 
                                 OUTPUT v-char-val).
         IF v-char-val <> "" THEN
            RUN fill-from-po-no(INPUT v-char-val).
      END.
      ELSE DO:
         FOR EACH b-oe-rel NO-LOCK WHERE b-oe-rel.company = cocode
                                     AND (b-oe-rel.stat    = "C" OR b-oe-rel.stat    = "Z")
                                     AND b-oe-rel.po-no   = vend-whse-trans.item-po-no:SCREEN-VALUE IN BROWSE {&browse-name}:
            FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company = cocode
                                          AND b-vend-whse-item.cust-no = b-oe-rel.cust-no
                                          AND b-vend-whse-item.fg-item = b-oe-rel.i-no NO-LOCK NO-ERROR.
            IF NOT AVAILABLE(b-vend-whse-item) THEN
               FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company = cocode
                                             AND b-vend-whse-item.fg-item = b-oe-rel.i-no NO-LOCK NO-ERROR.
            IF AVAILABLE(b-vend-whse-item) THEN DO:
               v-char-val = b-oe-rel.po-no 
                          + "," + b-oe-rel.i-no 
                          + "," + STRING(b-oe-rel.ord-no) 
                          + "," + STRING(b-oe-rel.line).
               RUN fill-from-po-no(INPUT v-char-val).
            END.
         END.
      END.
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-vend-whse-trans B-table-Win 
PROCEDURE create-vend-whse-trans :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* DEF VAR v-r-no LIKE vend-whse-trans.r-no NO-UNDO.                                                                                              */
/*                                                                                                                                                */
/* DEF BUFFER b-vend-whse-trans FOR vend-whse-trans.                                                                                              */
/* DEF BUFFER b-vend-whse-trans-hist FOR vend-whse-trans-hist.                                                                                    */
/* DEF BUFFER b-vend-whse-item FOR vend-whse-item.                                                                                                */
/* DEF BUFFER b-oe-ordl FOR oe-ordl.                                                                                                              */
/* DEF VAR v-rowid AS ROWID NO-UNDO.                                                                                                              */
/*                                                                                                                                                */
/*                                                                                                                                                */
/* DO WITH FRAME {&FRAME-NAME}:                                                                                                                   */
/*                                                                                                                                                */
/*    FOR EACH b-oe-ordl NO-LOCK WHERE b-oe-ordl.company = cocode                                                                                 */
/*                                 AND b-oe-ordl.po-no   = vend-whse-trans.item-po-no:SCREEN-VALUE IN BROWSE {&browse-name}:                      */
/*       FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company     = b-oe-ordl.company                                                       */
/*                                     AND b-vend-whse-item.cust-no     = b-oe-ordl.cust-no                                                       */
/*                                     AND b-vend-whse-item.fg-item-no  = b-oe-ordl.i-no NO-LOCK NO-ERROR.                                        */
/*       IF NOT AVAILABLE(b-vend-whse-item) THEN DO:                                                                                              */
/*          FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company     = b-oe-ordl.company                                                    */
/*                                        AND b-vend-whse-item.fg-item-no  = b-oe-ordl.i-no NO-LOCK NO-ERROR.                                     */
/*       END.                                                                                                                                     */
/*       IF AVAILABLE(b-vend-whse-item) THEN DO:                                                                                                  */
/*          FIND FIRST tt-oe-ordl WHERE tt-oe-ordl.company = b-oe-ordl.company                                                                    */
/*                                  AND tt-oe-ordl.po-no   = b-oe-ordl.po-no                                                                      */
/*                                  AND tt-oe-ordl.i-no    = b-oe-ordl.i-no NO-ERROR.                                                             */
/*          IF NOT AVAILABLE(tt-oe-ordl) THEN DO:                                                                                                 */
/*             CREATE tt-oe-ordl.                                                                                                                 */
/*             BUFFER-COPY b-oe-ordl EXCEPT b-oe-ordl.qty TO tt-oe-ordl NO-ERROR.                                                                 */
/*          END.                                                                                                                                  */
/*          tt-oe-ordl.qty = tt-oe-ordl.qty + b-oe-ordl.qty.                                                                                      */
/*       END.                                                                                                                                     */
/*    END.                                                                                                                                        */
/*                                                                                                                                                */
/*    v-r-no = vend-whse-trans.r-no.                                                                                                              */
/*                                                                                                                                                */
/*    RUN dispatch('cancel-records').                                                                                                             */
/*    RUN dispatch('end-update').                                                                                                                 */
/*                                                                                                                                                */
/*    FIND FIRST b-vend-whse-trans WHERE b-vend-whse-trans.company = cocode                                                                       */
/*                                   AND b-vend-whse-trans.r-no    = v-r-no NO-ERROR.                                                             */
/*    IF AVAILABLE(b-vend-whse-trans) THEN                                                                                                        */
/*       DELETE b-vend-whse-trans.                                                                                                                */
/*                                                                                                                                                */
/*    FOR EACH tt-oe-ordl:                                                                                                                        */
/*          v-r-no = 0.                                                                                                                           */
/*          FIND LAST b-vend-whse-trans USE-INDEX r-no NO-LOCK NO-ERROR.                                                                          */
/*          IF AVAIL b-vend-whse-trans AND b-vend-whse-trans.r-no > v-r-no THEN v-r-no = b-vend-whse-trans.r-no.                                  */
/*                                                                                                                                                */
/*          FIND LAST b-vend-whse-trans-hist USE-INDEX r-no NO-LOCK NO-ERROR.                                                                     */
/*          IF AVAIL b-vend-whse-trans-hist AND b-vend-whse-trans-hist.r-no GT v-r-no THEN v-r-no = b-vend-whse-trans-hist.r-no.                  */
/*                                                                                                                                                */
/*                                                                                                                                                */
/*          DO WHILE TRUE:                                                                                                                        */
/*             v-r-no = v-r-no + 1.                                                                                                               */
/*                                                                                                                                                */
/*             FIND FIRST b-vend-whse-trans-hist WHERE b-vend-whse-trans-hist.r-no = v-r-no USE-INDEX r-no NO-LOCK NO-ERROR.                      */
/*             IF AVAIL b-vend-whse-trans-hist THEN NEXT.                                                                                         */
/*                                                                                                                                                */
/*             FIND FIRST b-vend-whse-trans WHERE b-vend-whse-trans.r-no = v-r-no USE-INDEX r-no NO-LOCK NO-ERROR.                                */
/*             IF AVAIL b-vend-whse-trans THEN NEXT.                                                                                              */
/*                                                                                                                                                */
/*             LEAVE.                                                                                                                             */
/*          END.                                                                                                                                  */
/*                                                                                                                                                */
/*          CREATE vend-whse-trans.                                                                                                               */
/*          ASSIGN                                                                                                                                */
/*             vend-whse-trans.company       = tt-oe-ordl.company                                                                                 */
/*             vend-whse-trans.r-no          = v-r-no                                                                                             */
/*             vend-whse-trans.trans-type    = "U"                                                                                                */
/*             vend-whse-trans.trans-date    = TODAY                                                                                              */
/*             vend-whse-trans.create-date   = TODAY                                                                                              */
/*             vend-whse-trans.create-time   = TIME                                                                                               */
/*             vend-whse-trans.create-userid = USERID("nosweat")                                                                                  */
/*             vend-whse-trans.rec_key       = STRING(YEAR(TODAY), "9999") + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99") + STRING(TIME) */
/*             vend-whse-trans.upd-date      = TODAY                                                                                              */
/*             vend-whse-trans.upd-time      = TIME                                                                                               */
/*             vend-whse-trans.upd-userid    = USERID("nosweat")                                                                                  */
/*             vend-whse-trans.cust-no       = CAPS(tt-oe-ordl.cust-no)                                                                           */
/*             vend-whse-trans.fg-item-no    = CAPS(tt-oe-ordl.i-no)                                                                              */
/*             vend-whse-trans.item-line-no  = tt-oe-ordl.line                                                                                    */
/*             vend-whse-trans.item-po-no    = tt-oe-ordl.po-no                                                                                   */
/* /*             vend-whse-trans.trans-qty     = tt-oe-ordl.qty */                                                                               */
/*             vend-whse-trans.vend-job-no   = tt-oe-ordl.job-no                                                                                  */
/*             vend-whse-trans.vend-job-no2  = tt-oe-ordl.job-no2                                                                                 */
/*             vend-whse-trans.vend-ord-no   = tt-oe-ordl.ord-no                                                                                  */
/*             vend-whse-trans.sell-price    = tt-oe-ordl.price                                                                                   */
/*             vend-whse-trans.cust-part-no  = CAPS(tt-oe-ordl.part-no).                                                                          */
/*                                                                                                                                                */
/*       FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company     = tt-oe-ordl.company                                                      */
/*                                     AND b-vend-whse-item.cust-no     = tt-oe-ordl.cust-no                                                      */
/*                                     AND b-vend-whse-item.fg-item-no  = tt-oe-ordl.i-no NO-LOCK NO-ERROR.                                       */
/*       IF NOT AVAILABLE(b-vend-whse-item) THEN DO:                                                                                              */
/*          FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company     = tt-oe-ordl.company                                                   */
/*                                        AND b-vend-whse-item.fg-item-no  = tt-oe-ordl.i-no NO-LOCK NO-ERROR.                                    */
/*       END.                                                                                                                                     */
/*       IF AVAILABLE(b-vend-whse-item) THEN DO:                                                                                                  */
/*          ASSIGN                                                                                                                                */
/*             vend-whse-trans.vendor-code        = CAPS(b-vend-whse-item.vendor-code)                                                            */
/*             vend-whse-trans.vendor-dept-code   = CAPS(b-vend-whse-item.vendor-dept-code)                                                       */
/*             vend-whse-trans.vendor-plant-code  = CAPS(b-vend-whse-item.vendor-plant-code)                                                      */
/*             vend-whse-trans.plant-tot-oh-qty   = b-vend-whse-item.plant-tot-oh-qty - vend-whse-trans.trans-qty.                                */
/*       END.                                                                                                                                     */
/*       v-rowid = ROWID(vend-whse-trans).                                                                                                        */
/*                                                                                                                                                */
/*                                                                                                                                                */
/* /*          op-rowid-list = op-rowid-list + STRING(ROWID(vend-whse-trans)) + ",". */                                                           */
/*    END.                                                                                                                                        */
/* END.                                                                                                                                           */
/* {&CLOSE-QUERY-{&BROWSE-NAME}}                                                                                                                  */
/* {&OPEN-QUERY-{&BROWSE-NAME}}                                                                                                                   */
/* RUN repo-query(INPUT v-rowid).                                                                                                                 */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cust-no-xref B-table-Win 
PROCEDURE cust-no-xref :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-vend-code-cust-xref FOR vend-code-cust-xref.
DEF BUFFER b-vend-plant FOR vend-plant.

DO WITH FRAME {&FRAME-NAME}:
   IF vend-whse-trans.vendor-code:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN DO:
      FIND FIRST b-vend-code-cust-xref WHERE b-vend-code-cust-xref.company = cocode
                                         AND b-vend-code-cust-xref.vendor-code = vend-whse-trans.vendor-code:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
      IF AVAILABLE(b-vend-code-cust-xref) THEN DO:
         FIND FIRST b-vend-plant WHERE b-vend-plant.company = cocode
                                   AND b-vend-plant.cust-no = b-vend-code-cust-xref.cust-no NO-LOCK NO-ERROR.
         IF AVAILABLE(b-vend-plant) THEN DO:
            ASSIGN 
               vend-whse-trans.vendor-plant-code:SCREEN-VALUE IN BROWSE {&browse-name} = b-vend-plant.plant-id.
         END.
      END.
   END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fill-from-po-no B-table-Win 
PROCEDURE fill-from-po-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ip-char-val AS CHAR NO-UNDO.

DEF BUFFER b-oe-rel FOR oe-rel.
DEF BUFFER b-vend-whse-item FOR vend-whse-item.
DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF VAR v-cnt AS INT.

DO WITH FRAME {&FRAME-NAME}:
   FIND FIRST b-oe-rel WHERE b-oe-rel.company = cocode
                         AND (b-oe-rel.stat    = "C" OR b-oe-rel.stat    = "Z")
                         AND b-oe-rel.po-no   = ENTRY(1,ip-char-val)
                         AND b-oe-rel.i-no    = ENTRY(2,ip-char-val) 
                         AND b-oe-rel.ord-no  = INT(ENTRY(3,ip-char-val))
                         AND b-oe-rel.LINE    = INT(ENTRY(4,ip-char-val)) NO-LOCK NO-ERROR.
   FIND FIRST b-oe-ordl WHERE b-oe-ordl.company = b-oe-rel.company
                          AND b-oe-ordl.ord-no  = b-oe-rel.ord-no
                          AND b-oe-ordl.LINE    = b-oe-rel.LINE
                          AND b-oe-ordl.i-no    = b-oe-rel.i-no NO-LOCK NO-ERROR.
   IF AVAILABLE(b-oe-ordl) THEN DO:
      FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company = cocode
                                    AND b-vend-whse-item.cust-no = b-oe-rel.cust-no
                                    AND b-vend-whse-item.fg-item = b-oe-rel.i-no NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(b-vend-whse-item) THEN
         FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company = cocode
                                       AND b-vend-whse-item.fg-item = b-oe-rel.i-no NO-LOCK NO-ERROR.
      IF AVAILABLE(b-vend-whse-item) THEN DO:
         ASSIGN
            vend-whse-trans.item-po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}   = b-oe-rel.po-no
            vend-whse-trans.fg-item-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}   = b-oe-rel.i-no
            vend-whse-trans.vend-ord-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}  = STRING(b-oe-rel.ord-no)
            vend-whse-trans.item-line-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(b-oe-rel.line)
            vend-whse-trans.cust-part-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = b-vend-whse-item.cust-part-no
            vend-whse-trans.vendor-code:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}  = b-vend-whse-item.vendor-code
            vend-whse-trans.vendor-plant-code:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = b-vend-whse-item.vendor-plant-code
            vend-whse-trans.vendor-dept-code:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}  = b-vend-whse-item.vendor-dept-code
            vend-whse-trans.vend-job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}  = b-oe-ordl.job-no
            vend-whse-trans.vend-job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(b-oe-ordl.job-no2)
            vend-whse-trans.item-line-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(b-oe-rel.line)
            vend-whse-trans.sell-price:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}   = STRING(b-oe-ordl.t-price).
      END.
   END.
END.
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
   DEF BUFFER b-vend-whse-trans FOR vend-whse-trans.
   DEF BUFFER b-vend-whse-item  FOR vend-whse-item.
   DEF BUFFER b-itemfg          FOR itemfg.
   DEF BUFFER b-vend-code-cust-xref FOR vend-code-cust-xref.

   DEF VAR v-r-no       LIKE vend-whse-trans.r-no NO-UNDO.
   DEF VAR v-answer     AS LOG         NO-UNDO.
   DEF VAR chFile       AS CHAR        NO-UNDO.
   DEF VAR v-ok         AS LOG         NO-UNDO.
   DEF VAR chExcelAppl  AS COM-HANDLE  NO-UNDO.
   DEF VAR chWorkBook   AS COM-HANDLE  NO-UNDO.
   DEF VAR chWorksheet  AS COM-HANDLE  NO-UNDO.
   DEF VAR v-RowCount   AS INT INIT 2  NO-UNDO.
   DEF VAR v-valid-flag AS LOG INIT YES NO-UNDO.
   DEF VAR char-hdl     AS CHAR        NO-UNDO.
   DEF VAR v-rowid      AS ROWID       NO-UNDO.
   DEF VAR v-id         AS CHAR        NO-UNDO.
   DEF VAR v-deci-at    AS INTEGER     NO-UNDO.

   DEF VAR v-file       AS CHAR INIT "c:\tmp\vwexcimp-error.txt".

   OUTPUT TO VALUE(v-file).

   FOR EACH tt-vend-whse-trans:
      DELETE tt-vend-whse-trans.
   END.
   
   DO WITH FRAME {&FRAME-NAME}:
   
      SYSTEM-DIALOG GET-FILE chFile 
                    TITLE "Select File to Import"
                    FILTERS "Excel File (*.xls,*.xlsx) " "*.xls,*.xlsx"
                    INITIAL-DIR "c:\"
                    MUST-EXIST
                    USE-FILENAME
                    UPDATE v-ok.
     
      IF v-ok THEN DO:
         IF LENGTH(chFile) LT 4 OR
            (SUBSTR(chFile,LENGTH(chFile) - 3) NE ".xls" AND 
            SUBSTR(chFile,LENGTH(chFile) - 4) NE ".xlsx") THEN DO:
            MESSAGE "Invalid File.  Must Choose Excel (.xls) File."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            LEAVE.
         END.
     
         SESSION:SET-WAIT-STATE ("general").
   
         /* Initialize Excel. */
         CREATE "Excel.Application" chExcelAppl NO-ERROR.
     
         /* Check if Excel got initialized. */
         IF NOT (VALID-HANDLE (chExcelAppl)) THEN DO:
            MESSAGE "Unable to Start Excel." VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR. 
         END.
     
         /* Open our Excel File. */  
         chExcelAppl:VISIBLE = FALSE.
         chWorkbook = chExcelAppl:Workbooks:OPEN(chfile) NO-ERROR.
     
         /* Do not display Excel error messages. */
         chExcelAppl:DisplayAlerts = FALSE NO-ERROR.
     
         /* Go to the Active Sheet. */
         chWorkbook:WorkSheets(1):Activate NO-ERROR.
     
         ASSIGN
            chWorkSheet = chExcelAppl:Sheets:ITEM(1).

         REPEAT:
            IF chWorkSheet:Range("A" + STRING(v-RowCount)):VALUE EQ ? THEN LEAVE.
                
            CREATE tt-vend-whse-trans.
            ASSIGN
               tt-vend-whse-trans.company           = cocode
               tt-vend-whse-trans.vendor-code       = chWorkSheet:Range("A" + STRING(v-RowCount)):VALUE NO-ERROR.
               tt-vend-whse-trans.vendor-plant-code = chWorkSheet:Range("B" + STRING(v-RowCount)):VALUE NO-ERROR.
               tt-vend-whse-trans.vendor-dept-code  = chWorkSheet:Range("C" + STRING(v-RowCount)):VALUE NO-ERROR.
               tt-vend-whse-trans.cust-part-no      = chWorkSheet:Range("D" + STRING(v-RowCount)):VALUE NO-ERROR.
               tt-vend-whse-trans.revision          = chWorkSheet:Range("E" + STRING(v-RowCount)):VALUE NO-ERROR.
               tt-vend-whse-trans.est-annual-usage  = chWorkSheet:Range("G" + STRING(v-RowCount)):VALUE NO-ERROR.
               tt-vend-whse-trans.plant-tot-oh-qty  = chWorkSheet:Range("H" + STRING(v-RowCount)):VALUE NO-ERROR.
            ASSIGN
               tt-vend-whse-trans.row-no = v-RowCount
               v-RowCount = v-RowCount + 1.
         END.
      END.
      
      /*Free memory*/
      chWorkbook = chExcelAppl:Workbooks:CLOSE() NO-ERROR.
      RELEASE OBJECT chWorkbook NO-ERROR.
      RELEASE OBJECT chWorkSheet NO-ERROR.
      RELEASE OBJECT chExcelAppl NO-ERROR.

      FOR EACH tt-vend-whse-trans:

         v-deci-at = INDEX(tt-vend-whse-trans.vendor-code, ".0000000000").
         IF v-deci-at > 0 THEN
            tt-vend-whse-trans.vendor-code = SUBSTRING(tt-vend-whse-trans.vendor-code, 1, v-deci-at - 1). 

         v-deci-at = INDEX(tt-vend-whse-trans.vendor-plant-code, ".0000000000").
         IF v-deci-at > 0 THEN
            tt-vend-whse-trans.vendor-plant-code = SUBSTRING(tt-vend-whse-trans.vendor-plant-code, 1, v-deci-at - 1). 

         v-deci-at = INDEX(tt-vend-whse-trans.vendor-dept-code, ".0000000000").
         IF v-deci-at > 0 THEN
            tt-vend-whse-trans.vendor-dept-code = SUBSTRING(tt-vend-whse-trans.vendor-dept-code, 1, v-deci-at - 1). 

         v-deci-at = INDEX(tt-vend-whse-trans.fg-item-no, ".0000000000").
         IF v-deci-at > 0 THEN
            tt-vend-whse-trans.fg-item-no = SUBSTRING(tt-vend-whse-trans.fg-item-no, 1, v-deci-at - 1). 
        
         FIND FIRST b-vend-code-cust-xref WHERE b-vend-code-cust-xref.company     = tt-vend-whse-trans.company 
                                           AND b-vend-code-cust-xref.vendor-code  = tt-vend-whse-trans.vendor-code NO-LOCK NO-ERROR.

         IF NOT AVAILABLE(b-vend-code-cust-xref) THEN DO: 
            PUT UNFORMATTED "Invalid Customers A/P Code " + '"' + tt-vend-whse-trans.vendor-code + '"' + ", in row " + STRING(tt-vend-whse-trans.row-no) + "." SKIP.
            ASSIGN
               tt-vend-whse-trans.valid = FALSE
               v-valid-flag = FALSE.
         END.
         ELSE
            ASSIGN tt-vend-whse-trans.cust-no = b-vend-code-cust-xref.cust-no.

         IF NOT CAN-FIND(FIRST vend-plant WHERE vend-plant.company      = tt-vend-whse-trans.company
                                            AND vend-plant.vendor-code  = tt-vend-whse-trans.vendor-code
                                            AND vend-plant.plant-id     = tt-vend-whse-trans.vendor-plant-code) THEN DO:
            PUT UNFORMATTED "Invalid Customers Plant ID " + '"' + tt-vend-whse-trans.vendor-plant-code + '"' + ", in row " + STRING(tt-vend-whse-trans.row-no) + "." SKIP.
            ASSIGN
               tt-vend-whse-trans.valid = FALSE
               v-valid-flag = FALSE.
         END.

         IF NOT CAN-FIND(FIRST vend-plant WHERE vend-plant.company            = tt-vend-whse-trans.company
                                            AND vend-plant.vendor-code        = tt-vend-whse-trans.vendor-code
                                            AND vend-plant.plant-id           = tt-vend-whse-trans.vendor-plant-code
                                            AND vend-plant.vendor-dept-code   = tt-vend-whse-trans.vendor-dept-code) THEN DO:
            PUT UNFORMATTED "Invalid Customers Dept Code " + '"' + tt-vend-whse-trans.vendor-dept-code + '"' + ", in row " + STRING(tt-vend-whse-trans.row-no) + "." SKIP.
            ASSIGN
               tt-vend-whse-trans.valid = FALSE
               v-valid-flag = FALSE.
         END.
         
         IF NOT CAN-FIND(FIRST b-itemfg WHERE b-itemfg.company = tt-vend-whse-trans.company 
                                          AND b-itemfg.part-no = tt-vend-whse-trans.cust-part-no) THEN DO:

            PUT UNFORMATTED "Invalid Customers Part Number " + '"' + tt-vend-whse-trans.cust-part-no + '"' + ", in row " + STRING(tt-vend-whse-trans.row-no) + "." SKIP.
            ASSIGN
               tt-vend-whse-trans.valid = FALSE
               v-valid-flag = FALSE.
         END.
         
         IF NOT CAN-FIND(FIRST b-vend-whse-item WHERE b-vend-whse-item.company            = tt-vend-whse-trans.company
                                                  AND b-vend-whse-item.vendor-code        = tt-vend-whse-trans.vendor-code
                                                  AND b-vend-whse-item.vendor-plant-code  = tt-vend-whse-trans.vendor-plant-code
                                                  AND b-vend-whse-item.cust-part-no       = tt-vend-whse-trans.cust-part-no 
                                                  AND b-vend-whse-item.vendor-dept-code   = tt-vend-whse-trans.vendor-dept-code) THEN DO:

            PUT UNFORMATTED "Invalid Warehouse Management Customers Part Number " + '"' + tt-vend-whse-trans.cust-part-no + '"' + ", in row " + STRING(tt-vend-whse-trans.row-no) + "." SKIP.
            ASSIGN
               tt-vend-whse-trans.valid = FALSE
               v-valid-flag = FALSE.
         END.
      END.

      IF v-valid-flag = FALSE THEN DO:
         MESSAGE "The Excel file did not load, please review error file." SKIP
                 "c:\tmp\vw-xlsimp-error.txt"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
         
      END.
      ELSE DO:
         v-r-no = 0.
         FIND LAST b-vend-whse-trans USE-INDEX r-no NO-LOCK NO-ERROR.
         IF AVAIL b-vend-whse-trans AND b-vend-whse-trans.r-no > v-r-no THEN v-r-no = b-vend-whse-trans.r-no.

         FIND LAST vend-whse-trans-hist USE-INDEX r-no NO-LOCK NO-ERROR.
         IF AVAIL vend-whse-trans-hist AND vend-whse-trans-hist.r-no GT v-r-no THEN v-r-no = vend-whse-trans-hist.r-no.
         
         DO WHILE TRUE:
            v-r-no = v-r-no + 1.
   
            FIND FIRST vend-whse-trans-hist WHERE vend-whse-trans-hist.r-no = v-r-no USE-INDEX r-no NO-LOCK NO-ERROR.
            IF AVAIL vend-whse-trans-hist THEN NEXT.
   
            FIND FIRST b-vend-whse-trans WHERE b-vend-whse-trans.r-no = v-r-no USE-INDEX r-no NO-LOCK NO-ERROR.
            IF AVAIL b-vend-whse-trans THEN NEXT.
   
            LEAVE.
         END.
         
         FOR EACH tt-vend-whse-trans:
              
            FIND FIRST b-itemfg WHERE b-itemfg.company = tt-vend-whse-trans.company
                                  AND b-itemfg.cust-no = tt-vend-whse-trans.cust-no
                                  AND b-itemfg.part-no = tt-vend-whse-trans.cust-part-no NO-LOCK NO-ERROR.
            IF NOT AVAILABLE(b-itemfg) THEN
               FIND FIRST b-itemfg WHERE b-itemfg.company = tt-vend-whse-trans.company
                                     AND b-itemfg.part-no = tt-vend-whse-trans.cust-part-no NO-LOCK NO-ERROR.
            CREATE vend-whse-trans.
            ASSIGN
               vend-whse-trans.company           = tt-vend-whse-trans.company
               vend-whse-trans.r-no              = v-r-no
               vend-whse-trans.vendor-code       = tt-vend-whse-trans.vendor-code       
               vend-whse-trans.vendor-plant-code = tt-vend-whse-trans.vendor-plant-code
               vend-whse-trans.vendor-dept-code  = tt-vend-whse-trans.vendor-dept-code
               vend-whse-trans.fg-item-no        = b-itemfg.i-no
               vend-whse-trans.cust-part-no      = tt-vend-whse-trans.cust-part-no
               vend-whse-trans.est-annual-usage  = tt-vend-whse-trans.est-annual-usage               
               vend-whse-trans.plant-tot-oh-qty  = tt-vend-whse-trans.plant-tot-oh-qty                  
               vend-whse-trans.create-date       = TODAY
               vend-whse-trans.create-time       = TIME
               vend-whse-trans.create-userid     = USERID("nosweat")
               vend-whse-trans.cust-no           = tt-vend-whse-trans.cust-no
               vend-whse-trans.rec_key           = STRING(YEAR(TODAY)) + STRING(MONTH(TODAY)) + STRING(DAY(TODAY)) + STRING(TIME) 
               vend-whse-trans.trans-date        = TODAY
               vend-whse-trans.trans-type        = "X"                                  
               vend-whse-trans.upd-date          = TODAY 
               vend-whse-trans.upd-time          = TIME
               vend-whse-trans.upd-userid        = USERID("nosweat")
               vend-whse-trans.revision          = tt-vend-whse-trans.revision
               v-rowid = ROWID(vend-whse-trans)
               v-r-no = v-r-no + 1.

            FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company           = tt-vend-whse-trans.company
                                          AND b-vend-whse-item.vendor-code       = tt-vend-whse-trans.vendor-code
                                          AND b-vend-whse-item.vendor-plant-code = tt-vend-whse-trans.vendor-plant-code
                                          AND b-vend-whse-item.fg-item-no        = tt-vend-whse-trans.fg-item-no 
                                          AND b-vend-whse-item.vendor-dept-code  = tt-vend-whse-trans.vendor-dept-code NO-ERROR.
            ASSIGN
               b-vend-whse-item.est-annual-usage = tt-vend-whse-trans.est-annual-usage 
               b-vend-whse-item.plant-tot-oh-qty = tt-vend-whse-trans.plant-tot-oh-qty.
                        
            IF tt-vend-whse-trans.est-annual-usage = 0 AND b-vend-whse-item.obsolete-date <> ? THEN
               ASSIGN 
                  b-vend-whse-item.obsolete-date = TODAY
                  b-vend-whse-item.obsolete      = YES.
         END.

         MESSAGE "Excel File Import Completed." SKIP
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
      END.
   END.

   OUTPUT CLOSE.
  
   RUN dispatch ('open-query'). 

   
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
   DEF BUFFER b-vend-whse-item   FOR vend-whse-item.

   /* Code placed here will execute PRIOR to standard behavior. */

   /* Dispatch standard ADM method.                             */
   RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

   /* Code placed here will execute AFTER standard behavior.    */

   FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company = cocode 
                                 AND b-vend-whse-item.vendor-code = vend-whse-trans.vendor-code:SCREEN-VALUE IN BROWSE {&browse-name} 
                                 AND b-vend-whse-item.vendor-plant-code = vend-whse-trans.vendor-plant-code:SCREEN-VALUE IN BROWSE {&browse-name}
                                 AND b-vend-whse-item.fg-item-no = vend-whse-trans.fg-item-no:SCREEN-VALUE IN BROWSE {&browse-name}
                                 AND b-vend-whse-item.cust-part-no = vend-whse-trans.cust-part-no:SCREEN-VALUE IN BROWSE {&browse-name} 
                                 AND b-vend-whse-item.vendor-dept-code = vend-whse-trans.vendor-dept-code:SCREEN-VALUE IN BROWSE {&browse-name} NO-ERROR.
   IF AVAILABLE(b-vend-whse-item) THEN DO:
      ASSIGN
         vend-whse-trans.plant-tot-oh-qty = b-vend-whse-item.plant-tot-oh-qty - DECI(vend-whse-trans.trans-qty:SCREEN-VALUE IN BROWSE {&browse-name})
         vend-whse-trans.cust-no          = b-vend-whse-item.cust-no.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement B-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
      vend-whse-trans.upd-date     = TODAY
      vend-whse-trans.upd-time     = TIME
      vend-whse-trans.upd-userid   = USERID("nosweat").

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

IF v-lcuspo2 = YES THEN
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
   vend-whse-trans.company = cocode
   vend-whse-trans.r-no    = v-r-no
   vend-whse-trans.trans-type = "U".

IF adm-adding-record THEN DO:
   ASSIGN
      vend-whse-trans.trans-date      = TODAY
      vend-whse-trans.create-date     = TODAY
      vend-whse-trans.create-time     = TIME
      vend-whse-trans.create-userid   = USERID("nosweat")
      vend-whse-trans.rec_key         = STRING(YEAR(TODAY), "9999") + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99") + STRING(TIME).

   DISPLAY vend-whse-trans.trans-date vend-whse-trans.r-no WITH BROWSE {&browse-name}.

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

   /* Code placed here will execute AFTER standard behavior.    */

  DO WITH FRAME {&FRAME-NAME}:
    APPLY "entry" TO vend-whse-trans.trans-date IN BROWSE {&browse-name}.
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
   DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY 'cursor-left' TO {&BROWSE-NAME}.
    END.
  END.

   RUN val-usage-date NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN val-usage-qty NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN val-item-po-no NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN val-cust-part-no NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN val-fg-item-no NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN val-vendor-code NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN val-vendor-plant-code NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN val-vendor-dept-code NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN val-vend-ord-no NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN val-item-line-no NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN val-job-no NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN val-job-no-2 NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

/*    RUN val-sell-price NO-ERROR.                */
/*    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY. */

   RUN val-if-exists NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val-cust-part-no B-table-Win 
PROCEDURE val-cust-part-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-vend-whse-item FOR vend-whse-item.
DEF BUFFER b-itemfg FOR itemfg.


DO WITH FRAME {&FRAME-NAME}:
   IF NOT CAN-FIND(FIRST b-itemfg WHERE b-itemfg.company = cocode
                                    AND b-itemfg.part-no = TRIM(vend-whse-trans.cust-part-no:SCREEN-VALUE IN BROWSE {&browse-name})) THEN DO:
      MESSAGE "Invalid Customer Part Number     " VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO vend-whse-trans.cust-part-no IN BROWSE {&browse-name}.
      RETURN ERROR.
   END.
   ELSE DO:
      IF NOT CAN-FIND(FIRST b-vend-whse-item WHERE b-vend-whse-item.company = cocode
                                               AND TRIM(b-vend-whse-item.cust-part-no) = TRIM(vend-whse-trans.cust-part-no:SCREEN-VALUE IN BROWSE {&browse-name})) THEN DO:
         MESSAGE 'Customers Part Number "' vend-whse-trans.cust-part-no:SCREEN-VALUE IN BROWSE {&browse-name}  '" does not exist in the Vendor Management Item File.' SKIP
                 'Please add the Customers Part Number to the Vendor Management Item File.' 
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY "entry" TO vend-whse-trans.cust-part-no IN BROWSE {&browse-name}.
         RETURN ERROR.
      END.
      ELSE DO:
         FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company = cocode 
                                       AND b-vend-whse-item.cust-part-no      = TRIM(vend-whse-trans.cust-part-no:SCREEN-VALUE IN BROWSE {&browse-name}) NO-LOCK NO-ERROR.
            IF AVAILABLE(b-vend-whse-item) THEN DO:
               ASSIGN 
                  vend-whse-trans.fg-item-no:SCREEN-VALUE IN BROWSE {&browse-name} = b-vend-whse-item.fg-item-no
                  vend-whse-trans.vendor-plant-code:SCREEN-VALUE IN BROWSE {&browse-name} = b-vend-whse-item.vendor-plant-code
                  vend-whse-trans.vendor-dept-code:SCREEN-VALUE IN BROWSE {&browse-name} = b-vend-whse-item.vendor-dept-code
                  vend-whse-trans.vendor-code:SCREEN-VALUE IN BROWSE {&browse-name} = b-vend-whse-item.vendor-code.
            END.
      END.
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val-fg-item-no B-table-Win 
PROCEDURE val-fg-item-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-vend-whse-item FOR vend-whse-item.
DEF BUFFER b-itemfg FOR itemfg.

/* DO WITH FRAME {&FRAME-NAME}:                                                                                                                                         */
/*    IF NOT CAN-FIND(FIRST b-vend-whse-item WHERE b-vend-whse-item.company   = cocode                                                                                  */
/*                                             AND TRIM(b-vend-whse-item.fg-item-no) = TRIM(vend-whse-trans.fg-item-no:SCREEN-VALUE IN BROWSE {&browse-name})) THEN DO: */
/*       MESSAGE "Invalid FG Item Number     " VIEW-AS ALERT-BOX ERROR.                                                                                                 */
/*       APPLY "entry" TO vend-whse-trans.fg-item-no IN BROWSE {&browse-name}.                                                                                          */
/*       RETURN ERROR.                                                                                                                                                  */
/*    END.                                                                                                                                                              */
/* END.                                                                                                                                                                 */
DO WITH FRAME {&FRAME-NAME}:
   IF NOT CAN-FIND(FIRST b-itemfg WHERE b-itemfg.company = cocode
                                    AND b-itemfg.i-no = TRIM(vend-whse-trans.fg-item-no:SCREEN-VALUE IN BROWSE {&browse-name})) THEN DO:
      MESSAGE "Invalid FG Item     " VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO vend-whse-trans.fg-item-no IN BROWSE {&browse-name}.
      RETURN ERROR.
   END.
   ELSE DO:
      IF NOT CAN-FIND(b-vend-whse-item WHERE TRIM(b-vend-whse-item.company) = TRIM(cocode)
                                         AND TRIM(b-vend-whse-item.fg-item-no) = TRIM(vend-whse-trans.fg-item-no:SCREEN-VALUE IN BROWSE {&browse-name})) THEN DO:
         MESSAGE 'FG Item "' vend-whse-trans.fg-item-no:SCREEN-VALUE IN BROWSE {&browse-name}  '" does not exist in the Vendor Management Item File.' SKIP
                 'Please add the FG Item to the Vendor Management Item File.' 
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY "entry" TO vend-whse-trans.cust-part-no IN BROWSE {&browse-name}.
         RETURN ERROR.
      END.
      ELSE DO:
         FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company = cocode 
                                       AND b-vend-whse-item.fg-item-no      = TRIM(vend-whse-trans.fg-item-no:SCREEN-VALUE IN BROWSE {&browse-name}) NO-LOCK NO-ERROR.
            IF AVAILABLE(b-vend-whse-item) THEN DO:
               ASSIGN 
                  vend-whse-trans.cust-part-no:SCREEN-VALUE IN BROWSE {&browse-name} = b-vend-whse-item.cust-part-no
                  vend-whse-trans.vendor-plant-code:SCREEN-VALUE IN BROWSE {&browse-name} = b-vend-whse-item.vendor-plant-code
                  vend-whse-trans.vendor-dept-code:SCREEN-VALUE IN BROWSE {&browse-name} = b-vend-whse-item.vendor-dept-code
                  vend-whse-trans.vendor-code:SCREEN-VALUE IN BROWSE {&browse-name} = b-vend-whse-item.vendor-code.
            END.
      END.
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val-if-exists B-table-Win 
PROCEDURE val-if-exists :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF BUFFER b-vend-whse-trans FOR vend-whse-trans.

   DO WITH FRAME {&FRAME-NAME}:
      IF CAN-FIND(FIRST b-vend-whse-trans WHERE b-vend-whse-trans.company           = cocode
                                            AND b-vend-whse-trans.trans-type        = "U"
                                            AND b-vend-whse-trans.cust-part-no      = vend-whse-trans.cust-part-no:SCREEN-VALUE IN BROWSE {&browse-name}
                                            AND b-vend-whse-trans.fg-item-no        = vend-whse-trans.fg-item-no:SCREEN-VALUE IN BROWSE {&browse-name}
                                            AND b-vend-whse-trans.vendor-code       = vend-whse-trans.vendor-code:SCREEN-VALUE IN BROWSE {&browse-name}
                                            AND b-vend-whse-trans.vendor-dept-code  = vend-whse-trans.vendor-dept-code:SCREEN-VALUE IN BROWSE {&browse-name}
                                            AND b-vend-whse-trans.vendor-plant-code = vend-whse-trans.vendor-plant-code:SCREEN-VALUE IN BROWSE {&browse-name}
                                            AND b-vend-whse-trans.r-no              <> INT(vend-whse-trans.r-no:SCREEN-VALUE IN BROWSE {&browse-name})) THEN DO: 
         MESSAGE "Usage transaction already exists for: " SKIP
                 "      Customer Part# '" + vend-whse-trans.cust-part-no:SCREEN-VALUE IN BROWSE {&browse-name}          + "'" SKIP
                 "      FG Item '"            + vend-whse-trans.fg-item-no:SCREEN-VALUE IN BROWSE {&browse-name}        + "'" SKIP
                 "      Customer A/P Code '"  + vend-whse-trans.vendor-code:SCREEN-VALUE IN BROWSE {&browse-name}       + "'" SKIP
                 "      Customer Plant ID '"  + vend-whse-trans.vendor-dept-code:SCREEN-VALUE IN BROWSE {&browse-name}  + "'" SKIP
                 "      Customer Dept Code '" + vend-whse-trans.vendor-plant-code:SCREEN-VALUE IN BROWSE {&browse-name} + "'" SKIP
               VIEW-AS ALERT-BOX ERROR.
            RUN local-cancel-record.
            RETURN ERROR.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val-item-line-no B-table-Win 
PROCEDURE val-item-line-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
   IF INT(vend-whse-trans.item-line-no:SCREEN-VALUE IN BROWSE {&browse-name}) > 0 THEN DO:
      IF NOT CAN-FIND(FIRST oe-ordl WHERE oe-ordl.company = cocode
                                      AND oe-ordl.i-no    = vend-whse-trans.fg-item-no:SCREEN-VALUE IN BROWSE {&browse-name} 
                                      AND oe-ordl.ord-no  = INT(vend-whse-trans.vend-ord-no:SCREEN-VALUE IN BROWSE {&browse-name})
                                      AND oe-ordl.line    = INT(vend-whse-trans.item-line-no:SCREEN-VALUE IN BROWSE {&browse-name})) THEN DO:
         MESSAGE "Invalid Suppliers Order Line Number " VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO vend-whse-trans.item-line-no IN BROWSE {&browse-name}.
         RETURN ERROR.
      END.
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val-item-po-no B-table-Win 
PROCEDURE val-item-po-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-vend-whse-item FOR vend-whse-item.
DEF BUFFER b-oe-rel FOR oe-rel.

DEF VAR v-cnt AS INTEGER NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
   IF vend-whse-trans.item-po-no:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN DO:
      FOR EACH b-oe-rel NO-LOCK WHERE b-oe-rel.company = cocode
                                   AND (b-oe-rel.stat    = "C" OR b-oe-rel.stat    = "Z")
                                   AND b-oe-rel.po-no   = vend-whse-trans.item-po-no:SCREEN-VALUE IN BROWSE {&browse-name}:
         v-cnt = v-cnt + 1.
      END.

      IF v-cnt <= 1 THEN DO:

         FIND FIRST b-oe-rel WHERE b-oe-rel.company = cocode
                               AND (b-oe-rel.stat    = "C" OR b-oe-rel.stat    = "Z")
                               AND b-oe-rel.po-no   = vend-whse-trans.item-po-no:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
         IF NOT AVAILABLE(b-oe-rel) THEN DO:
            MESSAGE "Invalid Customers PO Number    "
               VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO vend-whse-trans.item-po-no IN BROWSE {&browse-name}.
            RETURN ERROR.
         END.
         ELSE DO:
            FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company = cocode
                                          AND b-vend-whse-item.fg-item = TRIM(b-oe-rel.i-no) NO-LOCK NO-ERROR.
            IF NOT AVAILABLE(b-vend-whse-item) THEN DO:
               MESSAGE 'FG Item "' b-oe-rel.i-no  '" does not exist in the Vendor Management Item File.' SKIP
                       'Please add the FG Item to the Vendor Management Item File.'
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
               APPLY "entry" TO vend-whse-trans.item-po-no IN BROWSE {&browse-name}.
               RETURN ERROR.
            END.
            ELSE DO:
               ASSIGN vend-whse-trans.fg-item-no:SCREEN-VALUE IN BROWSE {&browse-name} = b-oe-rel.i-no.
            END.
         END.
      END.
   END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val-item-po-no-lookup B-table-Win 
PROCEDURE val-item-po-no-lookup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-oe-rel FOR oe-rel.
DEF BUFFER b-vend-whse-item FOR vend-whse-item.

DO WITH FRAME {&FRAME-NAME}:
   IF vend-whse-trans.item-po-no:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN DO:
      FIND FIRST b-oe-rel WHERE b-oe-rel.company = cocode
                            AND (b-oe-rel.stat    = "C" OR b-oe-rel.stat    = "Z")
                            AND b-oe-rel.po-no   = vend-whse-trans.item-po-no:SCREEN-VALUE IN BROWSE {&browse-name}
                            AND b-oe-rel.i-no    = vend-whse-trans.fg-item-no:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
      FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company = cocode
                                    AND b-vend-whse-item.fg-item = b-oe-rel.i-no NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(b-vend-whse-item) THEN DO:
         MESSAGE 'FG Item "' b-oe-rel.i-no  '" does not exist in the Vendor Management Item File.' SKIP
                 'Please add the FG Item to the Vendor Management Item File.' 
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY "entry" TO vend-whse-trans.item-po-no IN BROWSE {&browse-name}.
         RETURN ERROR.
      END.
      ELSE

   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val-job-no B-table-Win 
PROCEDURE val-job-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
   IF vend-whse-trans.vend-job-no:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN DO:
      vend-whse-trans.vend-job-no:SCREEN-VALUE IN BROWSE {&browse-name} = FILL(" ",6 - LENGTH(TRIM(vend-whse-trans.vend-job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) 
                                                                   + TRIM(vend-whse-trans.vend-job-no:SCREEN-VALUE IN BROWSE {&browse-name}).
   
      IF NOT CAN-FIND(FIRST oe-ordl WHERE oe-ordl.company  = cocode
                                      AND oe-ordl.i-no     = vend-whse-trans.fg-item-no:SCREEN-VALUE IN BROWSE {&browse-name}
                                      AND oe-ordl.job-no   = vend-whse-trans.vend-job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                                      AND oe-ordl.ord-no   = INT(vend-whse-trans.vend-ord-no:SCREEN-VALUE IN BROWSE {&browse-name})
                                      AND oe-ordl.line     = INT(vend-whse-trans.item-line-no:SCREEN-VALUE IN BROWSE {&browse-name})) THEN DO:
         MESSAGE "Invalid Suppliers Job Number     " VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO vend-whse-trans.vend-job-no IN BROWSE {&browse-name}.
         RETURN ERROR.
      END.
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val-job-no-2 B-table-Win 
PROCEDURE val-job-no-2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
   IF INT(vend-whse-trans.vend-job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) > 0 THEN DO:
      vend-whse-trans.vend-job-no:SCREEN-VALUE IN BROWSE {&browse-name} = FILL(" ",6 - LENGTH(TRIM(vend-whse-trans.vend-job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) 
                                                                   + TRIM(vend-whse-trans.vend-job-no:SCREEN-VALUE IN BROWSE {&browse-name}).
   
      IF NOT CAN-FIND(FIRST oe-ordl WHERE oe-ordl.company  = cocode
                                      AND oe-ordl.i-no     = vend-whse-trans.fg-item-no:SCREEN-VALUE IN BROWSE {&browse-name}
                                      AND oe-ordl.job-no   = vend-whse-trans.vend-job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                                      AND oe-ordl.job-no2  = INT(vend-whse-trans.vend-job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
                                      AND oe-ordl.ord-no   = INT(vend-whse-trans.vend-ord-no:SCREEN-VALUE IN BROWSE {&browse-name})
                                      AND oe-ordl.line     = INT(vend-whse-trans.item-line-no:SCREEN-VALUE IN BROWSE {&browse-name})) THEN DO:
         MESSAGE "Invalid Supliers Job Line Number    " VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO vend-whse-trans.vend-job-no2 IN BROWSE {&browse-name}.
         RETURN ERROR.
      END.
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val-sell-price B-table-Win 
PROCEDURE val-sell-price :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
   IF DEC(vend-whse-trans.sell-price:SCREEN-VALUE IN BROWSE {&browse-name}) > 0 THEN DO:
      vend-whse-trans.vend-job-no:SCREEN-VALUE IN BROWSE {&browse-name} = FILL(" ",6 - LENGTH(TRIM(vend-whse-trans.vend-job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) 
                                                                   + TRIM(vend-whse-trans.vend-job-no:SCREEN-VALUE IN BROWSE {&browse-name}).
   
      IF NOT CAN-FIND(FIRST oe-ordl WHERE oe-ordl.company  = cocode
                                      AND oe-ordl.i-no     = vend-whse-trans.fg-item-no:SCREEN-VALUE IN BROWSE {&browse-name}
                                      AND oe-ordl.job-no   = vend-whse-trans.vend-job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                                      AND oe-ordl.job-no2  = INT(vend-whse-trans.vend-job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
                                      AND oe-ordl.ord-no   = INT(vend-whse-trans.vend-ord-no:SCREEN-VALUE IN BROWSE {&browse-name})
                                      AND oe-ordl.line     = INT(vend-whse-trans.item-line-no:SCREEN-VALUE IN BROWSE {&browse-name}) 
                                      AND oe-ordl.t-price  = DEC(vend-whse-trans.sell-price:SCREEN-VALUE IN BROWSE {&browse-name})) THEN DO:
         MESSAGE "Invalid Suppliers Sell Price     " VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO vend-whse-trans.sell-price IN BROWSE {&browse-name}.
         RETURN ERROR.
      END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val-usage-qty B-table-Win 
PROCEDURE val-usage-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
      IF NOT INT(vend-whse-trans.trans-qty:SCREEN-VALUE IN BROWSE {&browse-name}) > 0 THEN DO:
         MESSAGE "Usage Quantity must be greater than 0     " VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO vend-whse-trans.trans-qty IN BROWSE {&browse-name}.
         RETURN ERROR.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val-vend-ord-no B-table-Win 
PROCEDURE val-vend-ord-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
   IF INT(vend-whse-trans.vend-ord-no:SCREEN-VALUE IN BROWSE {&browse-name}) > 0 THEN DO:
      IF NOT CAN-FIND(FIRST oe-ordl WHERE oe-ordl.company = cocode
                                      AND oe-ordl.i-no    = vend-whse-trans.fg-item-no:SCREEN-VALUE IN BROWSE {&browse-name} 
                                      AND oe-ordl.ord-no  = INT(vend-whse-trans.vend-ord-no:SCREEN-VALUE IN BROWSE {&browse-name})) THEN DO:
         MESSAGE "Invalid Suppliers Order Number      " VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO vend-whse-trans.vend-ord-no IN BROWSE {&browse-name}.
         RETURN ERROR.
      END.
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val-vendor-code B-table-Win 
PROCEDURE val-vendor-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
   IF NOT CAN-FIND(FIRST vend-code-cust-xref WHERE vend-code-cust-xref.company = cocode
                                               AND TRIM(vend-code-cust-xref.vendor-code) = TRIM(vend-whse-trans.vendor-code:SCREEN-VALUE IN BROWSE {&browse-name})) THEN DO:
      MESSAGE "Invalid Customers A/P Code      " VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO vend-whse-trans.vendor-code IN BROWSE {&browse-name}.
      RETURN ERROR.
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val-vendor-dept-code B-table-Win 
PROCEDURE val-vendor-dept-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
   IF vend-whse-trans.vendor-dept-code:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN DO:
      IF NOT CAN-FIND(FIRST vend-plant WHERE vend-plant.company          = cocode
                                         AND vend-plant.vendor-code      = vend-whse-trans.vendor-code:SCREEN-VALUE IN BROWSE {&browse-name} 
                                         AND vend-plant.plant-id         = vend-whse-trans.vendor-plant-code:SCREEN-VALUE IN BROWSE {&browse-name}
                                         AND vend-plant.vendor-dept-code = vend-whse-trans.vendor-dept-code:SCREEN-VALUE IN BROWSE {&browse-name}) THEN DO:         
         MESSAGE "Invalid Customers Plant Dept Code.      " 
            VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO vend-whse-trans.vendor-dept-code IN BROWSE {&browse-name}.
         RETURN ERROR.
      END.
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val-vendor-plant-code B-table-Win 
PROCEDURE val-vendor-plant-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
   IF vend-whse-trans.vendor-plant-code:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN DO:
      IF NOT CAN-FIND(FIRST vend-plant WHERE vend-plant.company = cocode
                                         AND vend-plant.vendor-code = vend-whse-trans.vendor-code:SCREEN-VALUE IN BROWSE {&browse-name}
                                         AND vend-plant.plant-id    = vend-whse-trans.vendor-plant-code:SCREEN-VALUE IN BROWSE {&browse-name}) THEN DO:
         MESSAGE "Invalid Customers Plant ID.      " 
            VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO vend-whse-trans.vendor-plant-code IN BROWSE {&browse-name}.
         RETURN ERROR.
      END.
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

