&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  ap\b-apinvl.w

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

&SCOPED-DEFINE yellowColumnsName ap-invl
&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

DEF BUFFER bf-invl FOR ap-invl.


DEF VAR v-wid AS DEC NO-UNDO.
DEF VAR v-len AS DEC NO-UNDO.
DEF VAR v-dep AS DEC NO-UNDO.
DEF VAR v-bwt AS DEC NO-UNDO.
DEF VAR lv-po-glnum AS LOG NO-UNDO.
DEF VAR v-vend-actnum AS cha NO-UNDO.
DEF VAR v-seq AS INT FORM ">>>9"  NO-UNDO.
DEF VAR li-line-num AS INT NO-UNDO.  /* for seq# */
DEF VAR v-vend-act AS cha NO-UNDO.
DEF VAR lv-pol-rowid AS ROWID NO-UNDO.
def var lv-uom-list as cha init "C,CS,EA,L,LB,LF,LOT,M,MSF,SHT,TON,BF" no-undo.
DEF VAR pr-uom-list AS cha NO-UNDO INIT "EA,LB,M,MSF,TON,BF".
DEF VAR cons-uom-list AS CHA NO-UNDO INIT "M,LF,EA,LB,TON".
DEF VAR v-po-qty as log initial true no-undo.
DEF VAR v-ap-pur AS CHAR NO-UNDO.

DEF NEW SHARED TEMP-TABLE tt-pol FIELD selekt AS LOG LABEL "Selected"
                      FIELD rec-id AS RECID                      
                      FIELD qty-inv AS log
                      FIELD amt-inv AS LOG
                      FIELD qty-to-inv LIKE ap-invl.qty
                      FIELD qty-to-inv-uom AS CHAR.

DEF TEMP-TABLE tt-ei NO-UNDO LIKE e-item.
DEF TEMP-TABLE tt-eiv NO-UNDO
    FIELD rec_key LIKE e-item-vend.rec_key
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20
    FIELD setups AS DECIMAL DECIMALS 2 EXTENT 20.


{sys/inc/VAR.i "new shared"}
ASSIGN cocode = g_company
       locode = g_loc.
{sys/inc/ap-gl#.i}
{sys/inc/apinvmsg.i}
lv-po-glnum = ap-gl#-log.
DEF VAR lv-num-rec AS INT NO-UNDO.
DEF VAR lv-job-no LIKE po-ordl.job-no NO-UNDO.
DEF VAR lv-job-no2 LIKE po-ordl.job-no2 NO-UNDO.
DEF VAR lv-snum LIKE po-ordl.s-num NO-UNDO.
DEF VAR lv-bnum LIKE po-ordl.b-num NO-UNDO.
DEF VAR lv-item-no LIKE po-ordl.i-no NO-UNDO.
DEF VAR lv-invl-qty LIKE ap-invl.qty NO-UNDO.
DEF VAR lv-rowid AS ROWID NO-UNDO.
DEF VAR ll-added AS LOG NO-UNDO.

DEF VAR v-actdscr LIKE account.dscr NO-UNDO.
def var factor# as decimal no-undo.
DEF VAR v-basis-w AS DEC NO-UNDO. /* for po/po-adder2.p */

DEF TEMP-TABLE tt-ap-invl NO-UNDO LIKE ap-invl
    FIELD tt-rowid AS ROWID.

DEF BUFFER b-tt FOR tt-ap-invl.

DO TRANSACTION:
  {sys/inc/apdesc.i}
  {sys/inc/apsecure.i}
  {sys/inc/fgpostgl.i}
  {sys/inc/appaper.i}
  {sys/inc/poqty.i}
  {sys/inc/aptax.i}
END.

DEF VAR v-rmpostgl-char AS cha NO-UNDO.
DO TRANSACTION:
  {sys/inc/rmpostgl.i}
END.
v-rmpostgl-char = sys-ctrl.char-fld.

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
&Scoped-define EXTERNAL-TABLES ap-inv
&Scoped-define FIRST-EXTERNAL-TABLE ap-inv


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ap-inv.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ap-invl

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table ap-invl.po-no ~
display-line() @ ap-invl.line ap-invl.actnum get-actdscr() @ v-actdscr ~
ap-invl.qty ap-invl.cons-uom ap-invl.unit-pr ap-invl.pr-qty-uom ap-invl.tax ~
ap-invl.sf-sht ap-invl.amt ap-invl.amt-msf display-item-no() @ lv-item-no ~
display-item-no() @ lv-item-no ap-invl.dscr display-job() @ lv-job-no ~
display-snum() @ lv-snum display-bnum() @ lv-bnum 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table ap-invl.po-no ~
ap-invl.actnum ap-invl.qty ap-invl.unit-pr ap-invl.pr-qty-uom ap-invl.tax ~
ap-invl.sf-sht ap-invl.amt ap-invl.dscr 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table ap-invl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table ap-invl
&Scoped-define QUERY-STRING-Browser-Table FOR EACH ap-invl WHERE ap-invl.i-no = ap-inv.i-no NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH ap-invl WHERE ap-invl.i-no = ap-inv.i-no NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table ap-invl
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table ap-invl


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 browse-order auto_find ~
Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order fi_sortby auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-bnum B-table-Win 
FUNCTION display-bnum RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-item-no B-table-Win 
FUNCTION display-item-no RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-job B-table-Win 
FUNCTION display-job RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-line B-table-Win 
FUNCTION display-line RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-seq B-table-Win 
FUNCTION display-seq RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-snum B-table-Win 
FUNCTION display-snum RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-actdscr B-table-Win 
FUNCTION get-actdscr RETURNS CHARACTER
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
     SIZE 47 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

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
      ap-invl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      ap-invl.po-no FORMAT ">>>>>9":U WIDTH 13.4 LABEL-BGCOLOR 14
      display-line() @ ap-invl.line COLUMN-LABEL "Line" FORMAT "999":U
      ap-invl.actnum COLUMN-LABEL "Account Number" FORMAT "x(25)":U
            LABEL-BGCOLOR 14
      get-actdscr() @ v-actdscr COLUMN-LABEL "Account Description" FORMAT "x(45)":U
      ap-invl.qty FORMAT "->>>,>>9.9<<<<<":U WIDTH 12.4 LABEL-BGCOLOR 14
      ap-invl.cons-uom COLUMN-LABEL "UOM" FORMAT "x(4)":U WIDTH 6.2
            LABEL-BGCOLOR 14
      ap-invl.unit-pr COLUMN-LABEL "Price" FORMAT "->,>>>,>>9.99<<<<":U
            LABEL-BGCOLOR 14
      ap-invl.pr-qty-uom COLUMN-LABEL "UOM Price" FORMAT "x(4)":U
            WIDTH 5 LABEL-BGCOLOR 14
      ap-invl.tax COLUMN-LABEL "Tax" FORMAT "Y/N":U WIDTH 5.2 LABEL-BGCOLOR 14
      ap-invl.sf-sht COLUMN-LABEL "SqFt" FORMAT ">>,>>9.9<<<":U
            WIDTH 14 LABEL-BGCOLOR 14
      ap-invl.amt FORMAT "->>>>,>>9.99":U LABEL-BGCOLOR 14
      ap-invl.amt-msf COLUMN-LABEL "Total MSF" FORMAT "->>,>>9.99":U
            WIDTH 12.4 LABEL-BGCOLOR 14
      display-item-no() @ lv-item-no COLUMN-LABEL "Item#" WIDTH 11
      display-item-no() @ lv-item-no
      ap-invl.dscr COLUMN-LABEL "Description" FORMAT "x(35)":U
            LABEL-BGCOLOR 14
      display-job() @ lv-job-no COLUMN-LABEL "Job#" FORMAT "x(10)":U
      display-snum() @ lv-snum
      display-bnum() @ lv-bnum
  ENABLE
      ap-invl.po-no
      ap-invl.actnum
      ap-invl.qty
      ap-invl.unit-pr
      ap-invl.pr-qty-uom
      ap-invl.tax
      ap-invl.sf-sht
      ap-invl.amt
      ap-invl.dscr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 7.62
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 8.86 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     fi_sortby AT ROW 8.86 COL 49 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     auto_find AT ROW 8.86 COL 70 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 8.86 COL 132 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 8.86 COL 2
     RECT-4 AT ROW 8.62 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   External Tables: ASI.ap-inv
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
         HEIGHT             = 9.14
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
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE
       Browser-Table:COLUMN-RESIZABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_sortby:HIDDEN IN FRAME F-Main           = TRUE
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.ap-invl WHERE ASI.ap-inv <external> ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _JoinCode[1]      = "ASI.ap-invl.i-no = ASI.ap-inv.i-no"
     _FldNameList[1]   > ASI.ap-invl.po-no
"ap-invl.po-no" ? ? "integer" ? ? ? 14 ? ? yes ? no no "13.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"display-line() @ ap-invl.line" "Line" "999" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.ap-invl.actnum
"ap-invl.actnum" "Account Number" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"get-actdscr() @ v-actdscr" "Account Description" "x(45)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.ap-invl.qty
"ap-invl.qty" ? "->>>,>>9.9<<<<<" "decimal" ? ? ? 14 ? ? yes ? no no "12.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.ap-invl.cons-uom
"ap-invl.cons-uom" "UOM" ? "character" ? ? ? 14 ? ? no ? no no "6.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.ap-invl.unit-pr
"ap-invl.unit-pr" "Price" ? "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.ap-invl.pr-qty-uom
"ap-invl.pr-qty-uom" "UOM Price" ? "character" ? ? ? 14 ? ? yes ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.ap-invl.tax
"ap-invl.tax" "Tax" ? "logical" ? ? ? 14 ? ? yes ? no no "5.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.ap-invl.sf-sht
"ap-invl.sf-sht" "SqFt" ">>,>>9.9<<<" "decimal" ? ? ? 14 ? ? yes ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.ap-invl.amt
"ap-invl.amt" ? ? "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.ap-invl.amt-msf
"ap-invl.amt-msf" "Total MSF" ? "decimal" ? ? ? 14 ? ? no ? no no "12.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"display-item-no() @ lv-item-no" "Item#" ? ? ? ? ? ? ? ? no ? no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"display-item-no() @ lv-item-no" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.ap-invl.dscr
"ap-invl.dscr" "Description" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"display-job() @ lv-job-no" "Job#" "x(10)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"display-snum() @ lv-snum" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"display-bnum() @ lv-bnum" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
  DEF VAR char-val AS cha NO-UNDO.
  DEF VAR lk-recid AS RECID NO-UNDO.
  DEF VAR uom-list AS CHAR INIT "" NO-UNDO.
  DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.


  lw-focus = FOCUS.

  CASE lw-focus:NAME:    
        WHEN "po-no" THEN DO:
             FIND FIRST vend
                 WHERE vend.company = g_company
                   AND vend.vend-no = ap-inv.vend-no
                 NO-LOCK NO-ERROR.
             RUN windows/l-poven.w (RECID(vend), ap-invl.po-no:SCREEN-VALUE IN BROWSE {&browse-name}, OUTPUT lk-recid).             
             IF lk-recid <> ? THEN RUN display-po (lk-recid).
        END.
        WHEN "actnum" THEN DO:
            RUN windows/l-acct3.w (g_company,"T",lw-focus:SCREEN-VALUE, OUTPUT char-val).
            IF char-val <> "" THEN ASSIGN lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
                                         
        END.
        when "pr-qty-uom" then do:
            RUN get-uom-list (OUTPUT uom-list).
            run windows/l-stduom.w (g_company,uom-list, lw-focus:screen-value, output char-val).
            if char-val <> "" then 
              assign lw-focus:screen-value = entry(1,char-val)
                       .
        end.
  END CASE.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON return OF Browser-Table IN FRAME F-Main
ANYWHERE
DO:
   APPLY "tab" TO SELF.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-DISPLAY OF Browser-Table IN FRAME F-Main
DO:
     li-line-num = li-line-num + 1.
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
   /*{src/adm/template/brsleave.i} */
    {brsleave.i}
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

  IF AVAIL ap-invl                                                          AND
     CAN-FIND(FIRST tt-ap-invl WHERE tt-ap-invl.tt-rowid EQ ROWID(ap-invl)) THEN
    RUN update-tt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-invl.po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-invl.po-no Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF ap-invl.po-no IN BROWSE Browser-Table /* PO Number */
DO:
    IF NOT adm-adding-record THEN do:
       APPLY "entry" TO ap-invl.actnum IN BROWSE {&browse-name}.
       RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-invl.po-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ap-invl.po-no IN BROWSE Browser-Table /* PO Number */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-po-no NO-ERROR.    
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    IF INT({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN DO:
      APPLY "row-leave" TO BROWSE {&browse-name}.
      RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-invl.actnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-invl.actnum Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ap-invl.actnum IN BROWSE Browser-Table /* Account Number */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-actnum NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-invl.qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-invl.qty Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF ap-invl.qty IN BROWSE Browser-Table /* Quantity */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN ap/d-selrec.w (ROWID(ap-inv), lv-pol-rowid, OUTPUT lv-invl-qty).

    IF lv-invl-qty NE 0 THEN DO WITH FRAME {&FRAME-NAME}: 
      {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(lv-invl-qty).
      APPLY "leave" TO {&self-name} IN BROWSE {&browse-name}.
      RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-invl.qty Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ap-invl.qty IN BROWSE Browser-Table /* Quantity */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-qty NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-invl.qty Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF ap-invl.qty IN BROWSE Browser-Table /* Quantity */
DO:
  RUN calc-amt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-invl.unit-pr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-invl.unit-pr Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ap-invl.unit-pr IN BROWSE Browser-Table /* Price */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-unit-pr NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-invl.unit-pr Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF ap-invl.unit-pr IN BROWSE Browser-Table /* Price */
DO:
  RUN calc-amt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-invl.pr-qty-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-invl.pr-qty-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ap-invl.pr-qty-uom IN BROWSE Browser-Table /* UOM Price */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-pr-qty-uom NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    RUN calc-amt.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-invl.amt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-invl.amt Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF ap-invl.amt IN BROWSE Browser-Table /* Amount */
DO:
  RUN calc-amt2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{custom/yellowColumns.i}
{sys/ref/pocost.i}

 find sys-ctrl where sys-ctrl.company = cocode
               and sys-ctrl.name = "poprint" 
           no-lock no-error.

 factor# = if avail sys-ctrl and can-do("Premier,Middlesx,16th's",sys-ctrl.char-fld)
  then .16 else 1.

 FIND FIRST ap-ctrl WHERE ap-ctrl.company EQ cocode NO-LOCK NO-ERROR.
 IF AVAIL ap-ctrl THEN
    v-ap-pur = ap-ctrl.purchases.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/*{custom/resizmn.i} */

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
  {src/adm/template/row-list.i "ap-inv"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ap-inv"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assign-ap-from-po B-table-Win 
PROCEDURE assign-ap-from-po :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   IF NOT AVAIL po-ordl THEN FIND po-ordl WHERE ROWID(po-ordl) = lv-pol-rowid NO-LOCK NO-ERROR.
   ASSIGN ap-invl.LINE = (po-ordl.LINE + (po-ordl.po-no * 1000) ) /* ap/invline.i 1 */
          .

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

  RUN get-link-handle IN adm-broker-hdl
                       (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
  phandle = WIDGET-HANDLE(char-hdl).
  RUN auto-add IN phandle.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE auto-add-tt B-table-Win 
PROCEDURE auto-add-tt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF CAN-FIND(FIRST tt-ap-invl WHERE tt-ap-invl.i-no EQ ap-inv.i-no) THEN
    RUN auto-add.

  /*ELSE
  IF ll-added THEN DO:
    {methods/run_link.i "TABLEIO-SOURCE" "set-focus"}
  END.*/

  ll-added = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table B-table-Win 
PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose: Builds the temp-table of available receipts for PO    
  Parameters:  ROWID of PO-ORD
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipriPO AS RECID NO-UNDO.

DEFINE VARIABLE lChoice AS LOGICAL NO-UNDO.
DEFINE VARIABLE dQtyReceived AS DECIMAL NO-UNDO.
DEFINE VARIABLE dQtyInvoiced AS DECIMAL NO-UNDO.
DEFINE VARIABLE dAmountReceived AS DECIMAL NO-UNDO.
DEFINE VARIABLE dAmountInvoiced AS DECIMAL NO-UNDO.
DEFINE VARIABLE lNegativeReceipt AS LOGICAL NO-UNDO.
DEFINE VARIABLE cPoNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMatExceptionList AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.

FIND po-ord WHERE RECID(po-ord) = ipriPO NO-LOCK NO-ERROR.

FOR EACH tt-pol:
    DELETE tt-pol.
END.
  
lv-num-rec = 0.

IF AVAILABLE po-ord THEN 
DO:
    RUN sys/ref/nk1Look.p(INPUT po-ord.company,
                          INPUT "APMatTypeExceptions",
                          INPUT "C",
                          INPUT NO,
                          INPUT NO,
                          INPUT "",
                          INPUT "",
                          OUTPUT cMatExceptionList,
                          OUTPUT lFound).
    IF NOT lFound OR cMatExceptionList EQ '' THEN
        cMatExceptionList = 'MOXY789@'.
    FOR EACH po-ordl 
        WHERE po-ordl.company EQ po-ord.company 
          AND po-ordl.po-no EQ po-ord.po-no 
          AND (NOT CAN-FIND(FIRST ap-invl 
                            WHERE ap-invl.i-no EQ ap-inv.i-no
                              AND ap-invl.po-no EQ po-ordl.po-no
                              AND {ap/invlline.i -1} EQ po-ordl.line
                            USE-INDEX i-no))
        USE-INDEX po-no NO-LOCK:
        
        IF LOOKUP(po-ordl.stat,"X,F") > 0 /* not deleted or cancelled */ THEN
            NEXT.
    
        lNegativeReceipt = NO.
    
        IF po-ordl.t-rec-qty EQ 0 
            AND NOT CAN-FIND(FIRST item 
                                WHERE item.company EQ cocode
                                  AND item.i-no EQ po-ordl.i-no
                                  AND item.i-code  EQ "R"
                                  AND item.stocked EQ NO) THEN
        DO:
            cPoNo = STRING(po-ordl.po-no).
    
            FOR EACH rm-rcpth 
                WHERE rm-rcpth.company EQ po-ordl.company 
                  AND rm-rcpth.po-no EQ cPoNo 
                  AND rm-rcpth.rita-code EQ "R"
                NO-LOCK,
                EACH rm-rdtlh 
                WHERE rm-rdtlh.r-no EQ rm-rcpth.r-no 
                  AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code 
                  AND rm-rdtlh.qty LT 0
                NO-LOCK:

                lNegativeReceipt = YES.
                LEAVE.
            END.
                
            IF lNegativeReceipt EQ NO THEN
            DO:
                FOR EACH fg-rcpth 
                    WHERE fg-rcpth.company EQ po-ordl.company 
                      AND fg-rcpth.vend-no EQ po-ord.vend-no 
                      AND fg-rcpth.po-no EQ cPoNo 
                      AND LOOKUP(fg-rcpth.rita-code,"R,E") > 0
                    NO-LOCK,
                    EACH fg-rdtlh 
                    WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no 
                      AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code 
                      AND fg-rdtlh.qty LT 0
                    NO-LOCK:
                      
                    lNegativeReceipt = YES.
                    LEAVE.
                END.
            END.
        END.
        
        IF NOT (po-ordl.t-rec-qty NE 0 OR lNegativeReceipt 
            OR (po-ordl.item-type 
                AND CAN-FIND(FIRST item WHERE item.company EQ cocode
                AND item.i-no    EQ po-ordl.i-no 
                AND item.i-code  EQ "R"
                AND item.stocked EQ NO))) THEN NEXT.
            
        RUN sys/inc/po-recqa.p (RECID(po-ordl), OUTPUT dQtyReceived, OUTPUT dAmountReceived).    
        RUN sys/inc/po-invqa.p (RECID(po-ordl), OUTPUT dQtyInvoiced, OUTPUT dAmountInvoiced).
    
        ASSIGN 
            dAmountInvoiced = 0
            dAmountReceived = 0.
    
        FIND FIRST ITEM 
            WHERE item.company EQ cocode
              AND item.i-no    EQ po-ordl.i-no
              AND po-ordl.item-type
            NO-LOCK NO-ERROR.
    
        IF po-ordl.stat NE "C" 
            OR (apinvmsg-log = YES AND dQtyReceived EQ 0 
                AND dQtyInvoiced EQ 0) 
            OR dQtyReceived NE dQtyInvoiced 
            OR (AVAILABLE item           
                AND item.i-code EQ "R"   
                AND INDEX(cMatExceptionList,ITEM.mat-type) GT 0
                AND item.stocked EQ NO)  THEN 
        DO:
            CREATE tt-pol.
            ASSIGN 
                tt-pol.rec-id  = RECID(po-ordl)
                tt-pol.qty-inv = (apinvmsg-log = YES AND dQtyReceived EQ 0 AND
                                     dQtyInvoiced EQ 0) OR
                                    (dQtyInvoiced NE dQtyReceived AND dQtyReceived NE 0)
                tt-pol.amt-inv = dAmountInvoiced NE dAmountReceived AND dAmountReceived NE 0
                tt-pol.selekt  = IF lChoice THEN YES ELSE NO.
            lv-num-rec = lv-num-rec + 1.
        END.
    END.
END. /*avail po-ord*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-amt B-table-Win 
PROCEDURE calc-amt :
/*------------------------------------------------------------------------------
  Purpose:     from ap/ap-inlu.p
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR v-qty AS DEC NO-UNDO.
  DEF VAR lv-invl-qty LIKE ap-invl.qty NO-UNDO.
  DEF VAR v-temp-pr AS DEC FORM "->>>,>>9.99<<<" /*LIKE ap-invl.unit-pr */ NO-UNDO.
  DEF VAR ld-amt-msf LIKE ap-invl.amt-msf NO-UNDO.

  {ce/msfcalc.i}
 IF BROWSE {&browse-name}:NUM-SELECTED-ROWS >= 1  /* not to have screen-value error*/
  THEN DO:

  FIND po-ordl WHERE po-ordl.company = g_company
                 AND po-ordl.po-no = ap-invl.po-no
                 AND po-ordl.LINE = {ap/invlline.i -1} NO-LOCK NO-ERROR.
  
  IF AVAIL po-ordl THEN DO:
     ASSIGN v-wid = po-ordl.s-wid
            v-len = IF po-ordl.pr-qty-uom EQ "ROLL" THEN 12 ELSE po-ordl.s-len
            v-dep = 0
            v-bwt = 0.
     
     find first ITEM where item.company eq cocode
                       and item.i-no    eq po-ordl.i-no
                       and po-ordl.item-type
                       no-lock no-error.            
     if avail item then do:
          v-dep = item.s-dep.          
          {po/pol-dims.i}
     end.
  END.

  if v-wid eq 0 or v-len eq 0 then do:
    if v-wid eq 0 THEN v-wid = if v-len eq 0 then 12 else v-len.
    v-len = dec(ap-invl.sf-sht:SCREEN-VALUE IN BROWSE {&browse-name}) * 144 / v-wid.
  end.
  
  if v-len eq 0 then v-len = 12.
  
  if ap-invl.pr-qty-uom:SCREEN-VALUE eq ap-invl.cons-uom:SCREEN-VALUE OR
     ap-invl.pr-qty-uom:SCREEN-VALUE = "0"
      THEN v-temp-pr = dec(ap-invl.unit-pr:SCREEN-VALUE).
  ELSE
    run sys/ref/convcuom.p (ap-invl.pr-qty-uom:SCREEN-VALUE, ap-invl.cons-uom:SCREEN-VALUE,
                            v-bwt, v-len, v-wid, v-dep,
                            dec(ap-invl.unit-pr:SCREEN-VALUE), output v-temp-pr).

  ap-invl.amt:SCREEN-VALUE = string(dec(ap-invl.qty:SCREEN-VALUE) * v-temp-pr).
  
  IF ap-invl.pr-qty-uom:SCREEN-VALUE EQ "L" THEN ap-invl.amt:SCREEN-VALUE = ap-invl.unit-pr:SCREEN-VALUE.

  if ap-invl.cons-uom:SCREEN-VALUE eq "MSF" 
     THEN ap-invl.amt-msf:SCREEN-VALUE = string(ap-invl.qty:SCREEN-VALUE).
  else DO: 
    run sys/ref/convquom.p (ap-invl.cons-uom:SCREEN-VALUE, "MSF",
                          v-bwt, v-len, v-wid, v-dep,
                          DEC(ap-invl.qty:SCREEN-VALUE), output ld-amt-msf).
    ap-invl.amt-msf:SCREEN-VALUE = STRING(ld-amt-msf).
  END.
  
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-amt2 B-table-Win 
PROCEDURE calc-amt2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR v-qty AS DEC NO-UNDO.
  DEF VAR lv-invl-qty LIKE ap-invl.qty NO-UNDO.
  DEF VAR v-temp-pr LIKE ap-invl.unit-pr NO-UNDO.
  DEF VAR ld-amt-msf LIKE ap-invl.amt-msf NO-UNDO.

  {ce/msfcalc.i}
  IF BROWSE {&browse-name}:NUM-SELECTED-ROWS >= 1  /* not to have screen-value error*/
    THEN DO:

    FIND po-ordl WHERE po-ordl.company = g_company
                 AND po-ordl.po-no = ap-invl.po-no
                 AND po-ordl.LINE = {ap/invlline.i -1} NO-LOCK NO-ERROR.
  
    IF AVAIL po-ordl THEN DO:
       ASSIGN v-wid = po-ordl.s-wid
              v-len = IF po-ordl.pr-qty-uom EQ "ROLL" THEN 12 ELSE po-ordl.s-len
              v-dep = 0
              v-bwt = 0
            .
     
       find first ITEM where item.company eq cocode
                       and item.i-no    eq po-ordl.i-no
                       and po-ordl.item-type
                       no-lock no-error.            
       if avail item then do:
            v-dep = item.s-dep.          
            {po/pol-dims.i}
       end.
    END.

     if v-wid eq 0 or v-len eq 0 then do:
       if v-wid eq 0 THEN v-wid = if v-len eq 0 then 12 else v-len.
       v-len = dec(ap-invl.sf-sht:SCREEN-VALUE IN BROWSE {&browse-name}) * 144 / v-wid.
     end.
   
     if v-len eq 0 then v-len = 12.

     v-temp-pr = dec(ap-invl.amt:SCREEN-VALUE) / dec(ap-invl.qty:SCREEN-VALUE).

     if ap-invl.pr-qty-uom:SCREEN-VALUE NE ap-invl.cons-uom:SCREEN-VALUE AND
        ap-invl.pr-qty-uom:SCREEN-VALUE NE "0" THEN
       run sys/ref/convcuom.p (ap-invl.cons-uom:SCREEN-VALUE, ap-invl.pr-qty-uom:SCREEN-VALUE,
                               v-bwt, v-len, v-wid, v-dep,
                               v-temp-pr, output v-temp-pr).
                               
     ap-invl.unit-pr:SCREEN-VALUE = string(v-temp-pr).
   
     if ap-invl.cons-uom:SCREEN-VALUE eq "MSF" 
        THEN ap-invl.amt-msf:SCREEN-VALUE = string(ap-invl.qty:SCREEN-VALUE).
     else DO: 
       run sys/ref/convquom.p (ap-invl.cons-uom:SCREEN-VALUE, "MSF",
                            v-bwt, v-len, v-wid, v-dep,
                            DEC(ap-invl.qty:SCREEN-VALUE), output ld-amt-msf).
       ap-invl.amt-msf:SCREEN-VALUE = STRING(ld-amt-msf). 
     END.
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-amt3 B-table-Win 
PROCEDURE calc-amt3 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-qty AS DEC NO-UNDO.
  DEF VAR lv-invl-qty LIKE ap-invl.qty NO-UNDO.
  DEF VAR v-temp-pr AS DEC FORM "->>>,>>9.99<<<" /*LIKE ap-invl.unit-pr */ NO-UNDO.

  {ce/msfcalc.i}

  ASSIGN v-wid = po-ordl.s-wid
         v-len = IF po-ordl.pr-qty-uom EQ "ROLL" THEN 12 ELSE po-ordl.s-len
         v-dep = 0
         v-bwt = 0.
  
  find first ITEM where item.company eq cocode
                    and item.i-no    eq po-ordl.i-no
                    and po-ordl.item-type
                    no-lock no-error.            
  if avail item then do:
       v-dep = item.s-dep.          
       {po/pol-dims.i}
  end.
  

  if v-wid eq 0 or v-len eq 0 then do:
    if v-wid eq 0 THEN v-wid = if v-len eq 0 then 12 else v-len.
    v-len = ap-invl.sf-sht * 144 / v-wid.
  end.
  
  if v-len eq 0 then v-len = 12.
  
  if ap-invl.pr-qty-uom eq ap-invl.cons-uom OR
     ap-invl.pr-qty-uom = "0" THEN
     v-temp-pr = ap-invl.unit-pr.
  ELSE
    run sys/ref/convcuom.p (ap-invl.pr-qty-uom, ap-invl.cons-uom,
                            v-bwt, v-len, v-wid, v-dep,
                            ap-invl.unit-pr, output v-temp-pr).

  ap-invl.amt = ap-invl.qty * v-temp-pr.
  
  IF ap-invl.pr-qty-uom EQ "L" THEN ap-invl.amt = ap-invl.unit-pr.

  if ap-invl.cons-uom eq "MSF" 
     THEN ap-invl.amt-msf = ap-invl.qty.
  else  
     run sys/ref/convquom.p (ap-invl.cons-uom, "MSF",
                          v-bwt, v-len, v-wid, v-dep,
                          ap-invl.qty, output ap-invl.amt-msf).
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clear-lines B-table-Win 
PROCEDURE clear-lines :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RELEASE ap-inv.

  RUN dispatch ("open-query").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-ap-from-po B-table-Win 
PROCEDURE create-ap-from-po :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-qty AS DEC NO-UNDO.
  DEF VAR v-vend-act AS CHAR NO-UNDO.
  DEF VAR v-dscr AS CHAR NO-UNDO.
  DEF VAR v-tmp-qty LIKE ap-invl.qty NO-UNDO.

  DEF VAR v-ext-cost AS DEC NO-UNDO.
  DEF VAR v-qty-uom AS CHAR NO-UNDO.
  DEF VAR v-out-qty AS DEC NO-UNDO.        
  DEF VAR v-out-cost AS DEC NO-UNDO.
  DEF VAR v-setup-per AS DEC NO-UNDO.

  DEF VAR lSetup AS LOGICAL NO-UNDO.

  DEF BUFFER bf-ap-invl FOR ap-invl.

  {ce/msfcalc.i}


  FIND FIRST vend NO-LOCK
      WHERE vend.company EQ ap-inv.company
        AND vend.vend-no EQ ap-inv.vend-no
      NO-ERROR.
  ASSIGN
   v-vend-act = IF AVAIL vend THEN vend.actnum ELSE ""
   v-dscr     = "".

  IF v-vend-act EQ "" THEN
     v-vend-act = v-ap-pur.

  FOR EACH tt-pol
      WHERE tt-pol.selekt
        AND tt-pol.qty-to-inv NE 0,
      FIRST po-ordl WHERE RECID(po-ordl) EQ tt-pol.rec-id NO-LOCK
      BREAK BY po-ordl.po-no:

      FIND FIRST po-ord NO-LOCK
          WHERE po-ord.company EQ po-ordl.company 
            AND po-ord.po-no   EQ po-ordl.po-no
          NO-ERROR.

     CREATE tt-ap-invl.
     ASSIGN tt-ap-invl.i-no =  ap-inv.i-no
            tt-ap-invl.actnum = v-vend-act
            tt-ap-invl.company = ap-inv.company
            tt-ap-invl.vend-no = ap-inv.vend-no
            tt-ap-invl.dscr = v-dscr
            tt-ap-invl.loc = ap-inv.loc
            tt-ap-invl.period = ap-inv.period
            tt-ap-invl.posted = ap-inv.posted
            tt-ap-invl.tax = ap-inv.tax-gr NE ""
            .
      IF aptax-chr = "ITEM" THEN DO:
        FIND ITEM WHERE ITEM.company = g_company
                 AND ITEM.i-no = po-ordl.i-no NO-LOCK NO-ERROR.
        IF AVAIL ITEM THEN tt-ap-invl.tax = ITEM.tax-rcpt .
     END.

     ASSIGN tt-ap-invl.po-no = (po-ord.po-no)
            tt-ap-invl.LINE = (po-ordl.LINE + (po-ord.po-no * 1000) ) /* ap/invline.i 1 */
            tt-ap-invl.dscr = po-ordl.i-name
            tt-ap-invl.unit-pr = (po-ordl.cost)
            tt-ap-invl.pr-qty-uom = po-ordl.pr-uom
            tt-ap-invl.cons-uom = po-ordl.pr-qty-uom
            v-wid = po-ordl.s-wid
            v-len = IF po-ordl.pr-qty-uom EQ "ROLL" THEN 12 ELSE po-ordl.s-len
            v-dep = 0
            v-bwt = 0.
     
     IF tt-ap-invl.cons-uom EQ "ROLL" THEN tt-ap-invl.cons-uom = "LF".

     IF po-ordl.item-type AND appaper-chr NE "PO UOM" AND
        CAN-FIND(FIRST item
                 WHERE item.company EQ cocode
                   AND item.i-no    EQ po-ordl.i-no
                   AND item.mat-type EQ "P")          THEN
       tt-ap-invl.cons-uom = appaper-chr.

     RELEASE prod.
     RELEASE costtype.

     IF po-ordl.item-type EQ NO                          AND
        (fgpostgl EQ "AllItems" OR fgpostgl EQ "POOnly") THEN DO:
       FIND FIRST itemfg
           WHERE itemfg.company EQ po-ordl.company
             AND itemfg.i-no    EQ po-ordl.i-no
           NO-LOCK NO-ERROR.
              
       IF AVAIL itemfg THEN
       FIND FIRST prodl 
           WHERE prodl.company EQ itemfg.company
             AND prodl.procat  EQ itemfg.procat
             AND CAN-FIND(FIRST prod
                          WHERE prod.company EQ cocode
                            AND prod.prolin  EQ prodl.prolin)
           NO-LOCK NO-ERROR.

       IF AVAIL prodl THEN
       FIND FIRST prod
           WHERE prod.company EQ prodl.company
             AND prod.prolin  EQ prodl.prolin
           NO-LOCK NO-ERROR.
     END.

     ELSE
     IF po-ordl.item-type AND v-rmpostgl-char EQ "ALLITEMS" THEN DO:
       FIND FIRST item
          WHERE item.company EQ cocode
            AND item.i-no    EQ po-ordl.i-no
          NO-LOCK NO-ERROR.
      
       IF AVAIL item AND item.stocked THEN
       FIND FIRST costtype
           WHERE costtype.company   EQ cocode
             AND costtype.cost-type EQ item.cost-type
           NO-LOCK NO-ERROR.
     END.

     IF AVAIL prod AND prod.wip-mat NE "" THEN
        tt-ap-invl.actnum = prod.wip-mat.
     ELSE
     IF AVAIL costtype AND costtype.ap-accrued NE "" THEN
        tt-ap-invl.actnum = costtype.ap-accrued.
     ELSE
     IF lv-po-glnum THEN tt-ap-invl.actnum = po-ordl.actnum.
     ELSE DO:
        if v-vend-actnum eq "" then do:
            find first vend where vend.company eq cocode
                  and vend.vend-no eq po-ord.vend-no 
                no-lock no-error.
            if avail vend then v-vend-actnum = vend.actnum.
        end.
        tt-ap-invl.actnum = v-vend-actnum.
     end.  

     IF v-vend-actnum EQ "" THEN
        v-vend-actnum = v-ap-pur.

     if tt-ap-invl.actnum eq "" then tt-ap-invl.actnum = v-vend-actnum.

     find first ITEM where item.company eq cocode
                       and item.i-no    eq po-ordl.i-no
                       and po-ordl.item-type
                       no-lock no-error.            
     if avail item then do:
          v-dep = item.s-dep.          
          {po/pol-dims.i}
     end.

        IF NOT po-ordl.item-type AND tt-ap-invl.cons-uom NE "EA" THEN
          RUN sys/ref/convquom.p ("EA", tt-ap-invl.cons-uom,
                                  v-bwt, v-len, v-wid, v-dep,
                                  tt-pol.qty-to-inv, OUTPUT tt-pol.qty-to-inv).

        ASSIGN
         tt-ap-invl.qty     = tt-pol.qty-to-inv.

        IF tt-pol.qty-to-inv-uom NE "" AND
           tt-pol.qty-to-inv-uom NE po-ordl.pr-qty-uom THEN
           RUN sys/ref/convquom.p (tt-pol.qty-to-inv-uom, po-ordl.pr-qty-uom,
                                   v-bwt, v-len, v-wid, v-dep,
                                   tt-ap-invl.qty, OUTPUT v-tmp-qty).
        ELSE
           v-tmp-qty = tt-ap-invl.qty.
        IF LOOKUP(po-ordl.pr-uom,"L,LOT") GT 0 THEN DO:
            ASSIGN 
                tt-ap-invl.amt = po-ordl.t-cost
                tt-ap-invl.unit-pr = po-ordl.cost
                tt-ap-invl.pr-qty-uom = po-ordl.pr-uom.
        END.
        ELSE DO:
            /*Calculate proportionate amt based on total cost, not including setup*/
            tt-ap-invl.amt = (po-ordl.t-cost - po-ordl.setup) * ( v-tmp-qty / po-ordl.ord-qty ).
    
            /*Add setup charges back only if no other ap-invl exist for this po line*/
            lSetup = NO.
            FIND FIRST bf-ap-invl 
                WHERE bf-ap-invl.company EQ po-ordl.company
                  AND bf-ap-invl.po-no EQ po-ordl.po-no
                  AND bf-ap-invl.po-line EQ po-ordl.LINE
                  AND bf-ap-invl.item-no EQ po-ordl.i-no
                NO-LOCK NO-ERROR.
            IF NOT AVAIL bf-ap-invl THEN lSetup = YES.
            IF lSetup THEN tt-ap-invl.amt = tt-ap-invl.amt + po-ordl.setup.
            
            tt-ap-invl.unit-pr = tt-ap-invl.amt / tt-ap-invl.qty.
    
            IF tt-ap-invl.cons-uom NE tt-ap-invl.pr-qty-uom THEN
              RUN sys/ref/convcuom.p (tt-ap-invl.cons-uom, tt-ap-invl.pr-qty-uom,
                                      v-bwt, v-len, v-wid, v-dep,
                                      tt-ap-invl.unit-pr, OUTPUT tt-ap-invl.unit-pr).
            tt-ap-invl.unit-pr = ROUND(tt-ap-invl.unit-pr, 2).
        END.
         if v-len eq 0 then v-len = 12.
        if v-wid eq 0 then v-wid = 12.
        
        tt-ap-invl.sf-sht = if v-corr then (v-len * v-wid * .007)
                                      else (v-len * v-wid / 144).

        if not avail item             and
           (v-len eq 0 or v-wid eq 0) then do:
          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq po-ordl.i-no
                and NOT po-ordl.item-type
              no-lock no-error.
          if avail itemfg then tt-ap-invl.sf-sht = (itemfg.t-sqft).
        end.

        if tt-ap-invl.cons-uom eq "EA" THEN v-qty = tt-ap-invl.qty.          
        else
          run sys/ref/convquom.p(tt-ap-invl.cons-uom, "EA",
                                 v-bwt, v-len, v-wid, v-dep,
                                 tt-ap-invl.qty, output v-qty).

        tt-ap-invl.amt-msf = (tt-ap-invl.sf-sht * v-qty / 1000).

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-ap-from-po-old B-table-Win 
PROCEDURE create-ap-from-po-old :
/*------------------------------------------------------------------------------
  Purpose:    from ap/ap-inv.i  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF VAR v-qty AS DEC NO-UNDO.
  DEF VAR v-temp-pr LIKE bf-invl.unit-pr NO-UNDO.
  DEF VAR z AS INT NO-UNDO.
  DEF VAR v-vend-act AS cha NO-UNDO.
  DEF VAR v-dscr AS cha NO-UNDO.

  DEF BUFFER b-ap-inv FOR ap-inv.
  

  FIND LAST bf-invl WHERE bf-invl.i-no = ap-inv.i-no AND bf-invl.po-no = 0
                    USE-INDEX i-no NO-ERROR.
  z = IF AVAIL bf-invl THEN bf-invl.LINE + 1 ELSE 1.
  FIND FIRST vend WHERE vend.company = ap-inv.company
                    AND vend.vend-no = ap-inv.vend-no NO-LOCK NO-ERROR.
  v-vend-act = IF AVAIL vend THEN vend.actnum ELSE "".

  IF v-vend-act EQ "" THEN
     v-vend-act = v-ap-pur.
  
  v-dscr = "".
  IF AVAIL ap-invl THEN lv-rowid = ROWID(ap-invl).

  {ce/msfcalc.i}

  FOR EACH tt-pol WHERE tt-pol.selekt,
      first po-ordl WHERE RECID(po-ordl) = tt-pol.rec-id NO-LOCK
      BREAK BY po-ordl.po-no:

      FIND FIRST po-ord WHERE po-ord.company  = po-ordl.company 
                         AND po-ord.po-no = po-ordl.po-no NO-LOCK NO-ERROR.

          CREATE bf-invl.
     ASSIGN bf-invl.i-no =  ap-inv.i-no
            bf-invl.actnum = v-vend-act
            bf-invl.company = ap-inv.company
            bf-invl.vend-no = ap-inv.vend-no
            bf-invl.LINE = z
            bf-invl.dscr = v-dscr
            bf-invl.loc = ap-inv.loc
            bf-invl.period = ap-inv.period
            bf-invl.posted = ap-inv.posted
            .
     ASSIGN bf-invl.po-no = (po-ord.po-no)
            bf-invl.LINE = (po-ordl.LINE + (po-ord.po-no * 1000) ) /* ap/invline.i 1 */
            bf-invl.dscr = po-ordl.i-name
            bf-invl.unit-pr = (po-ordl.cost)
            bf-invl.pr-qty-uom = po-ordl.pr-uom
            bf-invl.cons-uom = po-ordl.pr-qty-uom
            v-wid = po-ordl.s-wid
            v-len = IF po-ordl.pr-qty-uom EQ "ROLL" THEN 12 ELSE po-ordl.s-len
            v-dep = 0
            v-bwt = 0.

     IF bf-invl.cons-uom EQ "ROLL" THEN bf-invl.cons-uom = "LF".

     IF lv-po-glnum THEN bf-invl.actnum = po-ordl.actnum.
     ELSE DO:
        if v-vend-actnum eq "" then do:
            find first vend where vend.company eq cocode
                  and vend.vend-no eq po-ord.vend-no 
                no-lock no-error.
            if avail vend then v-vend-actnum = vend.actnum.
        end.
        bf-invl.actnum = v-vend-actnum.
     end.   

     IF v-vend-actnum EQ "" THEN
        v-vend-actnum = v-ap-pur.

     if bf-invl.actnum eq "" then bf-invl.actnum = v-vend-actnum.

     find first ITEM where item.company eq cocode
                       and item.i-no    eq po-ordl.i-no
                       and po-ordl.item-type
                       no-lock no-error.            
     if avail item then do:
          v-dep = item.s-dep.          
          {po/pol-dims.i}
     end.
        
     /*   
     if po-ordl.item-type then do:
          v-qty = if avail item           and
                     item.i-code eq "R"   and
                     item.mat-type eq "M" and
                     item.stocked eq no   then po-ordl.cons-qty
                  else po-ordl.t-rec-qty.
                   
          if po-ordl.cons-uom eq bf-invl.cons-uom then
                 bf-invl.qty = (v-qty - po-ordl.t-inv-qty).
          
          else do:
            run sys/ref/convquom.p (po-ordl.cons-uom, bf-invl.cons-uom ,
                                   v-bwt, v-len, v-wid, v-dep,
                                   v-qty, output v-qty).
                                   
            bf-invl.qty = (v-qty - po-ordl.t-inv-qty).
          end.
     end.          
     ELSE do:
          if bf-invl.cons-uom eq "EA" then
            bf-invl.qty = (po-ordl.t-rec-qty - po-ordl.t-inv-qty).
            
          else do:
            run sys/ref/convquom.p("EA", bf-invl.cons-uom,
                                   0, 0, 0, 0,
                                   po-ordl.t-rec-qty, output v-qty).
                                   
            bf-invl.qty = (v-qty - po-ordl.t-inv-qty).
          end.  
      end.
      */

      if bf-invl.pr-qty-uom eq bf-invl.cons-uom
          THEN v-temp-pr = bf-invl.unit-pr.          
      else
          run sys/ref/convcuom.p (bf-invl.pr-qty-uom, bf-invl.cons-uom,
                                  v-bwt, v-len, v-wid, v-dep,
                                  bf-invl.unit-pr, output v-temp-pr).

        assign
         bf-invl.qty = tt-pol.qty-to-inv
         bf-invl.amt = (bf-invl.qty * v-temp-pr)
         /*ap-inv.net  = ap-inv.net + dec(bf-invl.amt)*/.

        if v-len eq 0 then v-len = 12.
        if v-wid eq 0 then v-wid = 12.
        
        bf-invl.sf-sht = if v-corr then (v-len * v-wid * .007)
                                      else (v-len * v-wid / 144).

        if not avail item             and
           (v-len eq 0 or v-wid eq 0) then do:
          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq po-ordl.i-no
                and NOT po-ordl.item-type
              no-lock no-error.
          if avail itemfg then bf-invl.sf-sht = (itemfg.t-sqft).
        end.

        if bf-invl.cons-uom eq "EA" THEN v-qty = bf-invl.qty.          
        else
          run sys/ref/convquom.p(bf-invl.cons-uom, "EA",
                                 v-bwt, v-len, v-wid, v-dep,
                                 bf-invl.qty, output v-qty).

        bf-invl.amt-msf = (bf-invl.sf-sht * v-qty / 1000).
 
  
     FIND b-ap-inv WHERE ROWID(b-ap-inv) EQ ROWID(ap-inv).

     IF LAST-OF(po-ordl.po-no) THEN b-ap-inv.freight = po-ord.t-freight.
     ASSIGN
      b-ap-inv.net = b-ap-inv.net + bf-invl.amt
      b-ap-inv.due = b-ap-inv.net - b-ap-inv.disc-taken -
                     b-ap-inv.paid + b-ap-inv.freight
      lv-rowid     = ROWID(bf-invl).

     FIND CURRENT b-ap-inv NO-LOCK.
  END.

  RUN repo-query (lv-rowid).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-tt B-table-Win 
PROCEDURE delete-tt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF AVAIL ap-invl THEN DO:
    FIND FIRST tt-ap-invl WHERE tt-ap-invl.tt-rowid EQ ROWID(ap-invl) NO-ERROR.
    IF AVAIL tt-ap-invl THEN DELETE tt-ap-invl.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-po B-table-Win 
PROCEDURE display-po :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  SESSION:SET-WAIT-STATE("general").

  DEF INPUT PARAMETER ip-porecid AS RECID .
  DEF VAR v-qty AS DEC NO-UNDO.
  DEF VAR v-temp-pr LIKE ap-invl.unit-pr NO-UNDO.
  {ce/msfcalc.i}

  FIND po-ordl WHERE RECID(po-ordl) EQ ip-porecid NO-LOCK NO-ERROR.
  
  IF AVAIL po-ordl THEN DO:
     lv-pol-rowid = ROWID(po-ordl).
     FIND FIRST po-ord WHERE po-ord.company = po-ordl.company 
                         AND po-ord.po-no = po-ordl.po-no NO-LOCK NO-ERROR.
     ASSIGN ap-invl.po-no:SCREEN-VALUE IN BROWSE {&browse-name} = string(po-ord.po-no)
            /*ap-invl.LINE:SCREEN-VALUE = string(po-ordl.LINE + (po-ord.po-no * 1000) ) /* ap/invline.i 1 */ */
            ap-invl.dscr:SCREEN-VALUE = po-ordl.i-name
            ap-invl.unit-pr:SCREEN-VALUE = STRING(po-ordl.cost)
            ap-invl.pr-qty-uom:SCREEN-VALUE = po-ordl.pr-uom
            ap-invl.cons-uom:SCREEN-VALUE = po-ordl.pr-qty-uom
            v-wid = po-ordl.s-wid
            v-len = IF po-ordl.pr-qty-uom EQ "ROLL" THEN 12 ELSE po-ordl.s-len
            v-dep = 0
            v-bwt = 0.

     IF aptax-chr = "ITEM" THEN DO:
        FIND ITEM WHERE ITEM.company = g_company
                 AND ITEM.i-no = po-ordl.i-no NO-LOCK NO-ERROR.
        IF AVAIL ITEM THEN ap-invl.tax:SCREEN-VALUE = IF ITEM.tax-rcpt THEN "Y" 
                          ELSE "N".
     END.
     IF ap-invl.cons-uom EQ "ROLL" THEN ap-invl.cons-uom = "LF".

     IF lv-po-glnum THEN ap-invl.actnum:SCREEN-VALUE = po-ordl.actnum.
     ELSE DO:
        if v-vend-actnum eq "" then do:
            find first vend where vend.company eq cocode
                  and vend.vend-no eq po-ord.vend-no 
                no-lock no-error.
            if avail vend then v-vend-actnum = vend.actnum.
        end.
        ap-invl.actnum:SCREEN-VALUE = v-vend-actnum.
     end.  
     IF v-vend-actnum EQ "" THEN
        v-vend-actnum = v-ap-pur.
     
     if ap-invl.actnum:SCREEN-VALUE eq "" then ap-invl.actnum:SCREEN-VALUE = v-vend-actnum.
     
     find first ITEM where item.company eq cocode
                       and item.i-no    eq po-ordl.i-no
                       and po-ordl.item-type
                       no-lock no-error.            
     if avail item then do:
          v-dep = item.s-dep.          
          {po/pol-dims.i}
     end.
        
     if po-ordl.item-type then do:
          v-qty = if avail item           and
                     item.i-code eq "R"   and
                     INDEX("MOXY789",ITEM.mat-type) GT 0 and
                     item.stocked eq no   then po-ordl.cons-qty
                  else po-ordl.t-rec-qty.

          if po-ordl.cons-uom eq ap-invl.cons-uom:SCREEN-VALUE then
                 ap-invl.qty:SCREEN-VALUE = string(v-qty - po-ordl.t-inv-qty).
          
          else do:
            run sys/ref/convquom.p (po-ordl.cons-uom, ap-invl.cons-uom:SCREEN-VALUE ,
                                   v-bwt, v-len, v-wid, v-dep,
                                   v-qty, output v-qty).
                                   
            ap-invl.qty:SCREEN-VALUE = string(v-qty - po-ordl.t-inv-qty).
          end.
     end.          
     ELSE do:
          if ap-invl.cons-uom:SCREEN-VALUE eq "EA" then
            ap-invl.qty:SCREEN-VALUE = string(po-ordl.t-rec-qty - po-ordl.t-inv-qty).
            
          else do:
            run sys/ref/convquom.p("EA", ap-invl.cons-uom:SCREEN-VALUE,
                                   0, 0, 0, 0,
                                   po-ordl.t-rec-qty, output v-qty).
                                   
            ap-invl.qty:SCREEN-VALUE = string(v-qty - po-ordl.t-inv-qty).
          end.  
      end.
      /*RUN ap/d-selrec.p (RECID(ap-invl), OUTPUT lv-invl-qty).*/

     /* gdm - 05290903  
      RUN ap/d-selrec.w (ROWID(ap-inv), lv-pol-rowid, OUTPUT lv-invl-qty).
      gdm - 05290903 end */
    
  /*    FIND CURRENT ap-invl NO-LOCK NO-ERROR.  */
   
      if ap-invl.pr-qty-uom:SCREEN-VALUE eq ap-invl.cons-uom:SCREEN-VALUE
          THEN v-temp-pr = dec(ap-invl.unit-pr:SCREEN-VALUE).          
      else
          run sys/ref/convcuom.p (ap-invl.pr-qty-uom:SCREEN-VALUE, ap-invl.cons-uom:SCREEN-VALUE,
                                  v-bwt, v-len, v-wid, v-dep,
                                  ap-invl.unit-pr:SCREEN-VALUE, output v-temp-pr).

        assign
         ap-invl.qty:SCREEN-VALUE = STRING(lv-invl-qty)
         ap-invl.amt:SCREEN-VALUE = string(lv-invl-qty * v-temp-pr)
         /*ap-inv.net  = ap-inv.net + dec(ap-invl.amt:SCREEN-VALUE)*/.

        if v-len eq 0 then v-len = 12.
        if v-wid eq 0 then v-wid = 12.
        
        ap-invl.sf-sht:SCREEN-VALUE = if v-corr then STRING(v-len * v-wid * .007)
                                      else STRING(v-len * v-wid / 144).

        if not avail item             and
           (v-len eq 0 or v-wid eq 0) then do:
          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq po-ordl.i-no
                and NOT po-ordl.item-type
              no-lock no-error.
          if avail itemfg then ap-invl.sf-sht:SCREEN-VALUE = string(itemfg.t-sqft).
        end.

        if ap-invl.cons-uom:SCREEN-VALUE eq "EA" THEN v-qty = dec(ap-invl.qty:SCREEN-VALUE).          
        else
          run sys/ref/convquom.p(ap-invl.cons-uom:SCREEN-VALUE, "EA",
                                 v-bwt, v-len, v-wid, v-dep,
                                 int(ap-invl.qty:SCREEN-VALUE), output v-qty).

        ap-invl.amt-msf:SCREEN-VALUE = string(dec(ap-invl.sf-sht:SCREEN-VALUE) * v-qty / 1000).
        
  END.  /* avail po-ordl */

  SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-uom-list B-table-Win 
PROCEDURE get-uom-list :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-uom-list AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    RELEASE item.
    RELEASE itemfg.
    RELEASE po-ordl.

    IF INT(ap-invl.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN
    FIND FIRST po-ordl
        WHERE po-ordl.company EQ g_company
          AND po-ordl.po-no   EQ INT(ap-invl.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
          AND po-ordl.i-no    EQ lv-item-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND po-ordl.line    EQ ap-invl.line - (ap-invl.po-no * 1000)
        NO-LOCK NO-ERROR.

    IF NOT AVAIL po-ordl OR po-ordl.item-type THEN
    FIND FIRST item
        WHERE item.company EQ g_company
          AND item.i-no    EQ lv-item-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND item.i-no    NE ""
        NO-LOCK NO-ERROR.

    IF NOT AVAIL item THEN
    FIND FIRST itemfg
        WHERE itemfg.company EQ g_company
          AND itemfg.i-no    EQ lv-item-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND itemfg.i-no    NE ""
        NO-LOCK NO-ERROR.

    IF AVAIL item THEN
      RUN sys/ref/uom-rm.p (item.mat-type, OUTPUT op-uom-list).
    ELSE
    IF AVAIL itemfg THEN
      RUN sys/ref/uom-fg.p (NO, OUTPUT op-uom-list). /* for fgitem */

    IF op-uom-list EQ "" THEN op-uom-list = lv-uom-list.

    IF AVAIL item AND INDEX("MOXY789@",ITEM.mat-type) GT 0 THEN
      op-uom-list = op-uom-list + ",L".
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
  DEF VAR v-msg AS cha NO-UNDO.
  DEF VAR v-is-in-update AS LOG NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  IF ap-inv.posted THEN do:
    MESSAGE "This invoice has been posted, no addings are allowed!"  VIEW-AS ALERT-BOX ERROR.
    RETURN.
  END.

  IF apsecure-log AND ap-inv.user-id NE USERID("nosweat") THEN DO:
    MESSAGE "This invoice may only be added to by UserID: " +
            TRIM(ap-inv.user-id) + "..."
         VIEW-AS ALERT-BOX ERROR.
    RETURN.
  END.
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"clear-source",OUTPUT char-hdl).
  RUN is-in-update IN WIDGET-HANDLE(char-hdl) (OUTPUT v-is-in-update).
  IF v-is-in-update THEN v-msg = "Save or Cancel Invoice Heading Update First." .
  IF v-msg NE "" THEN DO:
    MESSAGE v-msg VIEW-AS ALERT-BOX ERROR.
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source", OUTPUT char-hdl).
    RUN cancel-proc IN WIDGET-HANDLE(char-hdl).
    RETURN error.
  END.

 
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ll-added = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ld-tmp-amt LIKE ap-invl.amt NO-UNDO.
  DEF VAR ld-tmp-msf LIKE ap-invl.amt-msf NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN
   ld-tmp-amt = DEC(ap-invl.amt:SCREEN-VALUE IN BROWSE {&browse-name})
   ld-tmp-msf = DEC(ap-invl.amt-msf:SCREEN-VALUE IN BROWSE {&browse-name}).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ).

  /* Code placed here will execute AFTER standard behavior.    */
  FIND CURRENT ap-invl.

  IF adm-adding-record AND ap-invl.po-no NE 0 THEN RUN assign-ap-from-po.

  IF AVAIL ap-invl THEN 
    ASSIGN
     ap-invl.amt     = ld-tmp-amt
     ap-invl.amt-msf = ld-tmp-msf.

  RUN update-header (ROWID(ap-inv), NO).

  /* enable browser's  key strokes */
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-source",OUTPUT char-hdl).
  RUN SET-ATTRIBUTE-list IN WIDGET-HANDLE(char-hdl) ("NEW-AP=?").

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
  
  ASSIGN ap-invl.amt = DEC(ap-invl.amt:SCREEN-VALUE IN BROWSE {&browse-name})
         ap-invl.amt-msf = DEC(ap-invl.amt-msf:SCREEN-VALUE).

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
  RUN auto-add-tt.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR z AS INT NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  FIND LAST bf-invl
      WHERE bf-invl.i-no  EQ ap-inv.i-no
        AND bf-invl.po-no EQ 0
      USE-INDEX i-no NO-ERROR.

  FIND FIRST vend
      WHERE vend.company EQ ap-inv.company
        AND vend.vend-no EQ ap-inv.vend-no
      NO-LOCK NO-ERROR.

  ASSIGN
   z          = (IF AVAIL bf-invl THEN bf-invl.line ELSE 0) + 1
   v-vend-act = IF AVAIL vend THEN vend.actnum ELSE "".  

  IF v-vend-act EQ "" THEN
     v-vend-act = v-ap-pur.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST tt-ap-invl WHERE tt-ap-invl.i-no EQ ap-inv.i-no NO-ERROR.

  IF AVAIL tt-ap-invl THEN DO:
    BUFFER-COPY tt-ap-invl EXCEPT rec_key TO ap-invl.
    tt-ap-invl.tt-rowid = ROWID(ap-invl).
  END.

  ELSE DO:
    ASSIGN
     ap-invl.i-no       = ap-inv.i-no
     ap-invl.actnum     = v-vend-act
     ap-invl.company    = ap-inv.company
     ap-invl.vend-no    = ap-inv.vend-no
     ap-invl.line       = z
     ap-invl.loc        = ap-inv.loc
     ap-invl.period     = ap-inv.period
     ap-invl.posted     = ap-inv.posted
     ap-invl.cons-uom   = "EA"
     ap-invl.pr-qty-uom = "EA"
     ap-invl.tax        = ap-inv.tax-gr NE "".

    IF apdesc-log THEN DO:
      FIND LAST bf-invl
          WHERE bf-invl.i-no   EQ ap-inv.i-no
            AND bf-invl.po-no  EQ 0
            AND ROWID(bf-invl) NE ROWID(ap-invl)
          USE-INDEX i-no NO-ERROR.
       IF AVAIL bf-invl THEN ap-invl.dscr = bf-invl.dscr.
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
 DEF VAR lv-invamt LIKE ap-invl.amt NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  IF ap-inv.posted THEN do:
    MESSAGE "This invoice has been posted, no deletion allowed!"  VIEW-AS ALERT-BOX ERROR.
    RETURN.
  END.

  IF apsecure-log AND ap-inv.user-id NE USERID("nosweat") THEN DO:
    MESSAGE "This invoice may only be deleted by UserID: " +
            TRIM(ap-inv.user-id) + "..."
         VIEW-AS ALERT-BOX ERROR.
    RETURN.
  END.

  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.

  lv-invamt = ap-invl.amt.
  
  find first po-ordl where po-ordl.company   eq ap-inv.company
             and po-ordl.po-no     eq ap-invl.po-no
             and po-ordl.line      eq {ap/invlline.i -1}
             no-lock no-error. 
   if avail po-ordl THEN do: 
      for each fg-rcpth where fg-rcpth.company   eq ap-inv.company
               and fg-rcpth.i-no      eq po-ordl.i-no
               and fg-rcpth.po-no     eq trim(string(po-ordl.po-no,">>>>>>>>>>"))
               and fg-rcpth.rita-code eq "R"
               and fg-rcpth.b-no      eq ap-invl.i-no
               use-index item-po:

           fg-rcpth.b-no = 0.
       end.
       
       FOR EACH rm-rcpth NO-LOCK
           WHERE rm-rcpth.company EQ ap-inv.company
             AND rm-rcpth.i-no    EQ po-ordl.i-no
             AND rm-rcpth.po-no   EQ trim(string(po-ordl.po-no,">>>>>>>>>>"))
             AND rm-rcpth.rita-code EQ "R" ,
           EACH rm-rdtlh OF rm-rcpth EXCLUSIVE-LOCK :
         ASSIGN rm-rdtlh.receiver-no = "" .
      END.
   END.

  RUN delete-tt.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN update-header (ROWID(ap-inv), NO).

  RUN reopen-header.

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
     NOT AVAIL {&FIRST-TABLE-IN-QUERY-{&browse-name}}  THEN
    RETURN "ADM-ERROR".

  RUN proc-control.
  li-line-num = 0.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY "entry" TO BROWSE {&browse-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR v-msg AS CHAR NO-UNDO.
  DEF VAR v-is-in-update AS LOG NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  v-msg = "".

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"clear-source",OUTPUT char-hdl).
  RUN is-in-update IN WIDGET-HANDLE(char-hdl) (OUTPUT v-is-in-update).
  IF v-is-in-update THEN v-msg = "Save or Cancel Invoice Heading Update First." .

  IF v-msg EQ "" AND ap-inv.posted THEN
    v-msg = "This invoice has been posted, no changes are allowed!".
  
  IF v-msg EQ "" AND apsecure-log AND ap-inv.user-id NE USERID("nosweat") THEN
    v-msg = "This invoice may only be updated by UserID: " +
            TRIM(ap-inv.user-id) + "...".

  IF v-msg NE "" THEN DO:
    MESSAGE v-msg VIEW-AS ALERT-BOX ERROR.
  /*  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source", OUTPUT char-hdl).
    RUN apply-cancel IN WIDGET-HANDLE(char-hdl).
    RUN cancel-proc IN WIDGET-HANDLE(char-hdl).
  */  
    RETURN error.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  lv-pol-rowid = ?.

  DO WITH FRAME {&FRAME-NAME}:
    APPLY "entry" TO ap-invl.po-no IN BROWSE {&browse-name}.
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
  FOR EACH tt-ap-invl WHERE tt-ap-invl.i-no NE ap-inv.i-no:
    DELETE tt-ap-invl.
  END.

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
  DEF VAR lv-rowid AS ROWID NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  /* === validation ---- */
  RUN valid-po-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
  DO WITH FRAME {&FRAME-NAME}:
    IF DEC(ap-invl.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 AND
       ap-invl.actnum:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN DO:
      FIND FIRST vend WHERE vend.company = ap-inv.company
                        AND vend.vend-no = ap-inv.vend-no NO-LOCK NO-ERROR.
      v-vend-act = IF AVAIL vend THEN vend.actnum ELSE "".
      IF v-vend-act EQ "" THEN
         v-vend-act = v-ap-pur.
      
      ap-invl.actnum:SCREEN-VALUE = v-vend-act.
    END.
  END.

  IF NOT CAN-FIND(FIRST tt-ap-invl WHERE tt-rowid EQ ROWID(ap-invl))
  THEN RUN update-ttt.

  RUN valid-actnum NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-qty NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-unit-pr NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-pr-qty-uom NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN delete-tt.

  ASSIGN
   adm-new-record    = NO
   adm-adding-record = NO
   lv-rowid          = ROWID(ap-invl).

  IF CAN-FIND(FIRST tt-ap-invl) THEN
  FOR EACH tt-ap-invl:
    CREATE bf-invl.
    BUFFER-COPY tt-ap-invl EXCEPT rec_key TO bf-invl.
    FIND CURRENT bf-invl NO-LOCK.
    DELETE tt-ap-invl.
    RUN update-header (ROWID(ap-inv), NO).
  END.

  RUN reopen-header.

  RUN repo-query (lv-rowid).

  RUN auto-add-tt.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE po-adder2 B-table-Win 
PROCEDURE po-adder2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ip-recid  as recid.
DEF INPUT PARAM ip-recid1 as recid.
DEF INPUT PARAM ip-vend-no LIKE po-ord.vend-no NO-UNDO.
DEF INPUT PARAM ip-qty as DEC NO-UNDO.
DEF INPUT PARAM ip-cost as DEC NO-UNDO.
DEF INPUT PARAM ip-cons-cost as DEC NO-UNDO.

DEF OUTPUT PARAM op-cost AS DEC NO-UNDO.
DEF OUTPUT PARAM op-cons-cost AS DEC NO-UNDO.
DEF OUTPUT PARAM op-adder-setup AS DEC NO-UNDO.

def var v-tot-cost as dec no-undo.
def var v-cost     as dec no-undo.
def var v-add-cost as dec no-undo.
def var v-qty-comp as dec no-undo.
def var v-setup like e-item-vend.setup no-undo.
def var v-adder as dec extent 2 NO-UNDO.
DEF VAR v-index AS INT NO-UNDO.

def buffer xjob-mat for job-mat.

find xjob-mat where recid(xjob-mat) eq ip-recid1 no-lock.

assign
 v-adder[1] = ip-cost
 v-adder[2] = ip-cons-cost.

DO WITH FRAME {&FRAME-NAME}:
  IF ap-invl.pr-qty-uom EQ "EA" THEN
     v-tot-cost = ip-cost.
  ELSE
    RUN sys/ref/convcuom.p(ap-invl.pr-qty-uom, "EA",
                           v-basis-w, v-len, v-wid, v-dep,
                           ip-cost, OUTPUT v-tot-cost).
 
  for each job-mat no-lock
      where job-mat.company  eq xjob-mat.company
        and job-mat.job      eq xjob-mat.job
        and job-mat.frm      eq xjob-mat.frm
        and job-mat.job-no   eq xjob-mat.job-no
        and job-mat.job-no2  eq xjob-mat.job-no2
      use-index seq-idx,

      first item no-lock
      where item.company  eq job-mat.company
        and item.i-no     eq job-mat.i-no
        and item.mat-type eq "A":

    find first e-item no-lock
        where e-item.company eq po-ordl.company
          and e-item.i-no    eq po-ordl.i-no
        no-error.
    
    find first e-item-vend no-lock
        where e-item-vend.company eq item.company
          and e-item-vend.i-no    eq item.i-no
          and e-item-vend.vend-no eq ip-vend-no
        no-error.

    if avail e-item and avail e-item-vend AND ip-vend-no NE "" then do:
      if ap-invl.cons-uom eq e-item.std-uom then
           v-qty-comp = ip-qty.
      else
        run sys/ref/convquom.p(ap-invl.cons-uom, e-item.std-uom,
                               v-basis-w, v-len, v-wid, v-dep,
                               ip-qty, output v-qty-comp).

      ASSIGN
         v-setup = 0
         v-index = 0.
      do i = 1 to 10:
         if v-qty-comp le e-item-vend.run-qty[i] then
         DO:
            v-index = i.
            LEAVE.
         END.
      end.
    
      IF v-index EQ 0 THEN
      DO:
                 
         DO i = 1 TO 10:
            IF v-qty-comp LE e-item-vend.runQtyXtra[i] THEN
            DO:
               v-index = i + 10.
               LEAVE.
            END.
         END.
      END.

      IF v-index EQ 0 THEN
         v-index = 10.

      IF v-index LE 10 THEN
         ASSIGN
            v-setup = e-item-vend.setups[i]
            op-adder-setup = op-adder-setup + v-setup
            v-cost = ((e-item-vend.run-cost[i] * v-qty-comp) + v-setup) / v-qty-comp.
      ELSE
      DO:


         IF AVAIL e-item-vend THEN
            ASSIGN
               v-setup = e-item-vend.setupsXtra[v-index - 10]
               op-adder-setup = op-adder-setup + v-setup
               v-cost = ((e-item-vend.runCostXtra[v-index - 10] * v-qty-comp) + v-setup) / v-qty-comp.
      END.
      
      /* This adds the Adder cost in */
      IF e-item.std-uom NE ap-invl.pr-qty-uom THEN
        RUN sys/ref/convcuom.p(e-item.std-uom, ap-invl.pr-qty-uom, job-mat.basis-w,
                               job-mat.len, job-mat.wid, item.s-dep,
                               v-cost, OUTPUT v-cost).
    END.

    ELSE DO:
      v-cost = job-mat.std-cost.
      
      IF job-mat.sc-uom NE ap-invl.pr-qty-uom THEN
        RUN sys/ref/convcuom.p(job-mat.sc-uom, ap-invl.pr-qty-uom, job-mat.basis-w,
                               job-mat.len, job-mat.wid, item.s-dep,
                               job-mat.std-cost, OUTPUT v-cost).
    END.

    v-add-cost = v-add-cost + v-cost.
  END.

  IF ap-invl.pr-qty-uom NE "EA" THEN 
    RUN sys/ref/convcuom.p("EA", ap-invl.pr-qty-uom,
                           v-basis-w, v-len, v-wid, v-dep,
                           v-tot-cost, OUTPUT v-tot-cost).
 
  op-cost = v-add-cost + v-tot-cost.

  IF ap-invl.pr-qty-uom NE ap-invl.cons-uom THEN
    RUN sys/ref/convcuom.p(ap-invl.pr-qty-uom, ap-invl.cons-uom,
                           v-basis-w, v-len, v-wid, v-dep,
                           ip-cost, OUTPUT op-cons-cost).

assign
 v-adder[1] = op-cost      - v-adder[1]
 v-adder[2] = op-cons-cost - v-adder[2].
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-control B-table-Win 
PROCEDURE proc-control :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recalc-board B-table-Win 
PROCEDURE recalc-board :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-tot-rec-qty LIKE rm-rdtlh.qty NO-UNDO.
  DEF VAR v-qty LIKE rm-rdtlh.qty NO-UNDO.
  DEF VAR v-len like po-ordl.s-len NO-UNDO.
  DEF VAR v-wid like po-ordl.s-wid NO-UNDO.
  DEF VAR lv-item-no AS CHAR NO-UNDO.
  DEF VAR lv-uom AS CHAR NO-UNDO.
  DEF VAR v-old-qty LIKE lv-tot-rec-qty NO-UNDO.

  DEF BUFFER b-po-ordl FOR po-ordl.

  FOR EACH ap-invl WHERE ap-invl.i-no = ap-inv.i-no AND
    ap-invl.po-no NE 0
    EXCLUSIVE-LOCK,
    FIRST po-ordl WHERE po-ordl.company = g_company AND
      po-ordl.po-no = ap-invl.po-no AND
      po-ordl.LINE = {ap/invlline.i -1} AND
      po-ordl.item-type
      NO-LOCK,
    FIRST po-ord WHERE
      po-ord.company = cocode AND
      po-ord.po-no = ap-invl.po-no
      NO-LOCK:

    ASSIGN
      lv-item-no = display-item-no()
      lv-uom = ap-invl.cons-uom.

    FIND FIRST ITEM WHERE
      ITEM.company = cocode AND
      ITEM.i-no = lv-item-no AND
      ITEM.mat-type = "B"
      NO-LOCK NO-ERROR.
  
      ASSIGN lv-tot-rec-qty = 0
             v-len = 0
             v-wid = 0.

      FOR EACH rm-rcpth WHERE
        rm-rcpth.company = cocode AND
        rm-rcpth.po-no = STRING(ap-invl.po-no) AND
        rm-rcpth.i-no = lv-item-no AND
        rm-rcpth.job-no EQ po-ordl.job-no AND
        rm-rcpth.job-no2 EQ po-ordl.job-no2 AND
        rm-rcpth.rita-code = "R"
        NO-LOCK,
        FIRST rm-rdtlh OF rm-rcpth NO-LOCK:

        ASSIGN
          v-len = item.s-len
          v-wid = if item.r-wid gt 0 then item.r-wid else item.s-wid.
        
        for each b-po-ordl
            where b-po-ordl.company eq cocode
               and b-po-ordl.po-no   eq po-ord.po-no
               and b-po-ordl.i-no    eq rm-rcpth.i-no
               and b-po-ordl.job-no  eq rm-rcpth.job-no
               and b-po-ordl.job-no2 eq rm-rcpth.job-no2
             no-lock
             by b-po-ordl.s-num desc:
             
           assign
            v-len = b-po-ordl.s-len
            v-wid = b-po-ordl.s-wid.
          
           if b-po-ordl.s-num eq rm-rdtlh.s-num then leave.
        end.
      
        if rm-rcpth.job-no ne "" then
          for each job-mat
              where job-mat.company eq cocode
                and job-mat.rm-i-no eq item.i-no
                and job-mat.job-no  eq rm-rcpth.job-no
                and job-mat.job-no2 eq rm-rcpth.job-no2
              no-lock
              by job-mat.frm desc:
              
            assign
             v-len = job-mat.len
             v-wid = job-mat.wid.
          
            if job-mat.frm eq rm-rdtlh.s-num then leave.  
          end.
      
        if v-len eq 0 then v-len = 12.
        if v-wid eq 0 then v-wid = 12.

        v-qty = rm-rdtlh.qty.

        IF rm-rcpth.pur-uom NE lv-uom THEN
          RUN sys/ref/convquom.p (rm-rcpth.pur-uom, lv-uom,
                                item.basis-w, v-len, v-wid, item.s-dep,
                                v-qty, OUTPUT v-qty).

        {sys/inc/roundup.i v-qty}
        

        lv-tot-rec-qty = lv-tot-rec-qty + v-qty.
      END.

      IF lv-tot-rec-qty NE 0 THEN
      DO:
         ASSIGN
            v-old-qty = ap-invl.qty
            ap-invl.qty = lv-tot-rec-qty.
         RUN vend-cost.
         RUN calc-amt3.
         ap-invl.qty = v-old-qty.
      END.
  END.

  IF NOT CAN-FIND(FIRST tt-ap-invl WHERE tt-rowid EQ ROWID(ap-invl)) THEN
     RUN update-ttt.

  RUN delete-tt.

  RUN update-header (ROWID(ap-inv), NO).

  RUN reopen-header.

  RUN reopen-query.

  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-header B-table-Win 
PROCEDURE reopen-header :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source", OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
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
  
  RUN reopen-header.

  RUN dispatch ("open-query").

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
    RUN dispatch ("open-query").
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
    RUN dispatch ("row-changed").
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
  {src/adm/template/snd-list.i "ap-inv"}
  {src/adm/template/snd-list.i "ap-invl"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-dims B-table-Win 
PROCEDURE set-dims :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     v-len = po-ordl.s-len
     v-wid = po-ordl.s-wid
     {po/calc10.i v-len}
     {po/calc10.i v-wid}.

    FIND FIRST ITEM
        WHERE item.company EQ cocode
          AND item.i-no    EQ po-ordl.i-no
        NO-LOCK NO-ERROR.

    ASSIGN
     v-basis-w = IF AVAIL ITEM THEN item.basis-w ELSE 0
     v-dep     = IF AVAIL ITEM THEN item.s-dep ELSE 0.
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
  DEF OUTPUT PARAM op-delete-choice AS LOG INIT YES NO-UNDO.

  IF adm-new-record THEN RUN dispatch ("cancel-record").

  IF AVAIL ap-inv AND
     NOT CAN-FIND(FIRST ap-invl WHERE ap-invl.i-no EQ ap-inv.i-no) THEN DO:
     RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"clear-source", OUTPUT char-hdl).
     IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
     DO:
        RUN dispatch IN WIDGET-HANDLE(char-hdl) ("delete-record").
        RUN delete-choice IN WIDGET-HANDLE(char-hdl) (OUTPUT op-delete-choice).
     END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-header B-table-Win 
PROCEDURE update-header :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
  DEF INPUT PARAM ip-overwrite-tax AS LOG NO-UNDO.

  DEF VAR ld AS DEC NO-UNDO.
  DEF VAR v-tax-rate AS DEC NO-UNDO.
  DEF VAR v-frt-tax-rate AS DEC NO-UNDO.

  DEF BUFFER b-ap-inv  FOR ap-inv.
  DEF BUFFER b-ap-invl FOR ap-invl.


  FIND b-ap-inv WHERE ROWID(b-ap-inv) EQ ip-rowid EXCLUSIVE NO-ERROR.

  IF AVAIL b-ap-inv THEN DO:

    IF NOT ip-overwrite-tax THEN
       b-ap-inv.tax-amt = 0.

    ASSIGN
     b-ap-inv.net     = 0
     b-ap-inv.freight = 0.

    IF b-ap-inv.tax-gr NE "" THEN
      RUN ar/cctaxrt.p (b-ap-inv.company, b-ap-inv.tax-gr,
                        OUTPUT v-tax-rate, OUTPUT v-frt-tax-rate).

    FOR EACH b-ap-invl WHERE b-ap-invl.i-no EQ b-ap-inv.i-no NO-LOCK:
      b-ap-inv.net = b-ap-inv.net + b-ap-invl.amt.

      IF b-ap-invl.tax AND NOT ip-overwrite-tax THEN
        b-ap-inv.tax-amt = b-ap-inv.tax-amt +
                           ROUND((b-ap-invl.amt * v-tax-rate / 100),2).

      IF b-ap-invl.po-no NE 0 THEN DO:
        FIND FIRST po-ordl
            WHERE po-ordl.company EQ cocode
              AND po-ordl.po-no   EQ (IF b-ap-invl.po-no EQ 0 THEN b-ap-inv.po-no
                                                              ELSE b-ap-invl.po-no)
              AND po-ordl.line    EQ (b-ap-invl.line + (b-ap-invl.po-no * 1000 * -1)) 
            USE-INDEX po-no NO-ERROR.

        IF AVAIL po-ordl THEN DO:
          RUN po/getfrtcs.p (ROWID(po-ordl), b-ap-invl.qty, OUTPUT ld).
          b-ap-inv.freight = b-ap-inv.freight + ld.
        END.
      END.
    END.

    ASSIGN
     b-ap-inv.tax-amt = b-ap-inv.tax-amt +
                        ROUND((b-ap-inv.freight * v-frt-tax-rate / 100),2)
     b-ap-inv.net     = b-ap-inv.net + b-ap-inv.tax-amt
     b-ap-inv.due     = b-ap-inv.net - b-ap-inv.disc-taken -
                        b-ap-inv.paid + b-ap-inv.freight.
  END.

  FIND CURRENT b-ap-inv NO-LOCK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-receipts B-table-Win 
PROCEDURE update-receipts :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF AVAIL ap-invl THEN RUN ap/d-selrec.w (RECID(ap-invl), OUTPUT lv-invl-qty).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-tt B-table-Win 
PROCEDURE update-tt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/ 

  RUN get-link-handle IN adm-broker-hdl
                       (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
  phandle = WIDGET-HANDLE(char-hdl).
  RUN auto-save IN phandle.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-ttt B-table-Win 
PROCEDURE update-ttt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
   FIND FIRST tt-ap-invl WHERE tt-ap-invl.i-no EQ ap-inv.i-no NO-ERROR.

   IF AVAIL tt-ap-invl THEN DO:
     FIND CURRENT ap-invl.
     BUFFER-COPY tt-ap-invl EXCEPT rec_key TO ap-invl.
     tt-rowid = ROWID(ap-invl).
     FIND CURRENT ap-invl NO-LOCK.
     RUN dispatch ("display-fields").
   END.
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-actnum B-table-Win 
PROCEDURE valid-actnum :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST account
        WHERE account.company EQ g_company
          AND account.actnum  EQ ap-invl.actnum:SCREEN-VALUE IN BROWSE {&browse-name}
          AND account.TYPE    NE "T"          
        NO-LOCK NO-ERROR.
    IF NOT AVAIL account THEN DO:
      MESSAGE "Invalid Account Number..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ap-invl.actnum IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
    v-actdscr:SCREEN-VALUE IN BROWSE {&browse-name} = account.dscr.
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
  DEF VAR lv-msg AS CHAR NO-UNDO.
  DEF BUFFER b-ap-invl FOR ap-invl.

  DO WITH FRAME {&FRAME-NAME}:
    IF INT(ap-invl.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0              AND
       NOT CAN-FIND(FIRST tt-ap-invl WHERE tt-ap-invl.tt-rowid EQ ROWID(ap-invl)) THEN DO:

      FIND FIRST po-ord
          WHERE po-ord.company EQ g_company
            AND po-ord.vend-no EQ ap-inv.vend-no
            AND po-ord.po-no   EQ INT(ap-invl.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
          USE-INDEX vend-no NO-LOCK NO-ERROR.
      IF NOT AVAIL po-ord THEN lv-msg = "Invalid PO, try help".

      IF lv-msg EQ "" THEN DO:

        FIND FIRST b-ap-invl WHERE
             ROWID(b-ap-invl) EQ ROWID(ap-invl)
             NO-LOCK NO-ERROR.

        FOR EACH po-ordl NO-LOCK
            WHERE po-ordl.company EQ po-ord.company
              AND po-ordl.po-no   EQ po-ord.po-no:

          RUN ap/valid-po2.p (BUFFER po-ordl, BUFFER b-ap-invl).
          IF AVAIL po-ordl THEN LEAVE.
        END.
        IF NOT AVAIL po-ordl THEN lv-msg = "No Receipts exist for this PO".
      END.

      IF lv-msg EQ ""                                                        AND
         adm-adding-record                                                   AND
         NOT CAN-FIND(FIRST tt-ap-invl WHERE tt-ap-invl.i-no EQ ap-inv.i-no) THEN DO:

        RUN build-table (RECID(po-ord)).

        IF NOT apinvmsg-log AND lv-num-rec LE 0 THEN
          lv-msg = "All receipts for this PO have been invoiced".

        ELSE DO:
          RUN ap/d-selpos.w (RECID(ap-inv)).

          IF CAN-FIND(FIRST tt-pol
                      WHERE tt-pol.selekt
                        AND tt-pol.qty-to-inv NE 0) THEN
            RUN create-ap-from-po.

          ELSE
            lv-msg = "Nothing selected for this PO".
        END.
      END.
    END.

    IF lv-msg NE "" THEN DO:
      MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ap-invl.po-no.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-pr-qty-uom B-table-Win 
PROCEDURE valid-pr-qty-uom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR uom-list AS CHAR INIT "" NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    IF ap-invl.pr-qty-uom:SCREEN-VALUE IN BROWSE {&browse-name} NE "0" THEN DO:
      RUN get-uom-list (OUTPUT uom-list).

      FIND FIRST uom
          WHERE uom.uom EQ ap-invl.pr-qty-uom:SCREEN-VALUE
            AND LOOKUP(uom.uom,uom-list) GT 0
          NO-LOCK NO-ERROR.
      IF NOT AVAIL uom THEN DO:
        MESSAGE "Invalid Unit Of Measure..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO ap-invl.pr-qty-uom IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-qty B-table-Win 
PROCEDURE valid-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF DEC(ap-invl.qty:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN DO:
      MESSAGE "The QUANTITY you enter must be greater than 0, please re-enter..."
              VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ap-invl.qty IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-unit-pr B-table-Win 
PROCEDURE valid-unit-pr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF DEC(ap-invl.unit-pr:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN DO:
      MESSAGE "The UNIT PRICE you enter must be greater than 0, please re-enter..."
              VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ap-invl.unit-pr IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vend-cost B-table-Win 
PROCEDURE vend-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  

  /*from create po line item program (d-poordl.w) */
  DEF VAR v-qty  AS DEC NO-UNDO.
  DEF VAR v-cost AS DEC NO-UNDO.
  DEF VAR v-pb-qty AS DEC NO-UNDO.
  DEF VAR v-pb-stp AS DEC NO-UNDO.
  DEF VAR v-pb-cst AS DEC NO-UNDO.
  DEF VAR v-pb-cns AS DEC NO-UNDO.
  DEF VAR v-save-qty AS DEC NO-UNDO.
  DEF VAR v-setup AS DEC NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-added-cost AS DEC NO-UNDO.
  DEF VAR lv-added-cons-cost AS DEC NO-UNDO.
  DEF VAR lv-adder-setup AS DEC NO-UNDO.
  DEF VAR lv-recid AS RECID NO-UNDO.
  DEF VAR lv-t-cost AS DEC NO-UNDO.
  DEF VAR lv-cons-cost AS DEC NO-UNDO.
  DEF VAR v-ord-qty LIKE po-ordl.ord-qty NO-UNDO.
  DEF VAR lv-pb-qty LIKE po-ordl.ord-qty NO-UNDO.
  DEF VAR lv-final-cost LIKE po-ordl.cost NO-UNDO.
  DEF VAR lv-final-setup AS DEC NO-UNDO.
  DEF VAR ar-pr-qty-uom-cost AS DEC NO-UNDO.
  DEF VAR ld-dim-charge AS DEC NO-UNDO.
  DEF VAR v-index AS INT NO-UNDO.

  EMPTY TEMP-TABLE tt-ei.
  EMPTY TEMP-TABLE tt-eiv.

  DO WITH FRAME {&FRAME-NAME}:
    RUN set-dims.

    /* for adders */
    RELEASE job-mat.

    FIND FIRST job NO-LOCK
        WHERE job.company EQ po-ordl.company
          AND job.job-no  EQ po-ordl.job-no
          AND job.job-no2 EQ po-ordl.job-no2
        NO-ERROR.
    IF AVAIL job THEN
    FIND FIRST job-mat NO-LOCK
        WHERE job-mat.company  EQ job.company
          AND job-mat.job      EQ job.job
          AND job-mat.job-no   EQ job.job-no
          AND job-mat.job-no2  EQ job.job-no2
          AND job-mat.frm      EQ po-ordl.s-num
          AND job-mat.blank-no EQ po-ordl.b-num
        USE-INDEX seq-idx NO-ERROR.
        
    IF AVAIL job-mat THEN lv-recid = RECID(job-mat).

    FIND FIRST e-item NO-LOCK
        WHERE e-item.company EQ cocode
          AND e-item.i-no    EQ po-ordl.i-no
        NO-ERROR.

    IF AVAIL e-item THEN DO:
      CREATE tt-ei.
      BUFFER-COPY e-item TO tt-ei.

      FIND FIRST e-item-vend NO-LOCK
          WHERE e-item-vend.company EQ e-item.company
            AND e-item-vend.i-no    EQ e-item.i-no
            AND e-item-vend.vend-no EQ po-ord.vend-no
          NO-ERROR.

      IF AVAIL e-item-vend THEN DO:
        CREATE tt-eiv.
        tt-eiv.rec_key = e-item-vend.rec_key.


        DO v-index = 1 TO 20:

           IF v-index LE 10 THEN
              ASSIGN
                 tt-eiv.run-qty[v-index] = e-item-vend.run-qty[v-index]
                 tt-eiv.run-cost[v-index] = e-item-vend.run-cost[v-index]
                 tt-eiv.setups[v-index] = e-item-vend.setups[v-index].
           ELSE
              IF AVAIL e-item-vend THEN
                 ASSIGN
                 tt-eiv.run-qty[v-index] = e-item-vend.runQtyXtra[v-index - 10]
                 tt-eiv.run-cost[v-index] = e-item-vend.runCostXtra[v-index - 10]
                 tt-eiv.setups[v-index] = e-item-vend.setupsXtra[v-index - 10].
        END.
      END.
    END.

    IF AVAIL tt-eiv THEN DO:
      ASSIGN
       v-cost = po-ordl.cost
       v-qty  = ap-invl.qty.

      
      IF tt-ei.std-uom NE ap-invl.cons-uom THEN
         RUN sys/ref/convquom.p(ap-invl.cons-uom,
                               tt-ei.std-uom, v-basis-w,
                               v-len, v-wid, v-dep,
                               v-qty, OUTPUT v-qty).      

      v-save-qty = v-qty.

      ASSIGN
       v-save-qty = v-qty - v-save-qty
       v-setup    = 0
       v-pb-qty   = 0.
            
      RUN est/dim-charge.p (tt-eiv.rec_key,
                            v-wid,
                            v-len,
                            INPUT-OUTPUT ld-dim-charge).

      DO li = 1 TO 20:
        IF tt-eiv.run-qty[li] LT v-qty THEN NEXT.
        ASSIGN
         v-cost   = (tt-eiv.run-cost[li] + ld-dim-charge) * v-qty
         v-setup  = tt-eiv.setups[li]
         v-pb-qty = tt-eiv.run-qty[li] - v-save-qty.

        IF li LT 20 THEN
          ASSIGN
           v-pb-cst = tt-eiv.run-cost[li + 1] + ld-dim-charge
           v-pb-stp = tt-eiv.setups[li + 1].
        LEAVE.
      END.

      IF poqty-log THEN DO:
        IF v-pb-qty GE 9999999 THEN v-pb-qty = 0.

        IF v-pb-qty EQ 0 THEN v-pb-cst = 0.
        ELSE DO:
          v-pb-qty = v-pb-qty + .001.

          v-pb-cst = v-pb-cst * v-pb-qty.

          IF v-pb-qty NE 0 THEN v-pb-cst = v-pb-cst / v-pb-qty.  
          ELSE v-pb-cst = v-pb-cst.
        END.

        IF tt-ei.std-uom NE ap-invl.cons-uom THEN
          RUN sys/ref/convquom.p(tt-ei.std-uom,
                                 ap-invl.cons-uom,
                                 v-basis-w, v-len, v-wid, v-dep,
                                 v-pb-qty, OUTPUT v-pb-qty).

        IF tt-ei.std-uom NE ap-invl.pr-qty-uom  THEN
          RUN sys/ref/convcuom.p(tt-ei.std-uom,
                                 ap-invl.pr-qty-uom, v-basis-w,
                                 v-len, v-wid, v-dep,
                                 v-pb-cst, OUTPUT v-pb-cst).

        IF ap-invl.pr-qty-uom NE ap-invl.cons-uom THEN
          RUN sys/ref/convcuom.p(ap-invl.pr-qty-uom,
                                 ap-invl.cons-uom, v-basis-w,
                                 v-len, v-wid, v-dep,
                                 v-pb-cst, OUTPUT v-pb-cns).

        lv-pb-qty = IF v-pb-qty LE 0 THEN 0 ELSE v-pb-qty.
      END.
      
      IF v-qty <> 0 THEN v-cost = v-cost / v-qty.  
      
      IF tt-ei.std-uom NE ap-invl.pr-qty-uom THEN
        RUN sys/ref/convcuom.p(tt-ei.std-uom,
                               ap-invl.pr-qty-uom, v-basis-w,
                               v-len, v-wid, v-dep,
                               v-cost, OUTPUT v-cost).
      ASSIGN
        lv-final-cost = v-cost
        lv-final-setup = v-setup
        ar-pr-qty-uom-cost = v-cost.

      IF ap-invl.pr-qty-uom NE ap-invl.cons-uom THEN
        RUN sys/ref/convcuom.p(ap-invl.pr-qty-uom,
                               ap-invl.cons-uom, v-basis-w,
                               v-len, v-wid, v-dep,
                               v-cost, OUTPUT v-cost).
      lv-cons-cost = v-cost.
    END.

    IF AVAIL job-mat THEN DO:
      
      IF poqty-log THEN
        RUN po-adder2 (RECID(po-ordl), lv-recid, po-ord.vend-no,
                       lv-pb-qty,
                       v-pb-cst,
                       v-pb-cns,
                       OUTPUT v-pb-cst,
                       OUTPUT v-pb-cns,
                       OUTPUT lv-adder-setup).
      
        RUN po-adder2 (RECID(po-ordl), lv-recid, po-ord.vend-no,
                       ap-invl.qty,
                       ar-pr-qty-uom-cost,
                       lv-cons-cost,
                       OUTPUT lv-added-cost,
                       OUTPUT lv-added-cons-cost,
                       OUTPUT lv-adder-setup).

        lv-final-cost = lv-added-cost.
    END.

    IF poqty-log THEN DO:
      IF CAN-DO("L,LOT",ap-invl.pr-qty-uom) THEN
        lv-t-cost = (v-pb-cst + v-pb-stp) *
                    IF ap-invl.qty LT 0 THEN -1 ELSE 1.

      ELSE DO:
        v-ord-qty = lv-pb-qty.

        IF ap-invl.cons-uom NE ap-invl.pr-qty-uom THEN
   
          RUN sys/ref/convquom.p(ap-invl.cons-uom,
                                 ap-invl.pr-qty-uom,
                                 v-basis-w, v-len, v-wid, v-dep,
                                 v-ord-qty, OUTPUT v-ord-qty).
     
        lv-t-cost = (v-ord-qty * v-pb-cst) + v-pb-stp.
      END.

      IF po-ordl.disc NE 0 THEN
        lv-t-cost = lv-t-cost * (1 - (po-ordl.disc / 100)).
    END.

    v-ord-qty = ap-invl.qty.

    /*assuming getting setup from vendor cost file*/
    IF CAN-DO("L,LOT",ap-invl.pr-qty-uom) THEN
      lv-t-cost = (lv-final-cost + lv-final-setup) *
                  IF ap-invl.qty LT 0 THEN -1 ELSE 1.
    ELSE DO:
      
      IF ap-invl.cons-uom NE ap-invl.pr-qty-uom THEN
        RUN sys/ref/convquom.p(ap-invl.cons-uom,
                               ap-invl.pr-qty-uom,
                               v-basis-w, v-len, v-wid, v-dep,
                               v-ord-qty, OUTPUT v-ord-qty).
    
      lv-t-cost = (v-ord-qty * lv-final-cost) +
                  lv-final-setup.
    END.

    IF po-ordl.disc NE 0 THEN
       lv-t-cost = lv-t-cost * (1 - (po-ordl.disc / 100)).
    
    ap-invl.unit-pr = lv-t-cost / v-ord-qty.

  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-bnum B-table-Win 
FUNCTION display-bnum RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF AVAIL ap-invl THEN
     FIND FIRST po-ordl WHERE po-ordl.company = g_company
                          AND po-ordl.po-no = ap-invl.po-no
                          AND po-ordl.LINE = ap-invl.LINE - (ap-invl.po-no * 1000)
                          NO-LOCK NO-ERROR.
  IF AVAIL po-ordl THEN RETURN po-ordl.s-num.
  ELSE RETURN 0.   /* Function return value. */
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-item-no B-table-Win 
FUNCTION display-item-no RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF AVAIL ap-invl THEN
     FIND FIRST po-ordl WHERE po-ordl.company = g_company
                       AND po-ordl.po-no = ap-invl.po-no
                       AND po-ordl.LINE = ap-invl.LINE - (ap-invl.po-no * 1000)
                       NO-LOCK NO-ERROR.
 
  IF AVAIL po-ordl THEN RETURN po-ordl.i-no.
  ELSE RETURN "NO PO ITEM".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-job B-table-Win 
FUNCTION display-job RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF AVAIL ap-invl then
     FIND FIRST po-ordl WHERE po-ordl.company = g_company
                       AND po-ordl.po-no = ap-invl.po-no
                       AND po-ordl.LINE = ap-invl.LINE - (ap-invl.po-no * 1000)
                       NO-LOCK NO-ERROR.
  IF AVAIL po-ordl THEN RETURN po-ordl.job-no + "-" + STRING(po-ordl.job-no2,">9").
  ELSE RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-line B-table-Win 
FUNCTION display-line RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    /*
  RETURN {ap/invline.i -1} .   /* Function return value. */
  */
   IF AVAIL ap-invl THEN
      RETURN (ap-invl.line + (ap-invl.po-no * 1000 * -1)) .
   ELSE RETURN 0.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-seq B-table-Win 
FUNCTION display-seq RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  RETURN li-line-num .   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-snum B-table-Win 
FUNCTION display-snum RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF AVAIL ap-invl THEN
    FIND FIRST po-ordl WHERE po-ordl.company = g_company
                       AND po-ordl.po-no = ap-invl.po-no
                       AND po-ordl.LINE = ap-invl.LINE - (ap-invl.po-no * 1000)
                       NO-LOCK NO-ERROR.
  IF AVAIL po-ordl THEN RETURN po-ordl.s-num.
  ELSE RETURN 0.   /* Function return value. */

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
  IF AVAIL ap-invl THEN DO:
     FIND FIRST account WHERE account.company = g_company
                          AND account.actnum = ap-invl.actnum NO-LOCK NO-ERROR.
     IF AVAIL account THEN RETURN account.dscr.
     ELSE RETURN "".
  END.
  ELSE RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

