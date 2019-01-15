&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  rm\b-issued.w

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

&SCOPED-DEFINE yellowColumnsName b-issued
&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

&SCOPED-DEFINE BRWSDEFS rm-issued

DEF VAR char-val AS cha NO-UNDO.
DEF VAR ext-cost AS DECIMAL NO-UNDO.

DEF VAR ls-prev-po AS cha NO-UNDO.
DEF VAR hd-post AS WIDGET-HANDLE NO-UNDO.
DEF VAR hd-post-child AS WIDGET-HANDLE NO-UNDO.
DEF VAR ll-help-run AS LOG NO-UNDO.  /* set on browse help, reset row-entry */

DEF VAR lv-po-wid AS DEC FORMAT ">>>9.9999" NO-UNDO.
DEF VAR lv-po-len AS DEC FORMAT ">>,>>9.9999" NO-UNDO.
DEF VAR v-len LIKE lv-po-len NO-UNDO.
DEF VAR v-wid LIKE lv-po-len NO-UNDO.
DEF VAR lv-rmissue AS CHAR NO-UNDO.
DEF VAR v-avgcost AS LOG NO-UNDO.
DEF VAR lv-uom-list AS cha INIT ["EA,TON,MSF,MSH,LB,LF,DIA"] NO-UNDO.
DEF VAR jobreopn-log AS LOG NO-UNDO.
DEF VAR vlc-success AS CHAR INIT "" NO-UNDO.

DEF BUFFER xitem FOR ITEM.
DEF NEW SHARED TEMP-TABLE item-chg NO-UNDO
    FIELD i-no LIKE job-mat.i-no
    FIELD rec-id AS RECID.

DEF NEW SHARED VAR fil_id AS RECID NO-UNDO.
DEF VAR lv-job LIKE job.job NO-UNDO.
DEF NEW SHARED TEMP-TABLE tt-selected FIELD tt-rowid AS ROWID.

DEF VAR lv-i-no LIKE po-ordl.i-no NO-UNDO.
DEF VAR lv-line LIKE po-ordl.line NO-UNDO.
DEF VAR v-rmtags-log AS LOG NO-UNDO.
DEF VAR v-get-tandem-rec AS LOG NO-UNDO.
DEF VAR ll-is-copy-record AS LOG NO-UNDO.
DEF VAR lAllowRmAdd AS LOG NO-UNDO.
DEF VAR lcReturn   AS CHAR NO-UNDO.
DEF VAR llRecFound AS LOG  NO-UNDO.
DEFINE VARIABLE v-bin   AS CHARACTER     NO-UNDO.
DO TRANSACTION:
  {sys/inc/rmrecpt.i}
END.

RUN sys/ref/nk1look.p (cocode, "RMAllowAdd", "L", NO, NO, "", "", 
    OUTPUT lcReturn, OUTPUT llRecFound).

   lAllowRmAdd = LOGICAL(lcReturn) NO-ERROR.  

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name EQ "RMTAGS"
    NO-LOCK NO-ERROR.

IF NOT AVAIL sys-ctrl THEN
   DO TRANSACTION:
      CREATE sys-ctrl.
      ASSIGN
         sys-ctrl.company = cocode
         sys-ctrl.name    = "RMTAGS"
         sys-ctrl.descrip = "Number of RM Loadtags to Print & Create Wip Tags"
         sys-ctrl.char-fld = ""
         sys-ctrl.int-fld = 1
         sys-ctrl.log-fld = FALSE. /* true create wip/false do not */
   END.

ASSIGN v-rmtags-log = sys-ctrl.log-fld.

RELEASE sys-ctrl.

DEF VAR lv-job-no    LIKE rm-rctd.job-no NO-UNDO.
DEF VAR look-recid   AS RECID NO-UNDO.

DEF VAR v-number-rows-selected AS INT NO-UNDO.

{windows/l-jobmt3.i NEW}

DEF TEMP-TABLE tt-frm NO-UNDO 
    FIELD frm     LIKE job-mat.frm
    FIELD mrp     LIKE job-mat.qty
    FIELD qty     LIKE job-mat.qty
    FIELD qtypct  AS DEC
    INDEX frm frm.      

DEF TEMP-TABLE tt-tag NO-UNDO 
    FIELD tag-no  LIKE rm-rctd.tag
    FIELD qty LIKE rm-rctd.qty
    FIELD tt-rowid AS ROWID
    INDEX tt-rowid tt-rowid
    INDEX tag-no tag-no.

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
&Scoped-define INTERNAL-TABLES rm-rctd

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table rm-rctd.r-no rm-rctd.rct-date ~
rm-rctd.po-no rm-rctd.job-no rm-rctd.job-no2 rm-rctd.i-no rm-rctd.i-name ~
rm-rctd.s-num rm-rctd.b-num rm-rctd.loc rm-rctd.loc-bin rm-rctd.tag ~
rm-rctd.qty rm-rctd.pur-uom rm-rctd.cost rm-rctd.cost-uom ~
calc-ext-cost() @ ext-cost display-dimension('W') @ lv-po-wid ~
display-dimension('L') @ lv-po-len rm-rctd.user-id 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table rm-rctd.rct-date ~
rm-rctd.po-no rm-rctd.job-no rm-rctd.job-no2 rm-rctd.i-no rm-rctd.i-name ~
rm-rctd.s-num rm-rctd.b-num rm-rctd.loc rm-rctd.loc-bin rm-rctd.tag ~
rm-rctd.qty rm-rctd.pur-uom 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table rm-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table rm-rctd
&Scoped-define QUERY-STRING-Browser-Table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company = cocode and ~
rm-rctd.rita-code = "I" NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company = cocode and ~
rm-rctd.rita-code = "I" NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table rm-rctd
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table rm-rctd


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 RECT-5 browse-order ~
auto_find Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order fi_sortby auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calc-ext-cost B-table-Win 
FUNCTION calc-ext-cost RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-dimension B-table-Win 
FUNCTION display-dimension RETURNS DECIMAL
  ( INPUT ip-dim AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear" 
     SIZE 8 BY 1
     FONT 4.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 78 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 146 BY 17.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      rm-rctd SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      rm-rctd.r-no COLUMN-LABEL "Seq#" FORMAT ">>>>>>>9":U LABEL-BGCOLOR 14
      rm-rctd.rct-date COLUMN-LABEL "Issue Date" FORMAT "99/99/9999":U
            LABEL-BGCOLOR 14
      rm-rctd.po-no FORMAT "x(6)":U LABEL-BGCOLOR 14
      rm-rctd.job-no COLUMN-LABEL "Job#" FORMAT "x(6)":U LABEL-BGCOLOR 14
      rm-rctd.job-no2 FORMAT "99":U
      rm-rctd.i-no COLUMN-LABEL "Item" FORMAT "x(10)":U LABEL-BGCOLOR 14
      rm-rctd.i-name COLUMN-LABEL "Name/Desc" FORMAT "x(30)":U
            LABEL-BGCOLOR 14
      rm-rctd.s-num COLUMN-LABEL "S" FORMAT ">9":U
      rm-rctd.b-num COLUMN-LABEL "B" FORMAT ">9":U
      rm-rctd.loc COLUMN-LABEL "Whse" FORMAT "x(5)":U LABEL-BGCOLOR 14
      rm-rctd.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U LABEL-BGCOLOR 14
      rm-rctd.tag COLUMN-LABEL "Tag#" FORMAT "x(20)":U LABEL-BGCOLOR 14
      rm-rctd.qty COLUMN-LABEL "Qty" FORMAT "->>>>>>9.9<<<<<":U
            LABEL-BGCOLOR 14
      rm-rctd.pur-uom COLUMN-LABEL "PUOM" FORMAT "x(4)":U WIDTH 7
            LABEL-BGCOLOR 14
      rm-rctd.cost COLUMN-LABEL "Cost" FORMAT "->>>,>>>,>>9.99<<<<":U
            LABEL-BGCOLOR 14
      rm-rctd.cost-uom COLUMN-LABEL "CUOM" FORMAT "x(4)":U WIDTH 7
            LABEL-BGCOLOR 14
      calc-ext-cost() @ ext-cost COLUMN-LABEL "Ext.Amount" FORMAT "->>>,>>>,>>9.99<<<<":U
            COLUMN-BGCOLOR 14
      display-dimension('W') @ lv-po-wid COLUMN-LABEL "Width"
      display-dimension('L') @ lv-po-len COLUMN-LABEL "Length"
      rm-rctd.user-id COLUMN-LABEL "UserId" FORMAT "x(8)":U
  ENABLE
      rm-rctd.rct-date
      rm-rctd.po-no
      rm-rctd.job-no
      rm-rctd.job-no2
      rm-rctd.i-no
      rm-rctd.i-name
      rm-rctd.s-num
      rm-rctd.b-num
      rm-rctd.loc
      rm-rctd.loc-bin
      rm-rctd.tag
      rm-rctd.qty
      rm-rctd.pur-uom
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 144 BY 15.24
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 2 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 16.71 COL 7 HELP
          "Select Browser Sort Order" NO-LABEL
     fi_sortby AT ROW 16.71 COL 85 COLON-ALIGNED NO-LABEL
     auto_find AT ROW 16.71 COL 115 COLON-ALIGNED HELP
          "Enter Auto Find Value" NO-LABEL
     Btn_Clear_Find AT ROW 16.71 COL 138 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 16.71 COL 3
     RECT-4 AT ROW 16.48 COL 2
     RECT-5 AT ROW 1 COL 1
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
         HEIGHT             = 17.14
         WIDTH              = 146.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
{methods/template/browser.i}
{custom/yellowColumns.i}

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

ASSIGN 
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "asi.rm-rctd"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "rm-rctd.company = cocode and
rm-rctd.rita-code = ""I"""
     _FldNameList[1]   > asi.rm-rctd.r-no
"r-no" "Seq#" ? "integer" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.rm-rctd.rct-date
"rct-date" "Issue Date" ? "date" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > asi.rm-rctd.po-no
"po-no" ? "x(6)" "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > asi.rm-rctd.job-no
"job-no" "Job#" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > asi.rm-rctd.job-no2
"job-no2" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > asi.rm-rctd.i-no
"i-no" "Item" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > asi.rm-rctd.i-name
"i-name" "Name/Desc" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > asi.rm-rctd.s-num
"s-num" "S" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > asi.rm-rctd.b-num
"b-num" "B" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > asi.rm-rctd.loc
"loc" "Whse" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > asi.rm-rctd.loc-bin
"loc-bin" "Bin" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > asi.rm-rctd.tag
"tag" "Tag#" "x(20)" "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > asi.rm-rctd.qty
"qty" "Qty" ? "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > asi.rm-rctd.pur-uom
"pur-uom" "PUOM" "x(4)" "character" ? ? ? 14 ? ? yes ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > asi.rm-rctd.cost
"cost" "Cost" "->>>,>>>,>>9.99<<<<" "decimal" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > asi.rm-rctd.cost-uom
"cost-uom" "CUOM" "x(4)" "character" ? ? ? 14 ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"calc-ext-cost() @ ext-cost" "Ext.Amount" "->>>,>>>,>>9.99<<<<" ? 14 ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"display-dimension('W') @ lv-po-wid" "Width" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"display-dimension('L') @ lv-po-len" "Length" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > asi.rm-rctd.user-id
"user-id" "UserId" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
   DEFINE VARIABLE phandle AS WIDGET-HANDLE NO-UNDO.
   DEFINE VARIABLE char-hdl AS cha NO-UNDO.   
   
   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"buttons-target",OUTPUT char-hdl).
    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) 
       THEN RUN browser-dbclicked IN WIDGET-HANDLE(char-hdl).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON RETURN OF Browser-Table IN FRAME F-Main
ANYWHERE
DO:
        APPLY "tab" TO SELF.
        RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-DISPLAY OF Browser-Table IN FRAME F-Main
DO:  /* display calculated field */
  /* def var ii as int.
   ii = if avail rm-rctd then integer(rm-rctd.po-no) else 0.
   
   if avail rm-rctd then    run get-matrix (yes).
*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
  
  ASSIGN
   ll-help-run = NO
   lv-i-no     = ""
   lv-line     = 0.

  IF AVAIL rm-rctd AND INT(rm-rctd.po-no) NE 0 THEN
    ASSIGN
     lv-i-no = SUBSTR(rm-rctd.BOL,1,30)
     lv-line = INT(SUBSTR(rm-rctd.BOL,31,3)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
 /*  {src/adm/template/brsleave.i}  */
   
    IF KEYFUNCTION(LASTKEY) = "page-up" OR 
      keyfunction(LASTKEY) = "page-down" OR
      keyfunction(LASTKEY) = "cursor-up" OR
      keyfunction(LASTKEY) = "cursor-down" 
   THEN DO:  
      RETURN NO-APPLY.
   END.

   {est/brsleave.i}  /* same as src but update will be same as add record*/

   RUN check-modified IN THIS-PROCEDURE ('clear':U) NO-ERROR. 
   
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

{sys/inc/rmissue.i}
lv-rmissue = v-rmissue.
jobreopn-log = rmissue-int EQ 1.

FIND FIRST sys-ctrl WHERE
    sys-ctrl.company EQ cocode AND
    sys-ctrl.name    EQ "RMWHSBIN"
    NO-LOCK NO-ERROR.

IF NOT AVAILABLE sys-ctrl THEN
DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company  = cocode
        sys-ctrl.name     = "RMWHSBIN"
        sys-ctrl.descrip  = "Default Location for RM Warehouse / Bin?"
        sys-ctrl.char-fld = "RMITEM".
    FIND CURRENT sys-ctrl NO-LOCK.
END.
v-bin = sys-ctrl.char-fld.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-multi-line B-table-Win 
PROCEDURE check-multi-line :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-job-mat FOR job-mat.
DEF BUFFER b-w-rm-rctd FOR w-rm-rctd.

DEF VAR v-cnt    AS INT          NO-UNDO.
DEF VAR ll-multi AS LOG INIT YES NO-UNDO.

   ASSIGN ll-multi = AVAIL w-rm-rctd.

   IF ll-multi THEN
      FIND FIRST job-mat WHERE
           ROWID(job-mat) EQ w-rm-rctd.job-mat-rowid
           NO-LOCK NO-ERROR.

   ASSIGN ll-multi = AVAIL job-mat AND
                     CAN-FIND(FIRST b-job-mat
                               WHERE b-job-mat.company EQ job-mat.company
                                 AND b-job-mat.job     EQ job-mat.job
                                 AND b-job-mat.job-no  EQ job-mat.job-no
                                 AND b-job-mat.job-no2 EQ job-mat.job-no2
                                 AND b-job-mat.rm-i-no EQ job-mat.rm-i-no
                                 AND ROWID(b-job-mat)  NE ROWID(job-mat)
                              ).

   IF ll-multi THEN 
     ASSIGN rm-rctd.job-no:SCREEN-VALUE IN  BROWSE {&BROWSE-NAME} = w-rm-rctd.job-no
            rm-rctd.job-no2:SCREEN-VALUE = STRING(w-rm-rctd.job-no2)
            rm-rctd.i-no:SCREEN-VALUE    = w-rm-rctd.i-no
            rm-rctd.s-num:SCREEN-VALUE   = "?"
            rm-rctd.b-num:SCREEN-VALUE   = STRING(w-rm-rctd.b-num).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-item B-table-Win 
PROCEDURE create-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     CREATE ITEM.
     ASSIGN ITEM.company = cocode
            ITEM.loc = locode
            ITEM.i-no = rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            ITEM.i-code = "R".

    /* run rm/item.p */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-jobmat B-table-Win 
PROCEDURE display-jobmat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-recid AS RECID NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    FIND job-mat WHERE RECID(job-mat) EQ ip-recid NO-LOCK NO-ERROR.
    IF AVAIL job-mat THEN DO:
      rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = job-mat.i-no.

      RUN new-i-no.

      FIND job-mat WHERE RECID(job-mat) EQ ip-recid NO-LOCK.

      ASSIGN
       rm-rctd.s-num:SCREEN-VALUE = STRING(job-mat.frm)
       rm-rctd.b-num:SCREEN-VALUE = STRING(job-mat.blank-no).
    END.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-job-mat B-table-Win 
PROCEDURE get-job-mat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE w-rm-rctd.

    IF lv-job-no NE "" OR 
       rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE "" THEN
       RUN windows/l-jobmt3.w (cocode, lv-job-no,
                               INT(rm-rctd.job-no2:SCREEN-VALUE),
                               rm-rctd.i-no:SCREEN-VALUE,
                               OUTPUT char-val,OUTPUT look-recid,
                               OUTPUT v-number-rows-selected).

    FIND FIRST w-rm-rctd NO-ERROR.

    IF NOT AVAIL w-rm-rctd THEN DO:
       APPLY "ENTRY" TO rm-rctd.s-num.
       RETURN NO-APPLY.
    END.

    IF v-number-rows-selected > 1 THEN
       RUN check-multi-line.
    ELSE DO:         
       ASSIGN fil_id = look-recid.
       RUN s-b-help. 
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-matrix B-table-Win 
PROCEDURE get-matrix :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-first-disp AS LOG NO-UNDO.

  DEF VAR v-dep LIKE po-ordl.s-len NO-UNDO. 
  DEF VAR v-bwt LIKE po-ordl.s-len NO-UNDO.
  DEF VAR lv-out-qty AS DEC NO-UNDO.
  DEF VAR lv-out-cost AS DEC NO-UNDO.
  DEF VAR lv-qty-uom AS cha NO-UNDO.
  DEF VAR lv-cost-uom AS cha NO-UNDO.
  DEF VAR v-job-up LIKE job-hdr.n-on NO-UNDO.
  DEF VAR v-out LIKE ef.n-out INIT 1 NO-UNDO.
  DEF VAR lv-uom LIKE rm-rctd.pur-uom NO-UNDO.
  DEF VAR ld-lf-used AS DEC NO-UNDO.
  DEF VAR ld AS DEC NO-UNDO FORMAT ">,>>9.9<<<".
  DEF VAR ll-disp AS LOG INIT NO NO-UNDO.

  DEF BUFFER b-rm-rctd FOR rm-rctd.
  
IF ip-first-disp  AND AVAIL rm-rctd AND rm-rctd.i-no <> "" THEN DO: /* for row-display */
  FIND item  WHERE item.company EQ cocode                           /* no screen-value used */
                     AND item.i-no  EQ rm-rctd.i-no /*:screen-value in browse {&BROWSE-NAME}*/
                     USE-INDEX i-no NO-LOCK NO-ERROR.
  IF AVAIL item THEN v-dep = item.s-dep.

  RELEASE po-ordl.
      
  IF INT(rm-rctd.po-no) NE 0 THEN
  FIND FIRST po-ordl NO-LOCK
      WHERE po-ordl.company   EQ rm-rctd.company
        AND po-ordl.po-no     EQ INT(rm-rctd.po-no)
        AND (po-ordl.i-no     EQ lv-i-no OR lv-i-no EQ "")
        AND (po-ordl.line     EQ lv-line OR lv-line EQ 0)
        AND po-ordl.item-type EQ YES
      NO-ERROR.

  IF AVAIL po-ordl THEN DO:
    ASSIGN
     v-len       = po-ordl.s-len
     v-wid       = po-ordl.s-wid
     v-bwt       = 0
     lv-i-no     = po-ordl.i-no
     lv-line     = po-ordl.line.
    {rm/pol-dims.i}
  END.

  ELSE DO:
        FIND FIRST job WHERE job.company EQ cocode
                         AND job.job-no  EQ rm-rctd.job-no
                         AND job.job-no2 EQ rm-rctd.job-no2
                NO-LOCK NO-ERROR.
        IF AVAIL job THEN DO :
             FIND FIRST job-mat WHERE job-mat.company EQ cocode
                                  AND job-mat.job     EQ job.job
                                  AND job-mat.i-no    EQ rm-rctd.i-no
                                  AND job-mat.frm     EQ rm-rctd.s-num
                   NO-LOCK NO-ERROR.
             IF AVAIL job-mat THEN DO:
               ASSIGN 
                v-len = job-mat.len
                v-wid = job-mat.wid
                v-bwt = job-mat.basis-w.
             END.
        END.
  END.
        IF v-len EQ 0 THEN v-len = IF AVAIL item THEN item.s-len ELSE 0.
        IF v-wid EQ 0 THEN v-wid = IF AVAIL item AND item.r-wid NE 0 THEN item.r-wid ELSE IF AVAIL item THEN item.s-wid ELSE 0.
        IF v-bwt EQ 0 THEN v-bwt = IF AVAIL item THEN item.basis-w ELSE 0.
        ASSIGN lv-qty-uom = rm-rctd.pur-uom
               lv-cost-uom = rm-rctd.cost-uom.
  
  /* convert qty    pr-qty-uom or po-ordl.pr-uom cons-uom*/
 /* run rm/convquom.p(rm-rctd.pur-uom,
                    po-ordl.cons-uom,
                         v-bwt,
                         v-len,
                         input v-wid,
                         input v-dep,
                         input rm-rctd.qty,
                         output lv-out-qty).
  
  /* convert cost pr-uom*/
  run rm/convcuom.p(rm-rctd.cost-uom, po-ordl.cons-uom,
                    v-bwt, v-len, v-wid, v-dep,
                               rm-rctd.cost, output lv-out-cost).
  */
        
  RUN custom/convquom.p(cocode,
                        rm-rctd.pur-uom,
                        lv-qty-uom,
                        v-bwt,
                        v-len,
                        v-wid,
                        v-dep,
                        rm-rctd.qty,
                        OUTPUT lv-out-qty).
  
  /* convert cost pr-uom*/
  RUN custom/convcuom.p(cocode,
                        rm-rctd.cost-uom,
                        lv-qty-uom,
                        v-bwt,
                        v-len,
                        v-wid,
                        v-dep,
                        rm-rctd.cost, OUTPUT lv-out-cost).

   ASSIGN
    ext-cost = lv-out-qty * lv-out-cost
    ll-disp  = YES.
  
/*    ASSIGN                                                                     */
/*      rm-rctd.cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(lv-out-cost) */
/*      rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = lv-cost-uom     */
/*      ext-cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(ext-cost)        */
/*      rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(lv-out-qty)   */
/*      rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = lv-qty-uom       */
/*      NO-ERROR.                                                                */

   ASSIGN
   rm-rctd.qty:MODIFIED IN BROWSE {&BROWSE-NAME} = NO
   rm-rctd.pur-uom:MODIFIED IN BROWSE {&BROWSE-NAME} = NO.

  /*disp ext-cost with browse {&BROWSE-NAME}. it's displayed automatically */
 /* message "after calc:" po-ordl.cons-uom rm-rctd.cost-uom lv-out-cost ext-cost.
  */

END. /* ip-first */
/* ======================================================================= */
ELSE 
IF AVAIL rm-rctd AND rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} <> "" THEN DO: /* in update mode - use screen-value */
  ASSIGN
   lv-uom     = rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
   lv-out-qty = DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).

  FIND item  WHERE item.company EQ cocode
                AND item.i-no  EQ rm-rctd.i-no:screen-value IN BROWSE {&BROWSE-NAME}
                      USE-INDEX i-no NO-LOCK NO-ERROR.
  IF AVAIL item THEN v-dep = item.s-dep.    
  
  RELEASE po-ordl.

  IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE 0 THEN
  FIND FIRST po-ordl NO-LOCK
      WHERE po-ordl.company EQ cocode
        AND po-ordl.po-no   EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
        AND (po-ordl.i-no   EQ lv-i-no OR lv-i-no EQ "")
        AND (po-ordl.line   EQ lv-line OR lv-line EQ 0)
      NO-ERROR.

  IF AVAIL po-ordl THEN DO:
    ASSIGN
     v-len       = po-ordl.s-len
     v-wid       = po-ordl.s-wid
     v-bwt       = 0
     lv-i-no     = po-ordl.i-no
     lv-line     = po-ordl.line.
    {rm/pol-dims.i}
  END.

  ELSE DO:
        FIND FIRST job WHERE job.company EQ cocode
                         AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                         AND job.job-no2 EQ integer(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
                NO-LOCK NO-ERROR.
        IF AVAIL job THEN DO :
             v-job-up = 0.
             FOR EACH job-hdr FIELDS(n-on)
                 WHERE job-hdr.company EQ cocode
                   AND job-hdr.job     EQ job.job
                   AND job-hdr.job-no  EQ job.job-no
                   AND job-hdr.job-no2 EQ job.job-no2
                   AND job-hdr.frm     EQ int(rm-rctd.s-num:screen-value)
                 NO-LOCK:
               v-job-up = v-job-up + job-hdr.n-on.  
             END.
             
             FIND FIRST job-mat WHERE job-mat.company EQ cocode
                                  AND job-mat.job     EQ job.job
                                  AND job-mat.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                                  AND job-mat.frm     EQ int(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
                   NO-LOCK NO-ERROR.
             IF AVAIL job-mat THEN DO:
               IF lv-rmissue EQ "Net" THEN v-out = job-mat.n-up / v-job-up.
               ASSIGN 
                v-len = job-mat.len
                v-wid = job-mat.wid
                v-bwt = job-mat.basis-w.
             END.
        END.
  END.

  IF v-len EQ 0 THEN v-len = IF AVAIL item THEN item.s-len ELSE 0.
  IF v-wid EQ 0 THEN v-wid = IF AVAIL item AND item.r-wid NE 0 THEN item.r-wid ELSE IF AVAIL item THEN item.s-wid ELSE 0.
  IF v-bwt EQ 0 THEN v-bwt = IF AVAIL item THEN item.basis-w ELSE 0.

  ASSIGN lv-qty-uom = item.cons-uom
         lv-cost-uom = ITEM.cons-uom .

  IF lv-uom EQ "DIA" THEN DO:
    ld-lf-used = 0.

    FOR EACH rm-rcpth
        WHERE rm-rcpth.company   EQ cocode
          AND rm-rcpth.i-no      EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND rm-rcpth.rita-code EQ "R"
        NO-LOCK,
        EACH rm-rdtlh FIELDS(qty)
        WHERE rm-rdtlh.company   EQ rm-rcpth.company
          AND rm-rdtlh.r-no      EQ rm-rcpth.r-no
          AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
          AND rm-rdtlh.loc       EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND rm-rdtlh.loc-bin   EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND rm-rdtlh.tag       EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
        NO-LOCK:

      ld = rm-rdtlh.qty.

      IF rm-rcpth.pur-uom NE "LF" THEN
        RUN rm/convquom.p(rm-rcpth.pur-uom, "LF",
                          v-bwt, v-len, v-wid, v-dep,
                          ld, OUTPUT ld).

      ld-lf-used = ld-lf-used + ld.
    END.

    FOR EACH b-rm-rctd FIELDS(qty pur-uom)
        WHERE b-rm-rctd.company   EQ cocode
          AND b-rm-rctd.i-no      EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND b-rm-rctd.rita-code EQ "R"
          AND b-rm-rctd.loc       EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND b-rm-rctd.loc-bin   EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND b-rm-rctd.tag       EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND ROWID(b-rm-rctd)    NE ROWID(rm-rctd)
        NO-LOCK:

      ld = b-rm-rctd.qty.

      IF b-rm-rctd.pur-uom NE "LF" THEN
        RUN rm/convquom.p(b-rm-rctd.pur-uom, "LF",
                          v-bwt, v-len, v-wid, v-dep,
                          ld, OUTPUT ld).

      ld-lf-used = ld-lf-used + ld.
    END.

    FOR EACH rm-rcpth
        WHERE rm-rcpth.company   EQ cocode
          AND rm-rcpth.i-no      EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND rm-rcpth.rita-code EQ "I"
        NO-LOCK,
        EACH rm-rdtlh FIELDS(qty)
        WHERE rm-rdtlh.company   EQ rm-rcpth.company
          AND rm-rdtlh.r-no      EQ rm-rcpth.r-no
          AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
          AND rm-rdtlh.loc       EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND rm-rdtlh.loc-bin   EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND rm-rdtlh.tag       EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
        NO-LOCK:

      ld = rm-rdtlh.qty.

      IF rm-rcpth.pur-uom NE "LF" THEN
        RUN rm/convquom.p(rm-rcpth.pur-uom, "LF",
                          v-bwt, v-len, v-wid, v-dep,
                          ld, OUTPUT ld).

      ld-lf-used = ld-lf-used - ld.
    END.

    FOR EACH b-rm-rctd FIELDS(qty pur-uom)
        WHERE b-rm-rctd.company   EQ cocode
          AND b-rm-rctd.i-no      EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND b-rm-rctd.rita-code EQ "I"
          AND b-rm-rctd.loc       EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND b-rm-rctd.loc-bin   EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND b-rm-rctd.tag       EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND ROWID(b-rm-rctd)    NE ROWID(rm-rctd)
        NO-LOCK:

      ld = b-rm-rctd.qty.

      IF b-rm-rctd.pur-uom NE "LF" THEN
        RUN rm/convquom.p(b-rm-rctd.pur-uom, "LF",
                          v-bwt, v-len, v-wid, v-dep,
                          ld, OUTPUT ld).

      ld-lf-used = ld-lf-used - ld.
    END.

    ld = item.ect / 10000.

    IF ld LE 0 THEN DO TRANSACTION:
      MESSAGE "Please enter Core Diameter:" UPDATE ld.
      FIND CURRENT item EXCLUSIVE NO-ERROR.
      IF AVAIL item THEN item.ect = ld * 10000.
      FIND CURRENT item NO-LOCK NO-ERROR.
    END.

    ASSIGN
     lv-out-qty = ld-lf-used -
                  (((lv-out-qty * lv-out-qty) - (ld * ld)) *
                   .0655 / item.cal)
     lv-uom     = "LF".
  END. /* IF lv-uom EQ "DIA" */

  /* convert qty */
  IF lv-uom NE lv-qty-uom THEN
    RUN rm/convquom.p(lv-uom, lv-qty-uom, v-bwt, v-len, v-wid, v-dep,
                      lv-out-qty / v-out, OUTPUT lv-out-qty).
  
  /* convert cost */
  IF rm-rctd.cost-uom:screen-value IN BROWSE {&BROWSE-NAME} EQ lv-cost-uom THEN
    lv-out-cost = dec(rm-rctd.cost:screen-value IN BROWSE {&BROWSE-NAME}).
  ELSE
    RUN rm/convcuom.p(rm-rctd.cost-uom:screen-value IN BROWSE {&BROWSE-NAME},
                      lv-cost-uom, v-bwt, v-len, v-wid, v-dep,
                      rm-rctd.cost:screen-value IN BROWSE {&BROWSE-NAME},
                      OUTPUT lv-out-cost).

  ASSIGN
   ext-cost = lv-out-qty * lv-out-cost
   ll-disp  = YES.

  ASSIGN
     rm-rctd.cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(lv-out-cost)
     rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = lv-cost-uom
     rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(lv-out-qty)
     rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = lv-qty-uom
     ext-cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(ext-cost)
     NO-ERROR.
END.


/*IF ll-disp THEN
  ASSIGN
   rm-rctd.cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(lv-out-cost)
   rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = lv-cost-uom
   rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(lv-out-qty)
   rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = lv-qty-uom
   ext-cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(ext-cost)
   NO-ERROR.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-matrix-tandem-rec B-table-Win 
PROCEDURE get-matrix-tandem-rec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-dep LIKE po-ordl.s-len NO-UNDO. 
  DEF VAR v-bwt LIKE po-ordl.s-len NO-UNDO.
  DEF VAR lv-out-qty AS DEC NO-UNDO.
  DEF VAR lv-out-cost AS DEC NO-UNDO.
  DEF VAR lv-qty-uom AS cha NO-UNDO.
  DEF VAR lv-cost-uom AS cha NO-UNDO.
  DEF VAR v-job-up LIKE job-hdr.n-on NO-UNDO.
  DEF VAR v-out LIKE ef.n-out INIT 1 NO-UNDO.
  DEF VAR lv-uom LIKE rm-rctd.pur-uom NO-UNDO.
  DEF VAR ld-lf-used AS DEC NO-UNDO.
  DEF VAR ld AS DEC NO-UNDO FORMAT ">,>>9.9<<<".
  DEF VAR ll-disp AS LOG INIT NO NO-UNDO.

  DEF BUFFER b-rm-rctd FOR rm-rctd.
  
  IF AVAIL rm-rctd AND rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} <> "" THEN DO:
     ASSIGN
      lv-uom     = rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
      lv-out-qty = DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).
    
     FIND item  WHERE item.company EQ cocode
                   AND item.i-no  EQ rm-rctd.i-no:screen-value IN BROWSE {&BROWSE-NAME}
                         USE-INDEX i-no NO-LOCK NO-ERROR.
     IF AVAIL item THEN v-dep = item.s-dep.    
     
     RELEASE po-ordl.
    
     IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE 0 THEN
     FIND FIRST po-ordl NO-LOCK
         WHERE po-ordl.company EQ cocode
           AND po-ordl.po-no   EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
           AND (po-ordl.i-no   EQ lv-i-no OR lv-i-no EQ "")
           AND (po-ordl.line   EQ lv-line OR lv-line EQ 0)
         NO-ERROR.
    
     IF AVAIL po-ordl THEN DO:
       ASSIGN
        v-len       = po-ordl.s-len
        v-wid       = po-ordl.s-wid
        v-bwt       = 0
        lv-i-no     = po-ordl.i-no
        lv-line     = po-ordl.line.
       {rm/pol-dims.i}
     END.
    
     ELSE DO:
        FIND FIRST job WHERE job.company EQ cocode
                         AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                         AND job.job-no2 EQ integer(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
                NO-LOCK NO-ERROR.
        IF AVAIL job THEN DO :
             v-job-up = 0.
             FOR EACH job-hdr FIELDS(n-on)
                 WHERE job-hdr.company EQ cocode
                   AND job-hdr.job     EQ job.job
                   AND job-hdr.job-no  EQ job.job-no
                   AND job-hdr.job-no2 EQ job.job-no2
                   AND job-hdr.frm     EQ int(rm-rctd.s-num:screen-value)
                 NO-LOCK:
               v-job-up = v-job-up + job-hdr.n-on.  
             END.
             
             FIND FIRST job-mat WHERE job-mat.company EQ cocode
                                  AND job-mat.job     EQ job.job
                                  AND job-mat.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                                  AND job-mat.frm     EQ int(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
                   NO-LOCK NO-ERROR.
             IF AVAIL job-mat THEN DO:
               IF lv-rmissue EQ "Net" THEN v-out = job-mat.n-up / v-job-up.
               ASSIGN 
                v-len = job-mat.len
                v-wid = job-mat.wid
                v-bwt = job-mat.basis-w.
             END.
        END.
     END.
    
     IF v-len EQ 0 THEN v-len = IF AVAIL item THEN item.s-len ELSE 0.
     IF v-wid EQ 0 THEN v-wid = IF AVAIL item AND item.r-wid NE 0 THEN item.r-wid ELSE IF AVAIL item THEN item.s-wid ELSE 0.
     IF v-bwt EQ 0 THEN v-bwt = IF AVAIL item THEN item.basis-w ELSE 0.
    
     ASSIGN lv-qty-uom = item.cons-uom
            lv-cost-uom = ITEM.cons-uom .
    
     IF lv-uom EQ "DIA" THEN DO:
        ld-lf-used = 0.

        FOR EACH rm-rcpth
            WHERE rm-rcpth.company   EQ cocode
              AND rm-rcpth.i-no      EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
              AND rm-rcpth.rita-code EQ "R"
            NO-LOCK,
            EACH rm-rdtlh FIELDS(qty)
            WHERE rm-rdtlh.company   EQ rm-rcpth.company
              AND rm-rdtlh.r-no      EQ rm-rcpth.r-no
              AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
              AND rm-rdtlh.loc       EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
              AND rm-rdtlh.loc-bin   EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
              AND rm-rdtlh.tag       EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            NO-LOCK:

          ld = rm-rdtlh.qty.

          IF rm-rcpth.pur-uom NE "LF" THEN
            RUN rm/convquom.p(rm-rcpth.pur-uom, "LF",
                              v-bwt, v-len, v-wid, v-dep,
                              ld, OUTPUT ld).

          ld-lf-used = ld-lf-used + ld.
        END.

        FOR EACH b-rm-rctd FIELDS(qty pur-uom)
            WHERE b-rm-rctd.company   EQ cocode
              AND b-rm-rctd.i-no      EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
              AND b-rm-rctd.rita-code EQ "R"
              AND b-rm-rctd.loc       EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
              AND b-rm-rctd.loc-bin   EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
              AND b-rm-rctd.tag       EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
              AND ROWID(b-rm-rctd)    NE ROWID(rm-rctd)
            NO-LOCK:

          ld = b-rm-rctd.qty.

          IF b-rm-rctd.pur-uom NE "LF" THEN
            RUN rm/convquom.p(b-rm-rctd.pur-uom, "LF",
                              v-bwt, v-len, v-wid, v-dep,
                              ld, OUTPUT ld).

          ld-lf-used = ld-lf-used + ld.
        END.

        FOR EACH rm-rcpth
            WHERE rm-rcpth.company   EQ cocode
              AND rm-rcpth.i-no      EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
              AND rm-rcpth.rita-code EQ "I"
            NO-LOCK,
            EACH rm-rdtlh FIELDS(qty)
            WHERE rm-rdtlh.company   EQ rm-rcpth.company
              AND rm-rdtlh.r-no      EQ rm-rcpth.r-no
              AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
              AND rm-rdtlh.loc       EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
              AND rm-rdtlh.loc-bin   EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
              AND rm-rdtlh.tag       EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            NO-LOCK:

          ld = rm-rdtlh.qty.

          IF rm-rcpth.pur-uom NE "LF" THEN
            RUN rm/convquom.p(rm-rcpth.pur-uom, "LF",
                              v-bwt, v-len, v-wid, v-dep,
                              ld, OUTPUT ld).

          ld-lf-used = ld-lf-used - ld.
        END.

        FOR EACH b-rm-rctd FIELDS(qty pur-uom)
            WHERE b-rm-rctd.company   EQ cocode
              AND b-rm-rctd.i-no      EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
              AND b-rm-rctd.rita-code EQ "I"
              AND b-rm-rctd.loc       EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
              AND b-rm-rctd.loc-bin   EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
              AND b-rm-rctd.tag       EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
              AND ROWID(b-rm-rctd)    NE ROWID(rm-rctd)
            NO-LOCK:

          ld = b-rm-rctd.qty.

          IF b-rm-rctd.pur-uom NE "LF" THEN
            RUN rm/convquom.p(b-rm-rctd.pur-uom, "LF",
                              v-bwt, v-len, v-wid, v-dep,
                              ld, OUTPUT ld).

          ld-lf-used = ld-lf-used - ld.
        END.

        ld = item.ect / 10000.

        IF ld LE 0 THEN DO TRANSACTION:
          MESSAGE "Please enter Core Diameter:" UPDATE ld.
          FIND CURRENT item EXCLUSIVE NO-ERROR.
          IF AVAIL item THEN item.ect = ld * 10000.
          FIND CURRENT item NO-LOCK NO-ERROR.
        END.

        ASSIGN
         lv-out-qty = ld-lf-used -
                      (((lv-out-qty * lv-out-qty) - (ld * ld)) *
                       .0655 / item.cal)
         lv-uom     = "LF".
     END. /* IF lv-uom EQ "DIA" */
    

     /* convert qty */
     IF lv-uom NE lv-qty-uom THEN
       RUN rm/convquom.p(lv-uom, lv-qty-uom, v-bwt, v-len, v-wid, v-dep,
                         lv-out-qty / v-out, OUTPUT lv-out-qty).
     
     /* convert cost */
     IF rm-rctd.cost-uom:screen-value IN BROWSE {&BROWSE-NAME} EQ lv-cost-uom THEN
       lv-out-cost = dec(rm-rctd.cost:screen-value IN BROWSE {&BROWSE-NAME}).
     ELSE
       RUN rm/convcuom.p(rm-rctd.cost-uom:screen-value IN BROWSE {&BROWSE-NAME},
                         lv-cost-uom, v-bwt, v-len, v-wid, v-dep,
                         rm-rctd.cost:screen-value IN BROWSE {&BROWSE-NAME},
                         OUTPUT lv-out-cost).
    
     ASSIGN
      ext-cost = lv-out-qty * lv-out-cost
      ll-disp  = YES.
END.

IF ll-disp THEN
  ASSIGN
   rm-rctd.cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(lv-out-cost)
   rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = lv-cost-uom
   rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(lv-out-qty)
   rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = lv-qty-uom
   ext-cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(ext-cost)
   NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-qty-matrix B-table-Win 
PROCEDURE get-qty-matrix :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF VAR ld LIKE job-mat.qty NO-UNDO.
  DEF VAR v-qty LIKE job-mat.qty NO-UNDO. 

  RELEASE job.

  EMPTY TEMP-TABLE tt-frm.

  FIND FIRST job WHERE
       job.company EQ cocode AND
       job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} AND
       job.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
       NO-LOCK NO-ERROR.

  IF AVAIL job THEN DO:
     ld = 0.
    
     FOR EACH w-rm-rctd WHERE
         w-rm-rctd.i-no EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}:

         FIND FIRST tt-frm WHERE
              tt-frm.frm = w-rm-rctd.s-num
              NO-ERROR.

         IF NOT AVAIL tt-frm THEN
         DO:
            CREATE tt-frm.
            ASSIGN tt-frm.frm = w-rm-rctd.s-num.
            RELEASE tt-frm.
         END.
     END.

     FOR EACH tt-frm,
         EACH job-mat FIELDS(frm qty qty-uom rm-i-no job-no job-no2) WHERE
         job-mat.company EQ job.company AND
         job-mat.job     EQ job.job AND
         job-mat.job-no  EQ job.job-no AND
         job-mat.job-no2 EQ job.job-no2 AND
         job-mat.rm-i-no EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} AND
         job-mat.frm EQ tt-frm.frm
         NO-LOCK:
    
         RUN tandem-rec-uom-conv(INPUT job-mat.rm-i-no,
                                 INPUT job-mat.qty-uom,
                                 INPUT job-mat.qty,
                                 INPUT job-mat.job-no,
                                 INPUT job-mat.job-no2,
                                 INPUT job-mat.frm,
                                 OUTPUT v-qty).

         ASSIGN
            tt-frm.mrp = tt-frm.mrp + v-qty
            ld         = ld + v-qty.
     END.
    
     IF ld NE 0 THEN
        FOR EACH tt-frm:
            tt-frm.qtypct = (tt-frm.mrp / ld).
        END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-tandem-rec B-table-Win 
PROCEDURE get-tandem-rec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-cons-uom AS CHAR NO-UNDO.
    
   DEF VAR ld AS DEC NO-UNDO.

   v-get-tandem-rec = YES.

   RUN get-qty-matrix.

   EMPTY TEMP-TABLE tt-tag.

   FOR EACH tt-selected,
       FIRST rm-bin FIELDS(qty tag) WHERE
             ROWID(rm-bin) EQ tt-selected.tt-rowid
             NO-LOCK:

       CREATE tt-tag.
       ASSIGN
          tt-tag.tag = rm-bin.tag
          tt-tag.qty = rm-bin.qty
          tt-tag.tt-rowid = ROWID(rm-bin)
          ld = ld + rm-bin.qty.
   END.

   FOR EACH tt-frm:
       tt-frm.qty = ROUND(ld * tt-frm.qtypct,2).

       IF ip-cons-uom EQ "EA" THEN
       DO:
          {sys/inc/roundup.i tt-frm.qty}
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

  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.

  /* Problem was the record was still in browse after a cancel, */
  /* because the record was not deleted here, so make sure it's */
  /* found if there is a blank one                              */
  IF NOT AVAIL rm-rctd THEN
    FIND FIRST rm-rctd WHERE rm-rctd.company = cocode
      AND rm-rctd.i-no EQ ""
      AND rm-rctd.tag EQ ""
      AND rm-rctd.job-no EQ ""
      AND rm-rctd.rita-code EQ "I"
       EXCLUSIVE-LOCK NO-ERROR. 
    IF v-rmtags-log AND AVAIL(rm-rctd) THEN 
       FOR EACH wiptag WHERE wiptag.company = rm-rctd.company 
               AND wiptag.sts = "Issued" 
               AND wiptag.rm-tag-no = rm-rctd.tag EXCLUSIVE-LOCK:
            DELETE wiptag .
       END.
  IF NOT AVAIL rm-rctd THEN
    RETURN.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  EMPTY TEMP-TABLE tt-selected.

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
  
  ASSIGN
   adm-new-record      = NO
   adm-adding-record   = NO
   ll-is-copy-record   = NO.


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

  /*FIND FIRST rm-rctd NO-LOCK
      WHERE ROWID(rm-rctd) EQ ip-rowid NO-ERROR .

     IF v-rmtags-log AND AVAIL rm-rctd AND
     rm-rctd.tag NE "" AND
     NOT CAN-FIND(FIRST wiptag WHERE
         wiptag.company EQ cocode AND
         wiptag.rm-tag-no EQ rm-rctd.tag) THEN
     DO:
        MESSAGE "Launch WIP Tag Creation for Item " rm-rctd.i-no "Tag #" rm-rctd.tag "?" VIEW-AS ALERT-BOX QUESTION
                BUTTON YES-NO UPDATE ll-wip AS LOG.
        /* btr */
        IF ll-wip THEN
           RUN jcrep/wipldtg.w  (INPUT rm-rctd.tag,
                                 INPUT rm-rctd.job-no,
                                 INPUT rm-rctd.job-no2,
                                 INPUT rm-rctd.i-no,
                                 INPUT rm-rctd.s-num,
                                 INPUT rm-rctd.b-num,
                                 INPUT rm-rctd.qty,
                                 INPUT rm-rctd.pur-uom,
                                 OUTPUT vlc-success).
     END.

   RUN multi-issues (ROWID(rm-rctd)) NO-ERROR.*/


   RUN dispatch ('open-query').
  
  DO WITH FRAME {&FRAME-NAME}:
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
  END.
  
 APPLY "VALUE-CHANGED":U TO BROWSE {&browse-name}.
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
  
   RUN local-delete-record .
  
  
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
  {src/adm/template/snd-list.i "rm-rctd"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tag-method B-table-Win 
PROCEDURE tag-method :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER op-tag# AS LOG NO-UNDO.
 
  
  {rm/tag#.i}
  op-tag# = v-tag#.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tag-sequence B-table-Win 
PROCEDURE tag-sequence :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
  def var v-tag-seq as int no-undo.
  def var v-locode as cha no-undo.
  def buffer xrm-rctd for rm-rctd.
  
  assign v-tag-seq = 0
         v-locode  = "".

  do while true:
    find first xrm-rctd
        where xrm-rctd.company eq rm-rctd.company
          and xrm-rctd.loc     gt v-locode
        no-lock no-error.

    if avail xrm-rctd then do:
      v-locode = xrm-rctd.loc.

      for each xrm-rctd where xrm-rctd.company eq rm-rctd.company
            and xrm-rctd.loc     eq v-locode
            and xrm-rctd.tag     begins string(int(rm-rctd.po-no:screen-value in browse {&BROWSE-NAME}),"999999")
            use-index tag no-lock
            by xrm-rctd.tag desc:

           if int(substr(xrm-rctd.tag,7,2)) gt v-tag-seq then
           v-tag-seq = int(substr(xrm-rctd.tag,7,2)).
            leave.
      end.
    end.

    else leave.
  end.  /* do while */
/* ======= may not need any more 
  v-locode = "".
  if v-tag-seq eq 0 then do while true:
    find first rm-rctdh where rm-rctdh.company eq rm-rcth.company
          and rm-rctdh.loc     gt v-locode
        no-lock no-error.

    if avail rm-rctdh then do:
      v-locode = rm-rctdh.loc.

      for each rm-rctdh
          where rm-rctdh.company eq cocode
            and rm-rctdh.loc     eq v-locode
            and rm-rctdh.tag     begins string(int(rm-rctd.po-no),"999999")
          use-index tag no-lock
          by rm-rctdh.tag desc:

        if int(substr(rm-rctdh.tag,7,2)) gt v-tag-seq then
          v-tag-seq = int(substr(rm-rctdh.tag,7,2)).
        leave.
      end.
    end.

    else leave.
  end.
============================== */
  assign  v-tag-seq   = v-tag-seq + 1
         /* rm-rctd.tag:screen-value in browse {&BROWSE-NAME}
          = string(int(rm-rctd.po-no:screen-value in browse {&BROWSE-NAME}),"999999") + string(v-tag-seq,"99").
           */
        .        
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tandem-rec-uom-conv B-table-Win 
PROCEDURE tandem-rec-uom-conv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-i-no AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-pur-uom AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-qty AS DEC NO-UNDO.
   DEFINE INPUT PARAMETER ip-job-no AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-job-no2 AS INT NO-UNDO.
   DEFINE INPUT PARAMETER ip-s-num AS INT NO-UNDO.

   DEFINE OUTPUT PARAMETER lv-out-qty AS DEC NO-UNDO.

   DEF VAR v-dep LIKE po-ordl.s-len NO-UNDO. 
   DEF VAR v-job-up LIKE job-hdr.n-on NO-UNDO.
   DEF VAR v-out LIKE ef.n-out INIT 1 NO-UNDO.
   DEF VAR v-bwt LIKE po-ordl.s-len NO-UNDO.

   IF ip-i-no EQ "" THEN
      LEAVE.

   ASSIGN
      lv-out-qty = ip-qty.
   
   FIND FIRST item WHERE
        item.company EQ cocode AND
        item.i-no EQ ip-i-no
        NO-LOCK NO-ERROR.

   IF NOT AVAIL ITEM OR
      ip-pur-uom EQ item.cons-uom THEN
      LEAVE.

   v-dep = item.s-dep.

   FIND FIRST job WHERE
        job.company EQ cocode AND
        job.job-no  EQ ip-job-no AND
        job.job-no2 EQ ip-job-no2
        NO-LOCK NO-ERROR.
   
   IF AVAIL job THEN DO:
      v-job-up = 0.
      FOR EACH job-hdr FIELDS(n-on)
          WHERE job-hdr.company EQ cocode
            AND job-hdr.job     EQ job.job
            AND job-hdr.job-no  EQ job.job-no
            AND job-hdr.job-no2 EQ job.job-no2
            AND job-hdr.frm     EQ ip-s-num
          NO-LOCK:
          v-job-up = v-job-up + job-hdr.n-on.  
      END.
      
      FIND FIRST job-mat WHERE
           job-mat.company EQ cocode AND
           job-mat.job     EQ job.job AND
           job-mat.i-no    EQ ip-i-no AND
           job-mat.frm     EQ ip-s-num
           NO-LOCK NO-ERROR.

      IF AVAIL job-mat THEN DO:
         IF lv-rmissue EQ "Net" THEN
            v-out = job-mat.n-up / v-job-up.
         ASSIGN 
            v-len = job-mat.len
            v-wid = job-mat.wid
            v-bwt = job-mat.basis-w.
      END.
   END.
   
   IF v-len EQ 0 THEN v-len = IF AVAIL item THEN item.s-len ELSE 0.
   IF v-wid EQ 0 THEN v-wid = IF AVAIL item AND item.r-wid NE 0 THEN item.r-wid ELSE IF AVAIL item THEN item.s-wid ELSE 0.
   IF v-bwt EQ 0 THEN v-bwt = IF AVAIL item THEN item.basis-w ELSE 0.
   
   RUN rm/convquom.p(ip-pur-uom, item.cons-uom, v-bwt, v-len, v-wid, v-dep,
                     lv-out-qty / v-out, OUTPUT lv-out-qty).
   
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calc-ext-cost B-table-Win 
FUNCTION calc-ext-cost RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  RUN get-matrix (YES).

  RETURN ext-cost.
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-dimension B-table-Win 
FUNCTION display-dimension RETURNS DECIMAL
  ( INPUT ip-dim AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  from rcptdims.v  
    Notes:  
------------------------------------------------------------------------------*/
  
  RETURN IF ip-dim EQ "W" THEN v-wid ELSE v-len.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

