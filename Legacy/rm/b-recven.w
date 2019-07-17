&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  addon/rm/b-recven.w

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

/* gdm - 09190805 */
&SCOPED-DEFINE yellowColumnsName b-rcptd

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/gcompany.i}
{custom/gloc.i}
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED} 

ASSIGN
 cocode = g_company
 locode = g_loc.

DEFINE BUFFER b-po-ord FOR po-ord.
DEFINE BUFFER b-company FOR company.
DEFINE BUFFER bLoadTag FOR loadtag.

DEF VAR lv-search AS CHAR NO-UNDO.
def var char-val as cha no-undo.
def var ext-cost as decimal no-undo.
DEF VAR lv-rowid AS ROWID NO-UNDO.
def var ll-qty-valid as LOG no-undo.
def var hd-post as widget-handle no-undo.
def var hd-post-child as widget-handle no-undo.
def var ll-help-run as log no-undo.  /* set on browse help, reset row-entry */

DEF VAR lv-po-wid LIKE po-ordl.s-wid NO-UNDO.
DEF VAR lv-po-len LIKE po-ordl.s-len FORM ">>,>>9.9999" NO-UNDO.
DEF VAR v-avgcost AS LOG NO-UNDO.
DEF VAR ll-tag-meth AS LOG NO-UNDO.
DEF VAR ll-warned AS LOG NO-UNDO.
DEF VAR v-ssrmscan AS LOG NO-UNDO.
DEF VAR v-case-tag AS LOG NO-UNDO.
DEF VAR v-bin AS CHAR NO-UNDO.
DEF VAR v-loadtag AS CHAR NO-UNDO INIT "ASI". /* sys ctrl option */
DEF VAR lv-vend-tag AS CHAR NO-UNDO.
DEF VAR v-prg-idx AS INT NO-UNDO.
DEF VAR ll-add-setup AS LOG NO-UNDO.
DEF VAR lv-save-fld AS CHAR EXTENT 2 NO-UNDO.
DEF VAR lv-setup AS DEC NO-UNDO.
DEF VAR lv-entry-qty AS DEC NO-UNDO.
DEF VAR lv-entry-qty-uom AS CHAR NO-UNDO.
DEF VAR v-post-date AS DATE INIT TODAY NO-UNDO.
DEFINE VARIABLE cSSScanVendor AS CHARACTER NO-UNDO .
DEFINE VARIABLE lFound AS LOGICAL NO-UNDO .

&scoped-define fld-name-1 loadtag.misc-char[1]
&scoped-define SORTBY-PHRASE BY {&fld-name-1}

{windows/l-poitmw.i NEW}
{jc/jcgl-sh.i NEW}

DEF BUFFER br-tmp FOR rm-rctd.  /* for tag validation */
DEF BUFFER xrm-rdtlh FOR rm-rdtlh. /* for tag validation */

FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ g_company
                        AND sys-ctrl.name  EQ "SSRMSCAN" NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
   CREATE sys-ctrl.
   ASSIGN sys-ctrl.company = g_company
          sys-ctrl.NAME = "SSRMSCAN"
          sys-ctrl.descrip = "Prompt for the Warehouse/Bin?"
          sys-ctrl.log-fld = YES.
END.
v-ssrmscan = IF AVAIL sys-ctrl THEN sys-ctrl.log-fld ELSE YES.

DEF VAR v-rmtags-log AS LOG NO-UNDO.
FIND FIRST sys-ctrl
    WHERE sys-ctrl.company eq cocode
    AND sys-ctrl.name eq "RMTAGS"
    NO-LOCK NO-ERROR.
v-rmtags-log = IF AVAIL sys-ctrl THEN sys-ctrl.LOG-fld ELSE NO.

FIND FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.company EQ g_company
       AND sys-ctrl.name    EQ "RMWHSBIN" NO-ERROR.
IF NOT AVAIL sys-ctrl THEN
DO TRANSACTION:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = g_company
   sys-ctrl.name     = "RMWHSBIN"
   sys-ctrl.descrip  = "Default Location for RM Warehouse / Bin?"
   sys-ctrl.char-fld = "RMITEM".
  FIND CURRENT sys-ctrl NO-LOCK.
END.
v-bin = sys-ctrl.char-fld.

DO TRANSACTION:
  {sys/inc/rmrecpt.i}
  {sys/inc/rmunderover.i}
  {sys/inc/sspostvt.i}
  {sys/inc/rmemails.i}
  {sys/inc/postdate.i}
  {sys/inc/rmpostgl.i}
END.
RUN sys/ref/nk1look.p (INPUT cocode,
                        INPUT "SSScanVendor",
                        INPUT "C",
                        INPUT NO,
                        INPUT NO,
                        INPUT "",
                        INPUT "",
                        OUTPUT cSSScanVendor,
                        OUTPUT lFound).

DEFINE VARIABLE lv-do-what AS CHAR INIT "Receipt" NO-UNDO.

DEFINE BUFFER bpo-ordl FOR po-ordl.

DEF TEMP-TABLE tt-rctd NO-UNDO LIKE rm-rctd FIELD tt-row-id AS ROWID
                                        FIELD rm-row-id AS ROWID
                                        FIELD has-rec   AS LOG INIT NO
                                        FIELD seq-no    AS INT
                                        INDEX seq-no seq-no i-no.

DEF TEMP-TABLE tt-email NO-UNDO
    FIELD vend-no      AS CHAR 
    FIELD po-no        AS INT
    FIELD item-no      AS char
    FIELD item-name    AS CHAR
    FIELD po-qty       AS DEC
    FIELD recvd-qty    AS DEC
    FIELD total-recvd-qty AS DEC
    FIELD cons-uom        AS CHAR
    FIELD overrun-pct AS DEC
    FIELD underrun-pct AS DEC
    FIELD allow-qty    AS DEC  
    FIELD under-qty    AS DEC
    FIELD undovr       AS CHAR
    INDEX po-no po-no ASC item-no ASC.


DEF TEMP-TABLE tt-mat NO-UNDO FIELD frm LIKE job-mat.frm
                              FIELD qty LIKE job-mat.qty
                              INDEX frm frm.

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
&Scoped-define INTERNAL-TABLES rm-rctd loadtag

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table rm-rctd.r-no rm-rctd.tag ~
loadtag.misc-char[1] loadtag.misc-char[2] rm-rctd.loc rm-rctd.loc-bin rm-rctd.rct-date ~
rm-rctd.po-no rm-rctd.job-no rm-rctd.job-no2 rm-rctd.s-num rm-rctd.i-no ~
rm-rctd.i-name rm-rctd.qty rm-rctd.pur-uom rm-rctd.cost rm-rctd.cost-uom ~
calc-ext-cost() @ ext-cost display-dimension('W') @ lv-po-wid ~
display-dimension('L') @ lv-po-len rm-rctd.user-id 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table rm-rctd.tag ~
loadtag.misc-char[2] rm-rctd.loc rm-rctd.loc-bin rm-rctd.i-no rm-rctd.qty 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table loadtag rm-rctd 
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table loadtag
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-Browser-Table rm-rctd
&Scoped-define QUERY-STRING-Browser-Table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company EQ cocode ~
AND rm-rctd.rita-code EQ "R" ~
AND rm-rctd.tag NE "" ~
AND rm-rctd.qty GE 0 NO-LOCK, ~
      FIRST loadtag WHERE TRUE /* Join to rm-rctd incomplete */ ~
      AND loadtag.company eq rm-rctd.company and ~
loadtag.item-type = yes and ~
loadtag.tag-no eq rm-rctd.tag and ~
loadtag.misc-char[1] begins lv-search NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company EQ cocode ~
AND rm-rctd.rita-code EQ "R" ~
AND rm-rctd.tag NE "" ~
AND rm-rctd.qty GE 0 NO-LOCK, ~
      FIRST loadtag WHERE TRUE /* Join to rm-rctd incomplete */ ~
      AND loadtag.company eq rm-rctd.company and ~
loadtag.item-type = yes and ~
loadtag.tag-no eq rm-rctd.tag and ~
loadtag.misc-char[1] begins lv-search NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table rm-rctd loadtag
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table rm-rctd
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table loadtag


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-4 RECT-5 Browser-Table auto_find ~
Btn_Clear_Find browse-order 
&Scoped-Define DISPLAYED-OBJECTS fi_sortby auto_find browse-order 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD checkWhsBin B-table-Win 
FUNCTION checkWhsBin RETURNS LOGICAL
  (ipCompany AS CHARACTER,ipLoc AS CHARACTER,ipLocBin AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-dimension B-table-Win 
FUNCTION display-dimension RETURNS DECIMAL
  ( INPUT ip-dim AS char )  FORWARD.

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
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 74 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 144 BY 1.43.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 146 BY 12.38.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      rm-rctd, 
      loadtag SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      rm-rctd.r-no COLUMN-LABEL "Seq#" FORMAT ">>>>>>>9":U LABEL-BGCOLOR 14
      rm-rctd.tag COLUMN-LABEL "Tag#" FORMAT "x(20)":U LABEL-BGCOLOR 14
      loadtag.misc-char[1] COLUMN-LABEL "Vendor Tag #" FORMAT "x(21)":U
            WIDTH 30 LABEL-BGCOLOR 14
      loadtag.misc-char[2] COLUMN-LABEL "RM Lot #" FORMAT "x(21)":U
            WIDTH 20 
      rm-rctd.loc COLUMN-LABEL "Whse" FORMAT "x(13)":U LABEL-BGCOLOR 14
      rm-rctd.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U LABEL-BGCOLOR 14
      rm-rctd.rct-date FORMAT "99/99/9999":U LABEL-BGCOLOR 14
      rm-rctd.po-no FORMAT "x(6)":U WIDTH 9 LABEL-BGCOLOR 14
      rm-rctd.job-no COLUMN-LABEL "Job" FORMAT "x(6)":U LABEL-BGCOLOR 14
      rm-rctd.job-no2 FORMAT "99":U
      rm-rctd.s-num COLUMN-LABEL "S" FORMAT ">9":U
      rm-rctd.i-no COLUMN-LABEL "Item" FORMAT "x(10)":U LABEL-BGCOLOR 14
      rm-rctd.i-name COLUMN-LABEL "Name/Desc" FORMAT "x(30)":U
            LABEL-BGCOLOR 14
      rm-rctd.qty COLUMN-LABEL "Qty" FORMAT "->,>>>,>>9.9<<":U
            WIDTH 20 LABEL-BGCOLOR 14
      rm-rctd.pur-uom COLUMN-LABEL "UOM" FORMAT "x(4)":U WIDTH 7
            LABEL-BGCOLOR 14
      rm-rctd.cost COLUMN-LABEL "Cost" FORMAT "->>>,>>9.99<<<<":U
            LABEL-BGCOLOR 14
      rm-rctd.cost-uom COLUMN-LABEL "UOM" FORMAT "x(4)":U WIDTH 7
            LABEL-BGCOLOR 14
      calc-ext-cost() @ ext-cost COLUMN-LABEL "Ext.Amount" FORMAT "->>>,>>9.99<<":U
            COLUMN-BGCOLOR 14
      display-dimension('W') @ lv-po-wid COLUMN-LABEL "Width"
      display-dimension('L') @ lv-po-len COLUMN-LABEL "Length"
      rm-rctd.user-id COLUMN-LABEL "User ID" FORMAT "x(8)":U WIDTH 15
  ENABLE
      rm-rctd.tag
      loadtag.misc-char[2]
      rm-rctd.loc
      rm-rctd.loc-bin
      rm-rctd.i-no
      rm-rctd.qty
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 144 BY 11.19
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 2 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     fi_sortby AT ROW 12.29 COL 61 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     auto_find AT ROW 12.29 COL 93 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 12.29 COL 132 HELP
          "CLEAR AUTO FIND Value"
     browse-order AT ROW 12.33 COL 7 HELP
          "Select Browser Sort Order" NO-LABEL
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 12.19 COL 3
     RECT-4 AT ROW 12.1 COL 2
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
         HEIGHT             = 12.52
         WIDTH              = 146.
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
/* BROWSE-TAB Browser-Table RECT-5 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

IF cSSScanVendor = "RMLot" THEN
    ASSIGN 
    loadtag.misc-char[2]:VISIBLE IN BROWSE Browser-Table = TRUE.
ELSE
    ASSIGN 
        loadtag.misc-char[2]:VISIBLE IN BROWSE Browser-Table = FALSE.

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
     _TblList          = "asi.rm-rctd,asi.loadtag WHERE asi.rm-rctd ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST"
     _Where[1]         = "rm-rctd.company EQ cocode
AND rm-rctd.rita-code EQ ""R""
AND rm-rctd.tag NE """"
AND rm-rctd.qty GE 0"
     _Where[2]         = "asi.loadtag.company eq rm-rctd.company and
loadtag.item-type = yes and
loadtag.tag-no eq rm-rctd.tag and
loadtag.misc-char[1] begins lv-search"
     _FldNameList[1]   > asi.rm-rctd.r-no
"rm-rctd.r-no" "Seq#" ? "integer" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.rm-rctd.tag
"rm-rctd.tag" "Tag#" "x(20)" "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > asi.loadtag.misc-char[1]
"loadtag.misc-char[1]" "Vendor Tag #" "x(21)" "character" ? ? ? 14 ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > asi.rm-rctd.loc
"rm-rctd.loc" "Whse" "x(13)" "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > asi.rm-rctd.loc-bin
"rm-rctd.loc-bin" "Bin" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > asi.rm-rctd.rct-date
"rm-rctd.rct-date" ? ? "date" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > asi.rm-rctd.po-no
"rm-rctd.po-no" ? "x(6)" "character" ? ? ? 14 ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > asi.rm-rctd.job-no
"rm-rctd.job-no" "Job" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   = asi.rm-rctd.job-no2
     _FldNameList[10]   > asi.rm-rctd.s-num
"rm-rctd.s-num" "S" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > asi.rm-rctd.i-no
"rm-rctd.i-no" "Item" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > asi.rm-rctd.i-name
"rm-rctd.i-name" "Name/Desc" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > asi.rm-rctd.qty
"rm-rctd.qty" "Qty" "->,>>>,>>9.9<<" "decimal" ? ? ? 14 ? ? yes ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > asi.rm-rctd.pur-uom
"rm-rctd.pur-uom" "UOM" "x(4)" "character" ? ? ? 14 ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > asi.rm-rctd.cost
"rm-rctd.cost" "Cost" ? "decimal" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > asi.rm-rctd.cost-uom
"rm-rctd.cost-uom" "UOM" "x(4)" "character" ? ? ? 14 ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"calc-ext-cost() @ ext-cost" "Ext.Amount" "->>>,>>9.99<<" ? 14 ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"display-dimension('W') @ lv-po-wid" "Width" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"display-dimension('L') @ lv-po-len" "Length" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > asi.rm-rctd.user-id
"rm-rctd.user-id" "User ID" ? "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON CURSOR-DOWN OF Browser-Table IN FRAME F-Main
DO:  
  RUN get-matrix (YES).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
 def var ll-tag# as log no-undo.
 DEF VAR help-recid AS RECID NO-UNDO.
 DEF VAR help-rowid AS ROWID NO-UNDO.

 ll-help-run = yes.

 case focus:name :
      when "po-no" then do:
           run windows/l-poordl.w (rm-rctd.company,focus:screen-value, output char-val).
           if char-val <> "" then do:
              assign rm-rctd.po-no:screen-value in browse {&BROWSE-NAME} = entry(1,char-val)
                     rm-rctd.i-no:screen-value in browse {&BROWSE-NAME} = entry(2,char-val)
                     rm-rctd.i-name:screen-value in browse {&BROWSE-NAME} = entry(3,char-val)
                     rm-rctd.job-no:screen-value in browse {&BROWSE-NAME} = entry(4,char-val)
                     rm-rctd.job-no2:screen-value in browse {&BROWSE-NAME} = entry(5,char-val)
                     .
             find po-ordl where po-ordl.company = rm-rctd.company and
                                po-ordl.po-no = integer(entry(1,char-val)) and
                                po-ordl.line = integer(entry(6,char-val))
                                no-lock no-error.
             if avail po-ordl then RUN update-from-po-line.

             else do:
                find first item where item.company = rm-rctd.company and
                                      item.i-no = entry(2,char-val)
                                      no-lock no-error.
                assign rm-rctd.pur-uom:screen-value in browse {&BROWSE-NAME} = item.cons-uom
                       rm-rctd.cost-uom:screen-value in browse {&BROWSE-NAME} = item.cons-uom.
             end.
             if not avail item then find first item where item.company = rm-rctd.company and
                                                          item.i-no = entry(2,char-val)
                                      no-lock no-error.
             
             assign rm-rctd.loc:screen-value in browse {&BROWSE-NAME} =  item.loc
                    rm-rctd.loc-bin:screen-value in browse {&BROWSE-NAME} =  item.loc-bin
                    .
             if rm-rctd.loc-bin:screen-value in browse {&BROWSE-NAME} eq "" then do:
                  find first sys-ctrl where sys-ctrl.company eq rm-rctd.company
                                          and sys-ctrl.name    eq "AUTOISSU"
                                no-lock no-error.
                  if not avail sys-ctrl then do:
                        create sys-ctrl.
                        assign sys-ctrl.company = rm-rctd.company
                                   sys-ctrl.name    = "AUTOISSU"
                                   sys-ctrl.descrip = "Automatically Issue RM Receipts to asi"
                                   sys-ctrl.log-fld = yes.
                        message "Sys-ctrl record NOT found. " sys-ctrl.descrip
                           update sys-ctrl.char-fld.
                  end.
                  assign rm-rctd.loc-bin:screen-value in browse {&BROWSE-NAME} = sys-ctrl.char-fld.
             end.
             run tag-method (output ll-tag#).
             if ll-tag# and rm-rctd.po-no:screen-value in browse {&BROWSE-NAME} <> ""
             then do:
                 run tag-sequence.
             end.
             ext-cost = 0.
             disp ext-cost with browse {&BROWSE-NAME}.
           end.  /* char-val <> "" */
           return no-apply.   
     end.
     when "i-no" then do:
         IF DEC(rm-rctd.po-no:SCREEN-VALUE) NE 0 THEN DO:
           RUN windows/l-poitmw.w (YES, rm-rctd.company,rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},
                                   YES, FOCUS:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},
                                   ROWID(rm-rctd), OUTPUT help-rowid).
           FIND po-ordl WHERE ROWID(po-ordl) EQ help-rowid NO-LOCK NO-ERROR.
           IF AVAIL po-ordl THEN DO:
              ASSIGN 
               FOCUS:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}           = po-ordl.i-no
               rm-rctd.i-name:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}  = po-ordl.i-name
               rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}  = po-ordl.job-no
               rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(po-ordl.job-no2).
              RUN update-from-po-line.
           END.
         END.

         ELSE
         IF rm-rctd.job-no:SCREEN-VALUE NE "" THEN DO:
                RUN windows/l-jobmat.w (rm-rctd.company,rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},
                                        rm-rctd.job-no2:SCREEN-VALUE,rm-rctd.i-no:SCREEN-VALUE, OUTPUT char-val, OUTPUT help-recid).
                IF help-recid <> ? THEN RUN DISPLAY-jobmat (help-recid).
         END.

         ELSE DO:
             /* company,industry,mat-type,i-code,i-no, output, output */
            run windows/l-itmRE.w (rm-rctd.company,"","","R",FOCUS:SCREEN-VALUE, output char-val,OUTPUT help-recid).
            if char-val <> "" then do :
               ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                      rm-rctd.i-name:SCREEN-VALUE = ENTRY(2,char-val).
               RUN display-item(help-recid).
            END.
         END.
         return no-apply.   
     end.
     when "s-num" then do:
         IF rm-rctd.po-no:SCREEN-VALUE NE "" THEN DO:
           RUN windows/l-poitmw.w (YES, rm-rctd.company,rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},
                                   YES, rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},
                                   ROWID(rm-rctd), OUTPUT help-rowid).
           FIND po-ordl WHERE ROWID(po-ordl) EQ help-rowid NO-LOCK NO-ERROR.
           IF AVAIL po-ordl THEN DO:
              ASSIGN 
               rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}    = po-ordl.i-no
               rm-rctd.i-name:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}  = po-ordl.i-name
               rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}  = po-ordl.job-no
               rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(po-ordl.job-no2).
              RUN update-from-po-line.
           END.
         END.
         return no-apply.   
     end.
     when "job-no" or when "job-no2" then do:
         IF DEC(rm-rctd.po-no:SCREEN-VALUE) EQ 0 THEN DO:
           run windows/l-jobno.w (rm-rctd.company,rm-rctd.job-no:screen-value, output char-val, OUTPUT help-recid).
           if char-val <> "" then do :
              assign /*focus:screen-value in frame {&frame-name} = entry(1,char-val)
                     */ 
                     rm-rctd.job-no:screen-value = entry(1,char-val)
                     rm-rctd.job-no2:screen-value = entry(2,char-val).
           end.
         END.
         ELSE DO:
           run windows/l-pojob.w (rm-rctd.company,rm-rctd.po-no:screen-value,rm-rctd.i-no:screen-value, output char-val).
           if char-val <> "" then do :
              assign /*focus:screen-value in frame {&frame-name} = entry(1,char-val)
                     */ 
                     rm-rctd.job-no:screen-value = entry(1,char-val)
                     rm-rctd.job-no2:screen-value = entry(2,char-val).
           end.
         END.
         return no-apply.   
     end.  
     when "loc" then do:
           run rm/l-loc.w (rm-rctd.company,focus:screen-value, output char-val).
           if char-val <> "" then do :
              assign focus:screen-value in  browse {&BROWSE-NAME}  = entry(1,char-val)
                     /*rm-rctd.loc-bin:screen-value in browse {&BROWSE-NAME} = entry(2,char-val) */
                     .
             
           end.
           return no-apply.   
     end.
     when "loc-bin" then do:
           run rm/l-locbin.w (rm-rctd.company,rm-rctd.loc:screen-value, output char-val).
           if char-val <> "" then do :
              assign focus:screen-value  = entry(1,char-val)
                     rm-rctd.loc:screen-value = entry(2,char-val)
                    /* rm-rctd.qty:screen-value = entry(3,char-val)
                     rm-rctd.tag:screen-value = entry(4,char-val)*/
                     .
             
           end.
           return no-apply.   
     end.
   WHEN "tag" THEN DO:
         run windows/l-ldtag.w (g_company,yes,focus:screen-value,output char-val,OUTPUT HELP-recid).
         if char-val <> "" then do :
            tag:SCREEN-VALUE = ENTRY(1,char-val).
            /*  ===*/
            FIND FIRST br-tmp WHERE br-tmp.company = g_company AND
                       br-tmp.tag = SELF:SCREEN-VALUE
                      AND RECID(br-tmp) <> RECID(rm-rctd)
                      NO-LOCK NO-ERROR.
            IF AVAIL br-tmp THEN DO:
               MESSAGE "This Tag Number Has Already Been Used." skip
                       "Please Enter A Unique Tag Number." 
                       VIEW-AS ALERT-BOX ERROR.
               RETURN NO-APPLY.
            END.
            ELSE DO:
               
               find first xrm-rdtlh where xrm-rdtlh.company   eq g_company
                    and xrm-rdtlh.loc       eq rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                    and xrm-rdtlh.tag       eq rm-rctd.tag:SCREEN-VALUE
                    and xrm-rdtlh.qty       gt 0
                    and xrm-rdtlh.rita-code ne "S" 
                    use-index tag no-lock no-error.
               if avail xrm-rdtlh THEN  DO:
                  MESSAGE "This Tag Number Has Already Been Used." skip
                          "Please Enter A Unique Tag Number." 
                          VIEW-AS ALERT-BOX ERROR.
                  RETURN NO-APPLY.
               END.
            END.
            {addon/loadtags/disptagr.i "RMItem" rm-rctd.tag:SCREEN-VALUE}
            FIND FIRST bpo-ordl NO-LOCK
                 WHERE bpo-ordl.company EQ g_company
                   AND bpo-ordl.po-no EQ loadtag.po-no
                   AND bpo-ordl.job-no EQ loadtag.job-no
                   AND bpo-ordl.job-no2 EQ loadtag.job-no2
                   AND bpo-ordl.i-no EQ loadtag.i-no
                   AND bpo-ordl.s-num EQ loadtag.form-no NO-ERROR.
            IF AVAILABLE bpo-ordl THEN
               rm-rctd.s-num:SCREEN-VALUE = STRING(bpo-ordl.s-num).
            
            RETURN NO-APPLY.
         END.
     END.
   end case.

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
 /*  {src/adm/template/brsleave.i}  */
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


&Scoped-define SELF-NAME rm-rctd.tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.tag IN BROWSE Browser-Table /* Tag# */
DO:
    IF LASTKEY = -1 THEN RETURN.

    FIND FIRST br-tmp WHERE br-tmp.company = g_company 
                        AND br-tmp.tag = SELF:SCREEN-VALUE
                        AND SELF:SCREEN-VALUE <> ""
                        AND br-tmp.rita-code <> "P"
                        AND RECID(br-tmp) <> RECID(rm-rctd)
                        NO-LOCK NO-ERROR.
    IF AVAIL br-tmp THEN DO:
       MESSAGE "This Tag Number Has Already Been Used." skip
               "Please Enter A Unique Tag Number." 
           VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
    ELSE DO:
          find first xrm-rdtlh
                 where xrm-rdtlh.company   eq g_company
                   and xrm-rdtlh.loc       eq rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                   and xrm-rdtlh.tag       eq rm-rctd.tag:SCREEN-VALUE
                   AND xrm-rdtlh.tag <> ""
                   and xrm-rdtlh.qty       gt 0
                   and xrm-rdtlh.rita-code ne "S"
                 use-index tag no-lock no-error.
          if avail xrm-rdtlh THEN  DO:
                 MESSAGE "This Tag Number Has Already Been Used." skip
                         "Please Enter A Unique Tag Number." 
                         VIEW-AS ALERT-BOX ERROR.
                 RETURN NO-APPLY.
          END.
        
        RUN valid-tag NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        
        {addon/loadtags/disptagr.i "RMItem" rm-rctd.tag:SCREEN-VALUE}
        FIND FIRST bpo-ordl NO-LOCK
             WHERE bpo-ordl.company EQ g_company
               AND bpo-ordl.po-no EQ loadtag.po-no
               AND bpo-ordl.job-no EQ loadtag.job-no
               AND bpo-ordl.job-no2 EQ loadtag.job-no2
               AND bpo-ordl.i-no EQ loadtag.i-no 
               AND bpo-ordl.s-num EQ loadtag.form-no NO-ERROR.
        IF AVAILABLE bpo-ordl THEN
           rm-rctd.s-num:SCREEN-VALUE = STRING(bpo-ordl.s-num).
        
        loadtag.misc-char[1]:SCREEN-VALUE = loadtag.misc-char[1].
        loadtag.misc-char[2]:SCREEN-VALUE = loadtag.misc-char[2].

        APPLY "leave" TO rm-rctd.i-no IN BROWSE {&browse-name}.
        
        IF NOT v-ssrmscan THEN do:
          APPLY "row-leave" TO BROWSE {&browse-name}.
          RETURN NO-APPLY.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.loc IN BROWSE Browser-Table /* Whse */
DO:
    DEF VAR v-auto-rec-log AS LOG NO-UNDO.

    DO v-prg-idx = 1 TO 5:
       IF INDEX(PROGRAM-NAME(v-prg-idx),"create-rec-from-vend-tag") > 0 THEN
       DO:
          v-auto-rec-log = YES.
          LEAVE.
       END.
    END.

    IF LASTKEY = -1 AND v-auto-rec-log = NO THEN RETURN.

    DEF VAR v-locbin AS cha NO-UNDO.
    IF SELF:MODIFIED THEN DO:
       IF LENGTH(SELF:SCREEN-VALUE) > 5 THEN DO:

          v-locbin = SELF:SCREEN-VALUE.
          ASSIGN rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name} = SUBSTRING(v-locbin,1,5)
                 rm-rctd.loc-bin:SCREEN-VALUE = SUBSTRING(v-locbin,6,8).

          FIND FIRST loc WHERE loc.company = g_company
                        AND loc.loc = rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                        NO-LOCK NO-ERROR.
          IF NOT AVAIL loc THEN DO:
             MESSAGE "Invalid Warehouse. Try Help. " VIEW-AS ALERT-BOX ERROR.
             RETURN NO-APPLY.
          END.
          FIND FIRST rm-bin WHERE rm-bin.company = g_company
                           AND rm-bin.i-no = ""
                           AND rm-bin.loc = rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                           AND rm-bin.loc-bin = rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
          IF NOT AVAIL rm-bin THEN DO:
             MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.
             APPLY "entry" TO rm-rctd.loc .
             RETURN NO-APPLY.
          END.

          APPLY "leave" TO SELF.
          APPLY "tab" TO rm-rctd.loc-bin IN BROWSE {&browse-name}.
          /*APPLY "row-leave" TO BROWSE {&browse-name}.*/

          RETURN NO-APPLY.
       END.
    END.
    ELSE DO:
        FIND FIRST loc WHERE loc.company = g_company
                        AND loc.loc = rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                        NO-LOCK NO-ERROR.
        IF NOT AVAIL loc THEN DO:
             MESSAGE "Invalid Warehouse. Try Help. " VIEW-AS ALERT-BOX ERROR.
             RETURN NO-APPLY.
        END.
    END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc-bin Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.loc-bin IN BROWSE Browser-Table /* Bin */
DO:
  DEF VAR v-auto-rec-log AS LOG NO-UNDO.

  DO v-prg-idx = 1 TO 5:
     IF INDEX(PROGRAM-NAME(v-prg-idx),"create-rec-from-vend-tag") > 0 THEN
     DO:
        v-auto-rec-log = YES.
        LEAVE.
     END.
  END.

  IF LASTKEY = -1 AND v-auto-rec-log EQ NO THEN RETURN .

  IF SELF:MODIFIED THEN DO:
       FIND FIRST rm-bin WHERE rm-bin.company = g_company
                           AND rm-bin.i-no = ""
                           AND rm-bin.loc = rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                           AND rm-bin.loc-bin = rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
       IF NOT AVAIL rm-bin THEN DO:
          MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
       END.
   END.  
   IF SSPostFGVT-log EQ ? THEN DO:
        RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
        RUN auto-save IN WIDGET-HANDLE(char-hdl).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.po-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.po-no IN BROWSE Browser-Table /* PO# */
DO:

  DEF VAR v-auto-rec-log AS LOG NO-UNDO.

  DO v-prg-idx = 1 TO 5:
     IF INDEX(PROGRAM-NAME(v-prg-idx),"create-rec-from-vend-tag") > 0 THEN
     DO:
        v-auto-rec-log = YES.
        LEAVE.
     END.
  END.

  IF LASTKEY NE -1 OR v-auto-rec-log EQ YES THEN DO:
    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE 0 AND
       {&self-name}:MODIFIED                                         THEN DO:

      RUN display-po-job.
      IF lv-rowid EQ ? THEN RUN find-exact-po.

      FIND po-ordl WHERE ROWID(po-ordl) EQ lv-rowid NO-LOCK NO-ERROR.

      IF NOT AVAIL po-ordl THEN
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(SELF:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.

      IF AVAIL po-ordl THEN DO:
        lv-rowid = ROWID(po-ordl).
        RUN display-po-info.
      END.
    END.

    RUN valid-po-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.po-no Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.po-no IN BROWSE Browser-Table /* PO# */
DO:
  /*
  ll-warned = NO.
  IF INT({&self-name}:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) EQ 0 THEN
    {&self-name}:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = "".
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.job-no IN BROWSE Browser-Table /* Job */
DO:
   DEF VAR v-auto-rec-log AS LOG NO-UNDO.

   DO v-prg-idx = 1 TO 5:
      IF INDEX(PROGRAM-NAME(v-prg-idx),"create-rec-from-vend-tag") > 0 THEN
      DO:
         v-auto-rec-log = YES.
         LEAVE.
      END.
   END.

  IF LASTKEY NE -1 OR v-auto-rec-log THEN DO:
    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE 0 AND
       {&self-name}:MODIFIED                                         THEN DO:
      RUN find-exact-po.

      FIND po-ordl WHERE ROWID(po-ordl) EQ lv-rowid NO-LOCK NO-ERROR.

      IF NOT AVAIL po-ordl THEN
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.job-no    EQ {&self-name}:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.

      IF AVAIL po-ordl THEN DO:
        lv-rowid = ROWID(po-ordl).
        RUN display-po-info.
      END.
    END.

    RUN valid-job-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.job-no IN BROWSE Browser-Table /* Job */
DO:
  ll-warned = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.job-no2 IN BROWSE Browser-Table
DO:
    DEF VAR v-auto-rec-log AS LOG NO-UNDO.

    DO v-prg-idx = 1 TO 5:
       IF INDEX(PROGRAM-NAME(v-prg-idx),"create-rec-from-vend-tag") > 0 THEN
       DO:
          v-auto-rec-log = YES.
          LEAVE.
       END.
    END.

    IF LASTKEY NE -1 OR v-auto-rec-log THEN DO:
    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE 0 AND
       {&self-name}:MODIFIED                                         THEN DO:
      RUN find-exact-po.

      FIND po-ordl WHERE ROWID(po-ordl) EQ lv-rowid NO-LOCK NO-ERROR.

      IF NOT AVAIL po-ordl THEN
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND po-ordl.job-no2   EQ INT({&self-name}:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.

      IF AVAIL po-ordl THEN DO:
        lv-rowid = ROWID(po-ordl).
        RUN display-po-info.
      END.
    END.

    RUN valid-job-no2 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.job-no2 IN BROWSE Browser-Table
DO:
  ll-warned = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.s-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.s-num Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.s-num IN BROWSE Browser-Table /* S */
DO:
  DEF VAR v-auto-rec-log AS LOG NO-UNDO.

  DO v-prg-idx = 1 TO 5:
     IF INDEX(PROGRAM-NAME(v-prg-idx),"create-rec-from-vend-tag") > 0 THEN
     DO:
        v-auto-rec-log = YES.
        LEAVE.
     END.
  END.


  IF LASTKEY NE -1 OR v-auto-rec-log THEN DO:
    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE 0 AND
       {&self-name}:MODIFIED                                         THEN DO:
      RUN find-exact-po.

      FIND po-ordl WHERE ROWID(po-ordl) EQ lv-rowid NO-LOCK NO-ERROR.

      IF NOT AVAIL po-ordl THEN
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.s-num     EQ INT({&self-name}:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.

      IF AVAIL po-ordl THEN DO:
        lv-rowid = ROWID(po-ordl).
        RUN display-po-info.
      END.
    END.

    RUN valid-s-num NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.s-num Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.s-num IN BROWSE Browser-Table /* S */
DO:
  ll-warned = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.i-no IN BROWSE Browser-Table /* Item */
DO:
  APPLY 'TAB' TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.i-no IN BROWSE Browser-Table /* Item */
DO:
    DEF VAR v-auto-rec-log AS LOG NO-UNDO.

    DO v-prg-idx = 1 TO 5:
       IF INDEX(PROGRAM-NAME(v-prg-idx),"create-rec-from-vend-tag") > 0 THEN
       DO:
          v-auto-rec-log = YES.
          LEAVE.
       END.
    END.

  IF LASTKEY NE -1 OR v-auto-rec-log THEN DO:
    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE 0 AND
       {&self-name}:MODIFIED                                         THEN DO:
      RUN find-exact-po.

      FIND po-ordl WHERE ROWID(po-ordl) EQ lv-rowid NO-LOCK NO-ERROR.

      IF NOT AVAIL po-ordl THEN
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.s-num     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.i-no      EQ {&self-name}:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.

      IF AVAIL po-ordl THEN DO:
        lv-rowid = ROWID(po-ordl).
        RUN display-po-info.
      END.
    END.

    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.i-no IN BROWSE Browser-Table /* Item */
DO:
  ll-warned = NO.
  FIND ITEM
      WHERE item.company EQ rm-rctd.company
        AND item.i-no    BEGINS rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
      NO-LOCK NO-ERROR.             
  IF AVAIL ITEM THEN DO:
    rm-rctd.cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = "0".
    RUN display-item (RECID(ITEM)).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.qty Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.qty IN BROWSE Browser-Table /* Qty */
DO:
   lv-entry-qty = DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&browse-name}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.qty Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.qty IN BROWSE Browser-Table /* Qty */
DO:
   IF lv-entry-qty NE DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&browse-name}) THEN
      RUN po-cost.

   IF LASTKEY NE -1 THEN DO WITH FRAME {&FRAME-NAME}:
      RUN valid-qty NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
      RUN get-matrix (NO).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.pur-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.pur-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.pur-uom IN BROWSE Browser-Table /* UOM */
DO:
   lv-entry-qty-uom = rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.pur-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.pur-uom IN BROWSE Browser-Table /* UOM */
DO:
  IF lv-entry-qty-uom NE rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name} THEN
     RUN po-cost.

  DEF VAR v-auto-rec-log AS LOG NO-UNDO.

  DO v-prg-idx = 1 TO 5:
     IF INDEX(PROGRAM-NAME(v-prg-idx),"create-rec-from-vend-tag") > 0 THEN
     DO:
        v-auto-rec-log = YES.
        LEAVE.
     END.
  END.  

  IF LASTKEY NE -1 OR v-auto-rec-log THEN DO:
    RUN valid-uom (1) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN get-matrix (NO).

    IF NOT ll-qty-valid THEN DO:
      {rm/chkporun.i}
      ll-qty-valid = YES.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.cost Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.cost IN BROWSE Browser-Table /* Cost */
DO:
   lv-save-fld[1] = rm-rctd.cost:SCREEN-VALUE IN BROWSE {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.cost Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.cost IN BROWSE Browser-Table /* Cost */
DO:
  RUN get-matrix (NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.cost-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.cost-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.cost-uom IN BROWSE Browser-Table /* UOM */
DO:
   lv-save-fld[2] = rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.cost-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.cost-uom IN BROWSE Browser-Table /* UOM */
DO:
  DEF VAR v-auto-rec-log AS LOG NO-UNDO.

  DO v-prg-idx = 1 TO 5:
     IF INDEX(PROGRAM-NAME(v-prg-idx),"create-rec-from-vend-tag") > 0 THEN
     DO:
        v-auto-rec-log = YES.
        LEAVE.
     END.
  END.

  IF LASTKEY NE -1 OR v-auto-rec-log THEN DO:
    RUN valid-uom (2) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    /*ll-add-setup = DEC(rm-rctd.cost:SCREEN-VALUE IN BROWSE {&browse-name}) NE
                       DEC(lv-save-fld[1]) OR
                   TRIM(rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}) NE
                       TRIM(lv-save-fld[2]).*/

    RUN get-matrix (NO).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/tag#.i}
ll-tag-meth = v-tag#.

/* gdm - 09190805 */
{custom/yellowColumns.i}

  
    FIND FIRST sys-ctrl NO-LOCK
       WHERE sys-ctrl.company EQ gcompany
         AND sys-ctrl.name    EQ "LOADTAG" NO-ERROR.

  IF NOT AVAIL sys-ctrl THEN
     DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN
         sys-ctrl.company  = gcompany
         sys-ctrl.name     = "LOADTAG"
         sys-ctrl.descrip  = "Special Load tag print options, e.g. barcode printer"
         sys-ctrl.char-fld = "ASI".
        MESSAGE "System control record NOT found. Please enter the load tag option"
                UPDATE sys-ctrl.char-fld.
        FIND CURRENT sys-ctrl NO-LOCK.
    END.

  ASSIGN
     v-loadtag = sys-ctrl.char-fld.


  PROCEDURE mail EXTERNAL "xpMail.dll" :
        DEF INPUT PARAM mailTo AS CHAR.
        DEF INPUT PARAM mailsubject AS CHAR.
        DEF INPUT PARAM mailText AS CHAR.
        DEF INPUT PARAM mailFiles AS CHAR.
        DEF INPUT PARAM mailDialog AS LONG.
        DEF OUTPUT PARAM retCode AS LONG.
  END.


&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assign-prep-info B-table-Win 
PROCEDURE assign-prep-info :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER bf-tt-rctd FOR tt-rctd.
FOR EACH bf-tt-rctd   
    WHERE bf-tt-rctd.seq-no EQ tt-rctd.seq-no 
      AND bf-tt-rctd.i-no   EQ tt-rctd.i-no:

    FOR EACH prep WHERE prep.company EQ cocode                          
             AND prep.CODE = bf-tt-rctd.i-no:
        ASSIGN
            prep.loc            = bf-tt-rctd.loc
            prep.loc-bin        = bf-tt-rctd.loc-bin
            prep.received-date  = bf-tt-rctd.rct-date.
        IF bf-tt-rctd.job-no NE "" THEN
            ASSIGN
                prep.last-job-no    = bf-tt-rctd.job-no
                prep.last-job-no2   = bf-tt-rctd.job-no2.
    END. /* each prep */
END. /* each tt-rctd   */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE convert-vend-comp-curr B-table-Win 
PROCEDURE convert-vend-comp-curr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT-OUTPUT PARAMETER ip-cost AS DEC DECIMALS 4 NO-UNDO.
    
   FIND FIRST b-po-ord WHERE
        b-po-ord.company EQ po-ordl.company AND
        b-po-ord.po-no   EQ po-ordl.po-no
        NO-LOCK NO-ERROR.

   IF AVAIL b-po-ord THEN
   DO:
      FIND FIRST vend WHERE
           vend.company EQ po-ord.company AND
           vend.vend-no EQ po-ord.vend-no
           NO-LOCK NO-ERROR.

      IF AVAIL vend THEN
      DO:
         FIND FIRST b-company WHERE
              b-company.company EQ cocode
              NO-LOCK.

         IF vend.curr-code NE b-company.curr-code THEN
         DO:
            FIND FIRST currency WHERE
                 currency.company EQ cocode AND
                 currency.c-code EQ vend.curr-code
                 NO-LOCK NO-ERROR.

            IF AVAIL currency THEN
            DO:
               ip-cost = ip-cost * currency.ex-rate.

               RELEASE currency.
            END.
         END.

         RELEASE b-company.
         RELEASE vend.
      END.

      RELEASE b-po-ord.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-rec-from-vend-tag B-table-Win 
PROCEDURE create-rec-from-vend-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-loadtag-no AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-vendtag-no AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-cons-uom AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-cost AS DEC NO-UNDO.
   DEFINE INPUT PARAMETER ip-pr-uom AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-po-type AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-setup AS DEC NO-UNDO.
   DEFINE INPUT PARAMETER ip-rec-qty AS DEC NO-UNDO.
   DEFINE INPUT PARAMETER ip-add-setup AS LOG NO-UNDO.

   DEF BUFFER b-loadtag FOR loadtag.

   DEF VAR char-hdl AS CHAR NO-UNDO.
   DEF VAR i AS INT NO-UNDO.
   DEF VAR runWhichPost AS cha NO-UNDO.
   def var v-po-no like rm-rctd.po-no NO-UNDO.
   DEF VAR v-autoissue AS LOG NO-UNDO.
   DEF VAR ld AS DEC NO-UNDO.
   DEF BUFFER b-tt-rctd FOR tt-rctd.
   DEF BUFFER b-item FOR ITEM.
   DEF VAR v-ext-cost AS DEC NO-UNDO.
   DEF VAR v-create-issue AS LOG NO-UNDO.
   DEF VAR lriLastRec AS ROWID NO-UNDO.

   find first sys-ctrl where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "AUTOISSU" no-lock no-error.
   v-autoissue = IF AVAIL sys-ctrl THEN sys-ctrl.log-fld ELSE NO.
   v-create-issue = v-autoissue.

   IF v-ssrmscan AND NOT SSPostFGVT-log THEN
   DO:
      RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
      RUN auto-add IN WIDGET-HANDLE(char-hdl).
      
      ASSIGN
         rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} = ip-loadtag-no
         loadtag.misc-char[1]:SCREEN-VALUE IN BROWSE {&browse-name} = ip-vendtag-no.
      
      RUN leave-tag-proc(INPUT ip-rec-qty).

      IF cSSScanVendor = "RMLot" THEN
          APPLY "ENTRY" TO loadtag.misc-char[2] IN BROWSE {&browse-name}.
      ELSE
         APPLY "ENTRY" TO rm-rctd.loc IN BROWSE {&browse-name}.
   END.
   ELSE
   DO:
      RUN sys/ref/asiseq.p (INPUT cocode, INPUT "rm_rcpt_seq", OUTPUT i) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
        MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
      
      FIND FIRST b-loadtag WHERE
           b-loadtag.company EQ cocode AND
           b-loadtag.item-type EQ YES AND
           b-loadtag.tag-no EQ ip-loadtag-no
           NO-LOCK NO-ERROR.

      CREATE rm-rctd.
      ASSIGN
       rm-rctd.r-no       = i
       rm-rctd.rct-date   = TODAY
       rm-rctd.company    = cocode
       rm-rctd.rita-code  = "R"
       rm-rctd.i-name     = b-loadtag.i-name
       rm-rctd.i-no       = b-loadtag.i-no
       rm-rctd.job-no     = b-loadtag.job-no
       rm-rctd.job-no2    = b-loadtag.job-no2
       rm-rctd.po-no      = STRING(b-loadtag.po-no)
       rm-rctd.po-line    = b-loadtag.line
       rm-rctd.s-num      = b-loadtag.form-no
       rm-rctd.b-num      = b-loadtag.blank-no
       rm-rctd.qty        = ip-rec-qty
       rm-rctd.pur-uom    = ip-cons-uom
       rm-rctd.cost       = ip-cost
       rm-rctd.cost-uom   = ip-pr-uom
       rm-rctd.loc        = b-loadtag.loc
       rm-rctd.loc-bin    = b-loadtag.loc-bin
       rm-rctd.tag        = b-loadtag.tag-no
       rm-rctd.user-id  = USERID("nosweat")
       rm-rctd.upd-date = TODAY
       rm-rctd.upd-time = TIME. 
      
      lriLastRec = ROWID(rm-rctd).
      RELEASE rm-bin.
      IF b-loadtag.po-no EQ 0 THEN
         FIND FIRST rm-bin NO-LOCK
             WHERE rm-bin.company EQ cocode
               AND rm-bin.loc     EQ b-loadtag.loc
               AND rm-bin.i-no    EQ b-loadtag.i-no
               AND rm-bin.loc-bin EQ b-loadtag.loc-bin
             NO-ERROR.
      
      IF AVAIL rm-bin THEN DO:
        FIND FIRST item NO-LOCK
            WHERE item.company EQ rm-rctd.company
              AND item.i-no    EQ rm-rctd.i-no
            USE-INDEX i-no NO-ERROR.
        ASSIGN
         rm-rctd.cost     = rm-bin.cost
         rm-rctd.cost-uom = item.cons-uom.
      END.
      
      RUN get-matrix-2(INPUT ip-setup,
                       INPUT ip-po-type,
                       INPUT ip-add-setup).
     
      FIND CURRENT rm-rctd NO-LOCK NO-ERROR.
      IF SSPostFGVT-log THEN DO:
         EMPTY TEMP-TABLE tt-rctd.
         CREATE tt-rctd.
         BUFFER-COPY rm-rctd TO tt-rctd
         ASSIGN tt-rctd.rm-row-id = ROWID(rm-rctd)
                 tt-rctd.has-rec   = YES
                 tt-rctd.seq-no    = 1.

         v-ext-cost = tt-rctd.cost * tt-rctd.qty.
         find first costtype no-lock
          where costtype.company   eq cocode
            and costtype.cost-type eq item.cost-type
          no-error.
         IF rmpostgl AND AVAIL costtype AND costtype.inv-asset NE ""  AND
            v-ext-cost NE 0 AND v-ext-cost NE ?                       THEN DO:

           if tt-rctd.rita-code EQ "R"  AND  
             costtype.ap-accrued NE "" THEN DO:

               /* Debit RM Asset */
               FIND FIRST work-gl WHERE work-gl.actnum EQ costtype.inv-asset NO-LOCK NO-ERROR.
               IF NOT AVAIL work-gl THEN DO:
                 CREATE work-gl.
                 work-gl.actnum = costtype.inv-asset.
               END.
               work-gl.debits = work-gl.debits + v-ext-cost.

               /* Credit RM AP Accrued */
               FIND FIRST work-gl WHERE work-gl.actnum EQ costtype.ap-accrued NO-LOCK NO-ERROR.
               IF NOT AVAIL work-gl THEN DO:
                  CREATE work-gl.
                  work-gl.actnum = costtype.ap-accrued.
               END.
               work-gl.credits = work-gl.credits + v-ext-cost.
           END.
         END.

         auto-issue:  /* copied from run-report in rm/r-rmte&p.w */
         for each tt-rctd where tt-rctd.rita-code eq "R"
                               and tt-rctd.job-no    ne "" no-lock,
                first ITEM where item.company eq cocode
                            and item.i-no    eq tt-rctd.i-no NO-LOCK:
                release po-ordl.
                v-po-no = trim(tt-rctd.po-no).
                if v-po-no ne "" then do:
                    do x = 1 to length(v-po-no):
                      if substr(v-po-no,x,1) lt "0" or
                         substr(v-po-no,x,1) gt "9" then next auto-issue.
                    end.

                    find first po-ordl
                        where po-ordl.company   eq cocode
                          and po-ordl.i-no      eq tt-rctd.i-no
                          and po-ordl.po-no     eq int(v-po-no)
                          and po-ordl.job-no    eq tt-rctd.job-no
                          and po-ordl.job-no2   eq tt-rctd.job-no2
                          and po-ordl.item-type eq yes
                        use-index item-ordno no-lock no-error.
                end.

                IF item.mat-type NE "I" OR AVAIL po-ordl THEN
                  IF (item.i-code EQ "E" AND
                    NOT AVAIL po-ordl)      OR
                   (item.i-code EQ "R" AND
                    NOT v-autoissue)        THEN NEXT auto-issue.
        
                EMPTY TEMP-TABLE tt-mat.
              
                RELEASE job.
                IF tt-rctd.job-no NE "" AND tt-rctd.s-num EQ ? THEN
                FIND FIRST job
                  WHERE job.company EQ cocode
                  AND job.job-no  EQ tt-rctd.job-no
                  AND job.job-no2 EQ tt-rctd.job-no2 NO-LOCK NO-ERROR.

                IF AVAIL job THEN DO:
                    ld = 0.
            
                    FOR EACH job-mat
                        WHERE job-mat.company EQ job.company
                          AND job-mat.job     EQ job.job
                          AND job-mat.job-no  EQ job.job-no
                          AND job-mat.job-no2 EQ job.job-no2
                          AND job-mat.rm-i-no EQ tt-rctd.i-no
                        NO-LOCK
                        BY job-mat.frm:
                      CREATE tt-mat.
                      ASSIGN
                       tt-mat.frm = job-mat.frm
                       tt-mat.qty = job-mat.qty
                       ld         = ld + job-mat.qty.
                    END.
            
                    FOR EACH tt-mat:
                      tt-mat.qty = tt-rctd.qty * (tt-mat.qty / ld).
                      IF tt-rctd.pur-uom EQ "EA" THEN DO:
                         {sys/inc/roundup.i tt-mat.qty} 
                      END.
                    END.
            
                    ld = 0.
                    FOR EACH tt-mat:
                      ld = ld + tt-mat.qty.
                    END.
            
                    IF ld NE tt-rctd.qty THEN
                    FOR EACH tt-mat:
                       tt-mat.qty = tt-mat.qty + (tt-rctd.qty - ld).
                       LEAVE.
                    END.
                END. /* avail job*/
                ELSE DO:
                  CREATE tt-mat.
                  ASSIGN tt-mat.frm = tt-rctd.s-num
                         tt-mat.qty = tt-rctd.qty.
                END.

                IF v-create-issue THEN DO:
                   FOR EACH tt-mat:

                      CREATE b-tt-rctd.
                      BUFFER-COPY tt-rctd EXCEPT rec_key TO b-tt-rctd
                      ASSIGN
                        b-tt-rctd.rita-code = "I"
                        b-tt-rctd.tt-row-id = ROWID(tt-rctd)
                        b-tt-rctd.seq-no    = 2
                        b-tt-rctd.s-num     = tt-mat.frm
                        b-tt-rctd.qty       = tt-mat.qty.
                      DELETE tt-mat.
                   END. /* FOR EACH tt-mat */
                END. /* IF v-create-issue  */

         END. /* for each tt-rctd  if index(v-types,"R") gt 0 */

         issue-adder-for-board:
         for each tt-rctd where tt-rctd.rita-code eq "I"
                    and tt-rctd.job-no    ne "" no-lock,
             first job where job.company eq cocode
                    and job.job-no  eq tt-rctd.job-no
                    and job.job-no2 eq tt-rctd.job-no2 no-lock,
             first ITEM where item.company  eq cocode
                      and item.i-no     eq tt-rctd.i-no
                      and item.mat-type eq "B" no-lock:

              IF AVAIL b-tt-rctd THEN DO:
                {rm/rm-addcr.i E b-tt-rctd b-tt-rctd b-}
                  ASSIGN b-tt-rctd.tt-row-id = ROWID(tt-rctd)
                         b-tt-rctd.seq-no    = 3.
                END.
              END.
         END.


         RUN post-rm.
         
      END.
      
      RUN local-open-query.
      
      IF SSPostFGVT-log EQ ? THEN DO:
        REPOSITION {&browse-name} TO ROWID lriLastRec NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN RUN dispatch ("row-changed").
        RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
        RUN auto-save IN WIDGET-HANDLE(char-hdl).

        IF cSSScanVendor = "RMLot" THEN
          APPLY "ENTRY" TO loadtag.misc-char[2] IN BROWSE {&browse-name}.
        ELSE
          APPLY "entry" TO rm-rctd.loc IN BROWSE {&BROWSE-NAME}.
        
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-item B-table-Win 
PROCEDURE display-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER ip-recid AS RECID NO-UNDO.


FIND ITEM WHERE RECID(ITEM) EQ ip-recid NO-LOCK NO-ERROR.

DO WITH FRAME {&FRAME-NAME}:
  IF AVAIL ITEM THEN DO:
    ASSIGN
     rm-rctd.i-name:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}  = item.i-name
     rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     = item.loc
     rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = item.loc-bin
     rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = item.cons-uom.

    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) EQ 0 THEN DO:
      {rm/avgcost.i}

      ASSIGN
       /*rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = item.cons-uom */
       rm-rctd.cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}    =
           IF DEC(rm-rctd.cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) EQ 0 THEN
             IF v-avgcost THEN STRING(item.avg-cost)
             ELSE STRING(item.last-cost)
           ELSE rm-rctd.cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
       rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = item.cons-uom. 
    END.

    ELSE RUN update-from-po-line.
  END.

  IF NOT checkWhsBin(cocode,rm-rctd.loc,rm-rctd.loc-bin) THEN
  DO:
    IF v-bin NE 'RMITEM' THEN
    ASSIGN
      rm-rctd.loc = SUBSTR(v-bin,1,5)
      rm-rctd.loc-bin= SUBSTR(v-bin,6).
    IF NOT checkWhsBin(cocode,rm-rctd.loc,rm-rctd.loc-bin) THEN
    DO:
      FIND FIRST loc NO-LOCK WHERE loc.company EQ cocode NO-ERROR.
      IF AVAILABLE loc THEN
      FIND FIRST rm-bin WHERE rm-bin.company EQ cocode
                          AND rm-bin.loc EQ loc.loc
                          AND rm-bin.i-no EQ '' NO-ERROR.
      ASSIGN
        rm-rctd.loc = IF AVAILABLE loc THEN loc.loc ELSE ''
        rm-rctd.loc-bin = IF AVAILABLE rm-bin THEN rm-bin.loc-bin ELSE ''.
    END.
  END.
  
  IF rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     EQ "" OR
     rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} EQ "" THEN DO:
    FIND FIRST cust
        WHERE cust.company EQ cocode
          AND cust.active  EQ "X" 
        NO-LOCK NO-ERROR.
    IF AVAIL cust THEN DO:
      FIND FIRST shipto
          WHERE shipto.company EQ cocode
            AND shipto.cust-no EQ cust.cust-no
          NO-LOCK NO-ERROR.
      IF AVAIL shipto THEN
        ASSIGN   
         rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     = shipto.loc
         rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = shipto.loc-bin.
    END.
  END.
END.

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
      FIND FIRST item 
          WHERE item.company EQ cocode
            AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          NO-LOCK NO-ERROR.

      IF AVAIL item THEN RUN display-item (RECID(item)).

      FIND job-mat WHERE RECID(job-mat) EQ ip-recid NO-LOCK.

      rm-rctd.s-num:SCREEN-VALUE = STRING(job-mat.frm).
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-po-info B-table-Win 
PROCEDURE display-po-info :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-rm-rctd FOR rm-rctd.
  
  DEF VAR lv-i-no LIKE rm-rctd.i-no NO-UNDO.
  DEF VAR lv-locode like locode.
  DEF VAR li-tag-seq as int.
  DEF VAR lv-cost AS DEC DECIMALS 10 NO-UNDO.

  FIND po-ordl WHERE ROWID(po-ordl) EQ lv-rowid NO-LOCK NO-ERROR.

  IF AVAIL po-ordl THEN DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     lv-i-no   = rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}

     rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     = po-ordl.i-no
     rm-rctd.i-name:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}   = po-ordl.i-name
     rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}   = po-ordl.job-no
     rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}  = STRING(po-ordl.job-no2)
     rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}    = STRING(po-ordl.s-num)
     rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = po-ordl.pr-uom
     lv-setup                                               = po-ordl.setup
     lv-cost                                                = po-ordl.cost
     ll-warned = NO.

    RUN po-cost.

    IF rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE lv-i-no THEN DO:
      FIND FIRST ITEM
          WHERE item.company EQ rm-rctd.company
            AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          NO-LOCK NO-ERROR. 
      IF AVAIL ITEM THEN RUN display-item (RECID(ITEM)).
    END.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-po-job B-table-Win 
PROCEDURE display-po-job :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li-po-cnt AS INT NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    lv-rowid = ?.
    FOR EACH  po-ordl
        WHERE po-ordl.company   EQ rm-rctd.company
          AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) 
          AND po-ordl.item-type EQ YES
          AND ((po-ordl.job-no  EQ FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) AND
                po-ordl.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})                                                                                    AND
                po-ordl.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) OR
               adm-adding-record)
        NO-LOCK:
      ASSIGN
       li-po-cnt = li-po-cnt + 1
       lv-rowid  = ROWID(po-ordl).
      IF li-po-cnt GT 1 THEN LEAVE.
    END.
 
    IF li-po-cnt GT 1 THEN DO:
      lv-rowid = ?.
      RUN windows/l-poitmw.w (YES, rm-rctd.company, rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},
                              YES, rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},
                              ROWID(rm-rctd), OUTPUT lv-rowid).
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE do-search B-table-Win 
PROCEDURE do-search :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAM ip-search AS cha NO-UNDO.

   DEF VAR char-hdl AS cha NO-UNDO.

   lv-search = ip-search.

  &scoped-define IAMWHAT Search

  {&open-query-{&browse-name}}  

  IF ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) = ? AND lv-search <> "" THEN
  DO:
     MESSAGE "Record not found beginning with '" + lv-search + "' !!!"
        VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.
  end.    
  IF NUM-RESULTS ("{&browse-name}") = 1 THEN DO:
     RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
     RUN auto-save IN WIDGET-HANDLE(char-hdl).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Final-steps B-table-Win 
PROCEDURE Final-steps :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-tt-rctd FOR tt-rctd.
  DEF BUFFER rec-rm-rdtlh FOR rm-rdtlh.
  DEF BUFFER rec-rm-rcpth FOR rm-rcpth.

  DEF VAR v-int AS INT NO-UNDO.
  DEF VAR v-qty-received AS DEC NO-UNDO.
  
  IF rm-rctd.rita-code EQ "I" AND TRIM(rm-rctd.tag) NE "" THEN
     FOR EACH rec-rm-rdtlh NO-LOCK
         WHERE rec-rm-rdtlh.company   EQ rm-rctd.company
           AND rec-rm-rdtlh.tag       EQ rm-rctd.tag
           AND rec-rm-rdtlh.rita-code EQ "R"
         USE-INDEX tag,
         FIRST rec-rm-rcpth
         WHERE rec-rm-rcpth.r-no      EQ rec-rm-rdtlh.r-no
           AND rec-rm-rdtlh.rita-code EQ rec-rm-rdtlh.rita-code
         NO-LOCK:
           
       IF rm-rctd.po-no EQ "" THEN rm-rctd.po-no = rec-rm-rcpth.po-no.
    
       IF rm-rctd.job-no EQ "" THEN
         ASSIGN
          rm-rctd.job-no = rec-rm-rcpth.job-no
          rm-rctd.job-no2 = rec-rm-rcpth.job-no2.
    
       LEAVE.
     END.

    IF v-rmtags-log AND TRIM(rm-rctd.tag) NE "" THEN do:
       FOR EACH wiptag WHERE wiptag.company = rm-rctd.company 
                         AND wiptag.rm-tag-no = rm-rctd.tag EXCLUSIVE-LOCK:
          ASSIGN
             wiptag.sts = "On Hand" .
       END.
    END.
  
  {rm/rm-rctd.i rm-rcpth rm-rdtlh rm-rctd} /* Create History Records */

  IF rm-rctd.rita-code eq "R" then
  DO:
    {rm/rmemails.i}      
  END.

  DELETE rm-rctd.

  FOR EACH b-tt-rctd WHERE b-tt-rctd.tt-row-id EQ ROWID(tt-rctd):
    v-int = 0.
  RUN sys/ref/asiseq.p (INPUT g_company, INPUT "rm_rcpt_seq", OUTPUT v-int) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
       VIEW-AS ALERT-BOX INFO BUTTONS OK.


    CREATE rm-rctd.
    BUFFER-COPY b-tt-rctd TO rm-rctd
    ASSIGN
     rm-rctd.r-no        = v-int
     b-tt-rctd.r-no      = rm-rctd.r-no
     b-tt-rctd.has-rec   = YES
     b-tt-rctd.rm-row-id = ROWID(rm-rctd).    

  END.

  DELETE tt-rctd.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE find-exact-po B-table-Win 
PROCEDURE find-exact-po :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST po-ordl
        WHERE po-ordl.company   EQ rm-rctd.company
          AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
          AND po-ordl.job-no    EQ FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
          AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
          AND po-ordl.s-num     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
          AND po-ordl.i-no      EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND po-ordl.item-type EQ YES
        NO-LOCK NO-ERROR.
    lv-rowid = IF AVAIL po-ordl THEN ROWID(po-ordl) ELSE ?.
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
  def input parameter ip-first-disp as log no-undo.
  def var v-len like po-ordl.s-len no-undo.
  def var v-wid like po-ordl.s-len no-undo.
  def var v-dep like po-ordl.s-len no-undo. 
  def var v-bwt like po-ordl.s-len no-undo.
  def var lv-out-qty LIKE rm-rctd.qty no-undo.
  def var lv-out-cost LIKE rm-rctd.cost no-undo.
  DEF VAR lv-qty-uom LIKE rm-rctd.pur-uom NO-UNDO.
  DEF VAR lv-cost-uom LIKE rm-rctd.cost-uom NO-UNDO.
  DEF VAR lv-cost AS DEC DECIMALS 10 NO-UNDO.


if ip-first-disp  and avail rm-rctd and rm-rctd.i-no <> "" then do: /* for row-display */
  find item  where item.company eq cocode                           /* no screen-value used */
                     and item.i-no  eq rm-rctd.i-no /*:screen-value in browse {&BROWSE-NAME}*/
                     use-index i-no no-error.
  IF NOT AVAIL item THEN LEAVE.

  IF item.cons-uom EQ "" THEN item.cons-uom = rm-rctd.pur-uom.

  assign
   lv-qty-uom  = item.cons-uom
   lv-cost-uom = item.cons-uom
   v-dep       = item.s-dep.

  find first po-ordl where po-ordl.company = rm-rctd.company
                       and po-ordl.po-no = integer(rm-rctd.po-no)
                       and po-ordl.i-no  = rm-rctd.i-no
                       and po-ordl.job-no = rm-rctd.job-no
                       and po-ordl.job-no2 = rm-rctd.job-no2
                       and po-ordl.item-type = yes 
                       and po-ordl.s-num = rm-rctd.s-num
                           no-lock no-error.
  /*if not avail po-ordl then return.  */

  if avail po-ordl then do:
     assign  v-len = po-ordl.s-len
             v-wid = po-ordl.s-wid
             v-bwt = 0.
     {rm/pol-dims.i}
  end.
  else do:
        find first job where job.company eq cocode
                         and job.job-no  eq rm-rctd.job-no
                         and job.job-no2 eq rm-rctd.job-no2
                no-lock no-error.
        if avail job then do :
             find first job-mat where job-mat.company eq cocode
                                  and job-mat.job     eq job.job
                                  and job-mat.i-no    eq rm-rctd.i-no
                                  and job-mat.frm     eq rm-rctd.s-num
                   no-lock no-error.
             if avail job-mat then assign v-len         = job-mat.len
                                          v-wid         = job-mat.wid
                                          v-bwt         = job-mat.basis-w
                                          .
        end.
        if v-len eq 0 then v-len = if avail item then item.s-len else 0.
        if v-wid eq 0 then v-wid = if avail item and item.r-wid ne 0 then item.r-wid else if avail item then item.s-wid else 0.
        if v-bwt eq 0 then v-bwt = if avail item then item.basis-w else 0.
        ASSIGN lv-qty-uom = rm-rctd.pur-uom
               lv-cost-uom = rm-rctd.cost-uom.
  end.
  run custom/convquom.p(cocode,
                        rm-rctd.pur-uom,
                        lv-qty-uom,
                        v-bwt,
                        v-len,
                        input v-wid,
                        input v-dep,
                        input rm-rctd.qty,
                        output lv-out-qty).

  IF rm-rctd.cost-uom EQ "L" THEN
    lv-out-cost = rm-rctd.cost / lv-out-qty.
  ELSE
    run custom/convcuom.p(cocode, rm-rctd.cost-uom, lv-cost-uom,                    
                          v-bwt, v-len, v-wid, v-dep,
                          rm-rctd.cost, output lv-out-cost).

  IF ll-add-setup AND lv-out-qty NE 0 THEN
     lv-out-cost = lv-out-cost + (lv-setup / lv-out-qty).

  ext-cost = ROUND(lv-out-qty * lv-out-cost,2).
end. /* ip-first */
/* ======================================================================= */
else if avail rm-rctd and rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} <> "" then do: /* in update mode - use screen-value */
  find item  where item.company eq cocode
                and item.i-no  eq rm-rctd.i-no:screen-value in browse {&BROWSE-NAME}
                      use-index i-no no-error.
  IF NOT AVAIL item THEN LEAVE.

  IF item.cons-uom EQ "" THEN item.cons-uom = rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}.

  assign
   lv-qty-uom  = item.cons-uom
   lv-cost-uom = item.cons-uom
   v-dep       = item.s-dep.

  find first po-ordl where po-ordl.company = rm-rctd.company
                       and po-ordl.po-no = integer(rm-rctd.po-no:screen-value in browse {&BROWSE-NAME})
                       and po-ordl.i-no  = rm-rctd.i-no:screen-value
                       and po-ordl.job-no = (rm-rctd.job-no:screen-value)
                       and po-ordl.job-no2 = integer(rm-rctd.job-no2:screen-value)
                       and po-ordl.item-type = yes
                       and po-ordl.s-num = integer(rm-rctd.s-num:screen-value)
                           no-lock no-error.
 /*
  if not avail po-ordl then return.
 */   
  if avail po-ordl then do:
     assign  v-len = po-ordl.s-len
             v-wid = po-ordl.s-wid
             v-bwt = 0.
     {rm/pol-dims.i}
  end.
  else do:
        find first job where job.company eq cocode
                         and job.job-no  eq rm-rctd.job-no:screen-value
                         and job.job-no2 eq integer(rm-rctd.job-no2:screen-value)
                no-lock no-error.
        if avail job then do :
             find first job-mat where job-mat.company eq cocode
                                  and job-mat.job     eq job.job
                                  and job-mat.i-no    eq rm-rctd.i-no:screen-value
                                  and job-mat.frm     eq integer(rm-rctd.s-num:screen-value)
                   no-lock no-error.
             if avail job-mat then assign v-len         = job-mat.len
                                          v-wid         = job-mat.wid
                                          v-bwt         = job-mat.basis-w
                                          .
        end.
        if v-len eq 0 then v-len = if avail item then item.s-len else 0.
        if v-wid eq 0 then v-wid = if avail item and item.r-wid ne 0 then item.r-wid else if avail item then item.s-wid else 0.
        if v-bwt eq 0 then v-bwt = if avail item then item.basis-w else 0.
        ASSIGN lv-qty-uom = item.cons-uom
               lv-cost-uom = ITEM.cons-uom .
  end.
  
  /* convert qty */
  run custom/convquom.p(cocode,
                        rm-rctd.pur-uom:screen-value in browse {&BROWSE-NAME} ,
                        lv-qty-uom,
                         v-bwt,
                         v-len,
                         input v-wid,
                         input v-dep,
                         input DEC(rm-rctd.qty:screen-value in browse {&BROWSE-NAME}),
                         output lv-out-qty).
  
  /* convert cost */
  IF rm-rctd.cost-uom:screen-value in browse {&BROWSE-NAME} EQ "L" THEN
    lv-out-cost = DEC(rm-rctd.cost:screen-value in browse {&BROWSE-NAME}) / lv-out-qty.
  ELSE
    run custom/convcuom.p(cocode,
                          rm-rctd.cost-uom:screen-value in browse {&BROWSE-NAME},
                          lv-cost-uom,
                          v-bwt, v-len, v-wid, v-dep,
                          rm-rctd.cost:screen-value in browse {&BROWSE-NAME}, output lv-out-cost).

  IF lv-out-qty  EQ ? THEN lv-out-qty  = 0.
  IF lv-out-cost EQ ? THEN lv-out-cost = 0.

  IF ll-add-setup AND lv-out-qty NE 0 THEN
     lv-out-cost = lv-out-cost + (lv-setup / lv-out-qty).

  ASSIGN ext-cost = ROUND(lv-out-qty * lv-out-cost,2)
         rm-rctd.cost:SCREEN-VALUE = STRING(lv-out-cost)
         rm-rctd.cost-uom:SCREEN-VALUE = lv-cost-uom
         rm-rctd.qty:SCREEN-VALUE = STRING(lv-out-qty)
         rm-rctd.pur-uom:SCREEN-VALUE = lv-qty-uom.

  disp ext-cost with browse {&BROWSE-NAME}.

end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-matrix-2 B-table-Win 
PROCEDURE get-matrix-2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-setup AS DEC NO-UNDO.
   DEFINE INPUT PARAMETER ip-po-type AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-add-setup AS LOG NO-UNDO. 

   DEFINE VARIABLE v-len LIKE po-ordl.s-len NO-UNDO.
   DEFINE VARIABLE v-wid LIKE po-ordl.s-len NO-UNDO.
   DEFINE VARIABLE v-dep LIKE po-ordl.s-len NO-UNDO. 
   DEFINE VARIABLE v-bwt LIKE po-ordl.s-len NO-UNDO.
   DEFINE VARIABLE lv-out-qty LIKE rm-rctd.qty NO-UNDO.
   DEFINE VARIABLE lv-out-cost LIKE rm-rctd.cost NO-UNDO.
   DEFINE VARIABLE lv-qty-uom LIKE rm-rctd.pur-uom NO-UNDO.
   DEFINE VARIABLE lv-cost-uom LIKE rm-rctd.cost-uom NO-UNDO.
  
   FIND item NO-LOCK WHERE item.company EQ cocode
                       AND item.i-no EQ rm-rctd.i-no
                       USE-INDEX i-no NO-ERROR.
   IF NOT AVAIL item THEN LEAVE.
  
   IF item.cons-uom EQ '' THEN
      item.cons-uom = rm-rctd.pur-uom.
  
   ASSIGN
     lv-qty-uom = item.cons-uom
     lv-cost-uom = item.cons-uom
     v-dep = item.s-dep.
  
   FIND FIRST po-ordl WHERE
        po-ordl.company EQ rm-rctd.company AND
        po-ordl.po-no EQ INTEGER(rm-rctd.po-no) AND
        po-ordl.i-no EQ rm-rctd.i-no AND
        po-ordl.job-no EQ rm-rctd.job-no AND
        po-ordl.job-no2 EQ rm-rctd.job-no2 AND
        po-ordl.item-type EQ YES AND
        po-ordl.s-num EQ rm-rctd.s-num
        NO-LOCK NO-ERROR.

   IF AVAIL po-ordl THEN
   DO:
     ASSIGN
       v-len = po-ordl.s-len
       v-wid = po-ordl.s-wid
       v-bwt = 0.
     {rm/pol-dims.i}
   END.
   ELSE
   DO:
      FIND FIRST job WHERE
           job.company EQ cocode AND
           job.job-no EQ rm-rctd.job-no AND
           job.job-no2 EQ rm-rctd.job-no2
           NO-LOCK NO-ERROR.
     
      IF AVAIL job THEN
      DO:
         FIND FIRST job-mat NO-LOCK WHERE
              job-mat.company EQ rm-rctd.company AND
              job-mat.job EQ job.job AND
              job-mat.i-no EQ rm-rctd.i-no AND
              job-mat.frm EQ rm-rctd.s-num NO-ERROR.

         IF AVAIL job-mat THEN
            ASSIGN 
               v-len = job-mat.len
               v-wid = job-mat.wid
               v-bwt = job-mat.basis-w.
      END.

      IF v-len EQ 0 THEN v-len = IF AVAIL item THEN item.s-len ELSE 0.
      IF v-wid EQ 0 THEN v-wid = IF AVAIL item AND item.r-wid NE 0 THEN item.r-wid
                            ELSE IF AVAIL item THEN item.s-wid ELSE 0.
      IF v-bwt EQ 0 THEN v-bwt = IF AVAIL item THEN item.basis-w ELSE 0.
      
      ASSIGN
        lv-qty-uom = item.cons-uom
        lv-cost-uom = item.cons-uom.
   END.
  
   /* convert qty */
   RUN custom/convquom.p(gcompany,rm-rctd.pur-uom,lv-qty-uom,v-bwt,v-len,
                   INPUT v-wid,INPUT v-dep,INPUT rm-rctd.qty,OUTPUT lv-out-qty).
  
   /* convert cost */
   IF rm-rctd.cost-uom EQ 'L' THEN
      lv-out-cost = DEC(rm-rctd.cost) / lv-out-qty.
   ELSE
      RUN custom/convcuom.p(gcompany,rm-rctd.cost-uom,lv-cost-uom,
                            v-bwt,v-len,v-wid,v-dep,rm-rctd.cost,
                            OUTPUT lv-out-cost).

   IF ip-add-setup AND ip-po-type NE "S" AND lv-out-qty NE 0 THEN
      lv-out-cost = lv-out-cost + (ip-setup / lv-out-qty).

   ASSIGN
     rm-rctd.cost = lv-out-cost
     rm-rctd.cost-uom = lv-cost-uom
     rm-rctd.qty = lv-out-qty
     rm-rctd.pur-uom = lv-qty-uom.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-remain-qty B-table-Win 
PROCEDURE get-remain-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
  DEF OUTPUT PARAM op-remain-qty AS DEC NO-UNDO.

  DEF VAR lv-rowid AS ROWID NO-UNDO.


  FIND po-ordl WHERE ROWID(po-ordl) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL po-ordl THEN DO:
    FIND FIRST item
        WHERE item.company EQ po-ordl.company
          AND item.i-no    EQ po-ordl.i-no
        NO-LOCK NO-ERROR.

    op-remain-qty = po-ordl.cons-qty.

    RUN windows/l-poitmw.w (NO, po-ordl.company, STRING(po-ordl.po-no),
                            YES, "", ROWID(rm-rctd), OUTPUT lv-rowid).

    FIND FIRST tt-report WHERE tt-report.rec-id EQ RECID(po-ordl) NO-ERROR.

    IF AVAIL tt-report THEN op-remain-qty = DEC(tt-report.key-01).

    IF po-ordl.cons-uom NE rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} AND AVAIL item THEN
      RUN sys/ref/convquom.p(po-ordl.cons-uom, rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},
                             item.basis-w, po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                             op-remain-qty, OUTPUT op-remain-qty).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gl-from-work B-table-Win 
PROCEDURE gl-from-work :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-run AS INT NO-UNDO.
  DEF INPUT PARAM ip-trnum AS INT NO-UNDO.
  
  def var credits as dec init 0 no-undo.
  def var debits as dec init 0 no-undo. 

  
  FIND FIRST period
      WHERE period.company EQ cocode
        AND period.pst     LE v-post-date
        AND period.pend    GE v-post-date
      NO-LOCK.

  for each work-gl 
      where (ip-run eq 1 and work-gl.job-no ne "")
         or (ip-run eq 2 and work-gl.job-no eq "")
      break by work-gl.actnum:
      
    assign
     debits  = debits  + work-gl.debits
     credits = credits + work-gl.credits.

    if last-of(work-gl.actnum) then do:
      create gltrans.
      assign
       gltrans.company = cocode
       gltrans.actnum  = work-gl.actnum
       gltrans.jrnl    = "RMPOST"
       gltrans.period  = period.pnum
       gltrans.tr-amt  = debits - credits
       gltrans.tr-date = v-post-date
       gltrans.tr-dscr = if work-gl.job-no NE "" then "RM Issue to Job"
                                                 else "RM Receipt"
       gltrans.trnum   = ip-trnum
       debits  = 0
       credits = 0.

      RELEASE gltrans.
    end.
  end.

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
  DEFINE OUTPUT PARAMETER op-in-update AS LOGICAL NO-UNDO.

  op-in-update = adm-brs-in-update OR adm-new-record.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE leave-i-no-proc B-table-Win 
PROCEDURE leave-i-no-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE 0 THEN DO:
      RUN find-exact-po.

      FIND po-ordl WHERE ROWID(po-ordl) EQ lv-rowid NO-LOCK NO-ERROR.

      IF NOT AVAIL po-ordl THEN
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.s-num     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.i-no      EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.

      IF AVAIL po-ordl THEN DO:
        lv-rowid = ROWID(po-ordl).
        RUN display-po-info.
      END.
    END.

    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE leave-tag-proc B-table-Win 
PROCEDURE leave-tag-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-rec-qty AS DEC NO-UNDO.

   IF CAN-FIND(FIRST br-tmp WHERE br-tmp.company = g_company 
                 AND br-tmp.tag = SELF:SCREEN-VALUE
                 AND SELF:SCREEN-VALUE <> ""
                 AND br-tmp.rita-code <> "P"
                 AND RECID(br-tmp) <> RECID(rm-rctd)) THEN
      DO:
         MESSAGE "This Tag Number Has Already Been Used." skip
                 "Please Enter A Unique Tag Number." 
            VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
      END.
    ELSE DO:
          find first xrm-rdtlh
                 where xrm-rdtlh.company   eq g_company
                   and xrm-rdtlh.loc       eq rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                   and xrm-rdtlh.tag       eq rm-rctd.tag:SCREEN-VALUE
                   AND xrm-rdtlh.tag <> ""
                   and xrm-rdtlh.qty       gt 0
                   and xrm-rdtlh.rita-code ne "S"
                 use-index tag no-lock no-error.
          if avail xrm-rdtlh THEN  DO:
                 MESSAGE "This Tag Number Has Already Been Used." skip
                         "Please Enter A Unique Tag Number." 
                         VIEW-AS ALERT-BOX ERROR.
                 RETURN NO-APPLY.
          END.

        RUN valid-tag NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        
        {addon/loadtags/disptagr3.i "RMItem" rm-rctd.tag:SCREEN-VALUE}

        FIND FIRST bpo-ordl NO-LOCK
             WHERE bpo-ordl.company EQ g_company
               AND bpo-ordl.po-no EQ loadtag.po-no
               AND bpo-ordl.job-no EQ loadtag.job-no
               AND bpo-ordl.job-no2 EQ loadtag.job-no2
               AND bpo-ordl.i-no EQ loadtag.i-no 
               AND bpo-ordl.s-num EQ loadtag.form-no NO-ERROR.
        IF AVAILABLE bpo-ordl THEN
           rm-rctd.s-num:SCREEN-VALUE = STRING(bpo-ordl.s-num).
        
        RUN leave-i-no-proc.

        /*IF NOT v-ssrmscan THEN do:
          APPLY "row-leave" TO BROWSE {&browse-name}.
          RETURN NO-APPLY.
        END.*/
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
    rm-rctd.po-no = rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
    rm-rctd.rct-date = DATE(rm-rctd.rct-date:SCREEN-VALUE)
    rm-rctd.job-no = rm-rctd.job-no:SCREEN-VALUE
    rm-rctd.job-no2 = INT(rm-rctd.job-no2:SCREEN-VALUE)
    rm-rctd.s-num = INT(rm-rctd.s-num:SCREEN-VALUE)
    rm-rctd.i-no = rm-rctd.i-no:SCREEN-VALUE
    rm-rctd.i-name = rm-rctd.i-name:SCREEN-VALUE
    rm-rctd.pur-uom = rm-rctd.pur-uom:SCREEN-VALUE
    rm-rctd.cost = DEC(rm-rctd.cost:SCREEN-VALUE)
    rm-rctd.cost-uom = rm-rctd.cost-uom:SCREEN-VALUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR li-nxt-r-no AS INT INIT 1 NO-UNDO.
 DEF BUFFER bf-rctd FOR rm-rctd.

 /* Code placed here will execute PRIOR to standard behavior. */
  RUN sys/ref/asiseq.p (INPUT cocode, INPUT "rm_rcpt_seq", OUTPUT li-nxt-r-no) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   rm-rctd.company   = cocode
   rm-rctd.r-no      = li-nxt-r-no
   rm-rctd.rita-code = "R".

  IF adm-adding-record THEN DO:
    ASSIGN
     rm-rctd.s-num = 0
     rm-rctd.s-num:screen-value in browse {&BROWSE-NAME} = "0"
     rm-rctd.rct-date = TODAY.

    find first sys-ctrl where sys-ctrl.company eq cocode
                          and sys-ctrl.name    eq "RMWHSBIN"
                          no-lock no-error.
    if not avail sys-ctrl then do:
      create sys-ctrl.
      assign sys-ctrl.company = cocode
                 sys-ctrl.name    = "RMWHSBIN"
                 sys-ctrl.descrip = "Default Location for RM Warehouse / Bin for RM Receipts"
                 sys-ctrl.log-fld = YES.
      message "Sys-ctrl record NOT found. " sys-ctrl.descrip
                 update sys-ctrl.char-fld.
    end.
    IF sys-ctrl.char-fld NE 'RMITEM' THEN
       ASSIGN
          rm-rctd.loc = SUBSTR(sys-ctrl.char-fld,1,5)
          rm-rctd.loc-bin = SUBSTR(sys-ctrl.char-fld,6).

    disp rm-rctd.rct-date rm-rctd.loc rm-rctd.loc-bin with browse {&BROWSE-NAME}.
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
DEF VAR v-tag LIKE rm-rdtlh.tag.
  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.
  v-tag = "".
  IF AVAIL rm-rctd THEN
      v-tag = rm-rctd.tag.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  IF v-tag > "" THEN DO:
      FIND FIRST loadtag WHERE loadtag.company = g_company
                     AND loadtag.item-type = YES
                     AND loadtag.tag-no = v-tag
                   EXCLUSIVE-LOCK NO-ERROR.

      IF AVAIL loadtag THEN
          DELETE loadtag.
  END.

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
  def var li as int no-undo.
  def var hd-next as widget-handle no-undo.

  /* Code placed here will execute PRIOR to standard behavior. */
  /*
  run get-link-handle in adm-broker-hdl (this-procedure,"record-target", output out-hd-lst).
  hd-post = widget-handle(out-hd-lst).  /* procedure */
  if valid-handle(widget-handle(out-hd-lst)) then do:
    assign
     hd-post-child = hd-post:current-window
     hd-post-child = hd-post-child:first-child  /* frame */
     hd-post-child = hd-post-child:first-child /* field-group */
     hd-post-child = hd-post-child:first-child  /* field */
     hd-post-child:sensitive = no.
  end.
  */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:


    APPLY "entry" TO rm-rctd.tag IN BROWSE {&BROWSE-NAME}.

    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY 'cursor-left' TO {&BROWSE-NAME}.
    END.

    {&BROWSE-NAME}:READ-ONLY = NO.
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
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-source",OUTPUT char-hdl).

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
  DEF VAR char-hdl AS CHAR NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN get-matrix (NO).

  RUN valid-tag NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-po-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  
  RUN validate-record NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-job-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-job-no2 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-s-num NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-qty NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  DO li = 1 TO 2:
    RUN valid-uom (li) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  END.

   /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE po-cost B-table-Win 
PROCEDURE po-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   def var v-len like po-ordl.s-len no-undo.
   def var v-wid like po-ordl.s-len no-undo.
   def var v-dep like po-ordl.s-len no-undo. 
   def var v-bwt like po-ordl.s-len no-undo.
   def var lv-out-qty LIKE rm-rctd.qty no-undo.
   DEF VAR lv-qty-uom LIKE rm-rctd.pur-uom NO-UNDO.
   DEF VAR lv-cost AS DEC DECIMALS 10 NO-UNDO.

   FIND FIRST ITEM where
        item.company eq cocode AND
        item.i-no eq rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        use-index i-no
        NO-LOCK no-error.

   IF NOT AVAIL item THEN
      LEAVE.

   assign
      lv-qty-uom  = IF AVAIL po-ordl THEN po-ordl.pr-qty-uom ELSE ""
      v-len = IF AVAIL po-ordl THEN po-ordl.s-len ELSE 0
      v-wid = IF AVAIL po-ordl THEN po-ordl.s-wid ELSE 0
      v-bwt = 0.

   {rm/pol-dims.i}

   IF rm-rctd.pur-uom:screen-value in browse {&browse-name} EQ lv-qty-uom THEN
      lv-out-qty = DEC(rm-rctd.qty:screen-value in browse {&browse-name}).
   ELSE
      run custom/convquom.p (INPUT cocode,
                           INPUT rm-rctd.pur-uom:screen-value in browse {&browse-name},
                           INPUT lv-qty-uom,
                           INPUT v-bwt,
                           INPUT v-len,
                           input v-wid,
                           input v-dep,
                           INPUT DEC(rm-rctd.qty:screen-value in browse {&browse-name}),
                           output lv-out-qty).

  IF AVAIL po-ordl THEN do:

   FIND FIRST po-ord WHERE
        po-ord.company EQ cocode AND
        po-ord.po-no EQ po-ordl.po-no
        NO-LOCK NO-ERROR.

   ll-add-setup = NO.

   IF lv-out-qty LT po-ordl.ord-qty THEN
      lv-cost = po-ordl.cost +
               (po-ordl.setup /
               ((po-ordl.t-cost - po-ordl.setup) / po-ordl.cost)).
   ELSE
      ASSIGN
         lv-cost = po-ordl.cost
         ll-add-setup = IF AVAIL po-ord AND po-ord.type NE "S" THEN YES
                        ELSE NO.
   
   rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = po-ordl.pr-uom.

   RUN rm/getpocst.p (BUFFER po-ordl,
                      rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name},
                      INPUT-OUTPUT lv-cost).
   
   RUN convert-vend-comp-curr(INPUT-OUTPUT lv-cost).
   RUN convert-vend-comp-curr(INPUT-OUTPUT lv-setup).     

   rm-rctd.cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(lv-cost).
  END. /* avail po-ordl */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-rm B-table-Win 
PROCEDURE post-rm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* --------------------------------------------------- rm/rm-post.p 10/94 rd  */
/* raw materials inventory control receipt maintenance                        */
/* -------------------------------------------------------------------------- */

def buffer xrm-rctd     for rm-rctd.
def buffer xrm-bin      for rm-bin.
def buffer b-rm-rctd    for rm-rctd.
def buffer b-item       for item.
def buffer b-po-ordl    for po-ordl.
def buffer b-job-mat    for job-mat.

DEF BUFFER b-rh FOR rm-rcpth.
DEF BUFFER b-rd FOR rm-rdtlh.

def var v-avg-cst   as log.
def var v-next_r-no like rm-rctd.r-no.
def var v_r-no like rm-rctd.r-no.
def var v-conv-qty as dec.
def var v-reduce-qty like po-ordl.ord-qty.
def var no-of-items as int no-undo.
def var ld-cvt-qty as dec no-undo.
def var v-trnum like gl-ctrl.trnum no-undo.

def var v-r-qty     as   dec                    no-undo.
def var v-i-qty     as   dec                    no-undo.
def var v-t-qty     as   dec                    no-undo.
def var cost        as   dec                    no-undo.
def var out-qty     as   dec                    no-undo.
def var v-bwt       like item.basis-w           no-undo.
def var v-len       like item.s-len             no-undo.
def var v-wid       like item.s-wid             no-undo.
def var v-dep       like item.s-dep             no-undo.
def var v-recid     as   recid                  no-undo.
DEF VAR li          AS   INT                    NO-UNDO.

DEF VAR v-rmemail-file AS cha NO-UNDO.

find first rm-ctrl where rm-ctrl.company eq cocode no-lock no-error.
v-avg-cst = rm-ctrl.avg-lst-cst.

    SESSION:SET-WAIT-STATE ("general").
    transblok:
    FOR EACH tt-rctd
        WHERE CAN-FIND(FIRST item WHERE item.company EQ cocode
                                    AND item.i-no    EQ tt-rctd.i-no)          
        BREAK BY tt-rctd.seq-no
              BY tt-rctd.i-no
              BY tt-rctd.r-no
              BY RECID(tt-rctd)
        
        TRANSACTION:

      RELEASE rm-rctd.
      RELEASE item.
      li = 0.

      DO WHILE (NOT AVAIL rm-rctd OR NOT AVAIL item) AND li LT 1000:
        li = li + 1.

        FIND rm-rctd EXCLUSIVE-LOCK WHERE ROWID(rm-rctd) EQ tt-rctd.rm-row-id
            NO-WAIT NO-ERROR.
      
        FIND FIRST item EXCLUSIVE-LOCK
            WHERE item.company EQ rm-rctd.company
              AND item.i-no    EQ rm-rctd.i-no
            USE-INDEX i-no NO-WAIT NO-ERROR.
      END.
      IF NOT AVAIL rm-rctd OR NOT AVAIL item THEN NEXT transblok.

      IF rm-rctd.rita-code EQ "I" AND INT(rm-rctd.po-no) NE 0 THEN
      FOR EACH xrm-rctd
          WHERE xrm-rctd.company   EQ cocode
            AND xrm-rctd.i-no      EQ rm-rctd.i-no
            AND xrm-rctd.rita-code EQ "R"
            AND xrm-rctd.po-no     EQ rm-rctd.po-no
            AND xrm-rctd.r-no      LT rm-rctd.r-no
          NO-LOCK:
            
        UNDO transblok, NEXT transblok.
      END.

      FIND FIRST job
          WHERE job.company EQ rm-rctd.company
            AND job.job-no  EQ FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no))) +
                               TRIM(rm-rctd.job-no)
            AND job.job-no2 EQ rm-rctd.job-no2
          NO-ERROR.

      /** Find Bin & if not avail then create it **/
      FIND FIRST rm-bin
          WHERE rm-bin.company EQ rm-rctd.company
            AND rm-bin.loc     EQ rm-rctd.loc
            AND rm-bin.i-no    EQ rm-rctd.i-no
            AND rm-bin.loc-bin EQ rm-rctd.loc-bin
            AND rm-bin.tag     EQ rm-rctd.tag
          NO-ERROR.
      IF NOT AVAIL rm-bin THEN DO:
        CREATE rm-bin.
        ASSIGN
         rm-bin.company = rm-rctd.company
         rm-bin.loc     = rm-rctd.loc
         rm-bin.loc-bin = rm-rctd.loc-bin
         rm-bin.tag     = rm-rctd.tag
         rm-bin.i-no    = rm-rctd.i-no.
      END. /* not avail rm-bin */

      ld-cvt-qty = rm-rctd.qty.

      IF rm-rctd.pur-uom NE item.cons-uom AND item.cons-uom NE "" THEN
        RUN sys/ref/convquom.p (rm-rctd.pur-uom, item.cons-uom,
                              item.basis-w,
                              (if item.r-wid eq 0 then item.s-len else 12), 
                              (if item.r-wid eq 0 then item.s-wid else item.r-wid),
                              item.s-dep,
                              ld-cvt-qty, OUTPUT ld-cvt-qty).
        
      if rm-rctd.rita-code eq "R" then do:        /** RECEIPTS **/
        {rm/rm-post.i "rm-bin.qty" "rm-bin.cost" "rm-rctd.qty" "rm-rctd.cost"}

        assign
         rm-bin.qty     = rm-bin.qty + ld-cvt-qty
         item.last-cost = rm-rctd.cost
         item.q-onh     = item.q-onh + ld-cvt-qty.

        {rm/rm-poupd.i 2}
          
        item.q-avail = item.q-onh + item.q-ono - item.q-comm.
      end. /* R */

      else
      if rm-rctd.rita-code eq "I" then do:  /** ISSUES **/

         IF rm-rctd.tag NE "" THEN
            FOR EACH b-rd FIELDS(r-no rita-code tag2) WHERE
                b-rd.company   EQ cocode AND
                b-rd.tag       EQ rm-rctd.tag AND
                b-rd.loc       EQ rm-rctd.loc AND
                b-rd.loc-bin   EQ rm-rctd.loc-bin AND
                b-rd.rita-code EQ "R" AND
                b-rd.tag2      NE ""
                NO-LOCK
                USE-INDEX tag,
                FIRST b-rh WHERE
                      b-rh.r-no EQ b-rd.r-no AND
                      b-rh.rita-code EQ b-rd.rita-code AND
                      b-rh.i-no      EQ rm-rctd.i-no
                      NO-LOCK:
           
                rm-rctd.tag2 = b-rd.tag2.
            END.

         if avail job and job.job-no ne "" then do:
            run rm/mkjobmat.p (recid(rm-rctd),rm-rctd.company, output v-recid).
              
            find job-mat where recid(job-mat) eq v-recid no-error.
              
            if not avail job-mat then do:
               bell.
               message " Job Mat Record not found for "
                       string(job.job-no + "-" + string(job.job-no2,"99") +
                              "  " + rm-rctd.i-no)
                       VIEW-AS ALERT-BOX.
               undo transblok, next transblok.
            end.
           
            assign
             v-bwt = job-mat.basis-w
             v-len = job-mat.len
             v-wid = job-mat.wid
             v-dep = item.s-dep.
           
            if v-len eq 0 then v-len = item.s-len.
           
            if v-wid eq 0 then
              v-wid = if item.r-wid ne 0 then item.r-wid else item.s-wid.
           
            if v-bwt eq 0 then v-bwt = item.basis-w.
           
            if index("RL",job.stat) ne 0 then job.stat = "W".
              
            {rm/rmmatact.i}            /* Create Actual Material */
              
            out-qty = rm-rctd.qty.
            IF rm-rctd.pur-uom NE job-mat.qty-uom AND rm-rctd.pur-uom NE "" THEN
               RUN sys/ref/convquom.p(rm-rctd.pur-uom, job-mat.qty-uom,
                                      v-bwt, v-len, v-wid, v-dep,
                                      rm-rctd.qty, output out-qty).
           
            cost = rm-rctd.cost.
            IF rm-rctd.pur-uom NE job-mat.sc-uom AND rm-rctd.pur-uom NE "" THEN
               RUN sys/ref/convcuom.p(rm-rctd.pur-uom, job-mat.sc-uom,
                                      v-bwt, v-len, v-wid, v-dep,
                                      rm-rctd.cost, OUTPUT cost).
           
            assign
             mat-act.qty-uom = job-mat.qty-uom
             mat-act.cost    = cost
             mat-act.qty     = mat-act.qty     + out-qty
             job-mat.qty-iss = job-mat.qty-iss + out-qty
             job-mat.qty-all = job-mat.qty-all - out-qty
             item.q-comm     = item.q-comm     - rm-rctd.qty.
              
            run sys/ref/convquom.p(rm-rctd.pur-uom, job-mat.sc-uom,
                                   v-bwt, v-len, v-wid, v-dep,
                                   rm-rctd.qty, output out-qty).
           
            mat-act.ext-cost = mat-act.ext-cost + (cost * out-qty).
           
            /* Don't relieve more than were allocated */
            if job-mat.qty-all lt 0 then do:
              run sys/ref/convquom.p(job-mat.qty-uom, rm-rctd.pur-uom,
                                     v-bwt, v-len, v-wid, v-dep,
                                     job-mat.qty-all, output out-qty).
              assign
               job-mat.qty-all = 0
               item.q-comm     = item.q-comm - out-qty.
            end.
           
            /*job-mat.all-flg = (job-mat.qty-all gt 0).*/
            if item.q-comm lt 0 then item.q-comm = 0.
           
            IF item.mat-type EQ "B" THEN RUN rm/rm-addcr.p (ROWID(rm-rctd)).
         end.
           
         find first rm-bin
             where rm-bin.company eq rm-rctd.company
               and rm-bin.loc     eq rm-rctd.loc
               and rm-bin.i-no    eq rm-rctd.i-no
               and rm-bin.loc-bin eq rm-rctd.loc-bin
               and rm-bin.tag     eq rm-rctd.tag
             no-error.
           
         assign
          rm-bin.qty     = rm-bin.qty - ld-cvt-qty
          item.q-onh     = item.q-onh - ld-cvt-qty
          item.qlast-iss = rm-rctd.qty
          item.dlast-iss = rm-rctd.rct-date
          item.q-ytd     = item.q-ytd + rm-rctd.qty
          item.q-ptd     = item.q-ptd + rm-rctd.qty
          item.u-ptd     = item.u-ptd + (rm-rctd.cost * rm-rctd.qty)
          item.u-ytd     = item.u-ytd + (rm-rctd.cost * rm-rctd.qty)
          item.q-avail   = item.q-onh + item.q-ono - item.q-comm.
      end.  /* I */

      else
      if rm-rctd.rita-code eq "A" then do:  /** ADJUSTMENTS **/
        if rm-rctd.cost ne 0 then do:
          {rm/rm-post.i "rm-bin.qty" "rm-bin.cost" "rm-rctd.qty" "rm-rctd.cost"}
        end.

        assign
         rm-bin.qty     = rm-bin.qty + ld-cvt-qty
         item.last-cost = if rm-rctd.cost ne 0 then rm-rctd.cost
                                               else item.last-cost
         item.q-onh     = item.q-onh + ld-cvt-qty
         item.q-avail   = item.q-onh + item.q-ono - item.q-comm.
      end. /* A */

      else
      if rm-rctd.rita-code eq "T" then do:  /** TRANSFERS **/
        assign
         rm-bin.qty   = rm-bin.qty - rm-rctd.qty
         rm-rctd.cost = rm-bin.cost.

        /* This code is to handel the Transfer to quantity to increase the BIN
           using a buffer record so current rm-bin record is not updated. */

        find first xrm-bin
             where xrm-bin.company eq rm-rctd.company
               and xrm-bin.loc     eq rm-rctd.loc2
               and xrm-bin.i-no    eq rm-rctd.i-no
               and xrm-bin.loc-bin eq rm-rctd.loc-bin2
               and xrm-bin.tag     eq rm-rctd.tag2
             no-error.
        if not avail xrm-bin then do:
          create xrm-bin.
          assign
           xrm-bin.company = rm-rctd.company
           xrm-bin.loc     = rm-rctd.loc2
           xrm-bin.loc-bin = rm-rctd.loc-bin2
           xrm-bin.tag     = rm-rctd.tag2
           xrm-bin.i-no    = rm-rctd.i-no.
        end.

        {rm/rm-post.i "xrm-bin.qty" "xrm-bin.cost" "rm-rctd.qty" "rm-rctd.cost"}

        xrm-bin.qty = xrm-bin.qty + rm-rctd.qty.
      end. /* T */

/*       /** Delete Bins With Zero Quantities. **/ */
/*       IF rm-bin.qty EQ 0 THEN DELETE rm-bin.    */

      RELEASE loadtag.
      IF TRIM(rm-rctd.tag) NE "" THEN
      FIND FIRST loadtag EXCLUSIVE-LOCK 
          WHERE loadtag.company     EQ rm-rctd.company
            AND loadtag.item-type   EQ YES
            AND loadtag.tag-no      EQ rm-rctd.tag
            AND loadtag.i-no        EQ rm-rctd.i-no
            AND loadtag.is-case-tag EQ NO
          NO-ERROR.

      IF AVAIL loadtag THEN DO:
        IF rm-rctd.rita-code EQ "T" THEN 
          ASSIGN
           loadtag.loc     = rm-rctd.loc2
           loadtag.loc-bin = rm-rctd.loc-bin2.
        ELSE
          ASSIGN
           loadtag.loc     = rm-rctd.loc
           loadtag.loc-bin = rm-rctd.loc-bin.

        li = INDEX("RI",rm-rctd.rita-code).

        IF li EQ 1 AND (NOT AVAIL rm-bin OR rm-bin.qty LT 0) THEN li = 3.

        IF li GT 0 THEN loadtag.sts = ENTRY(li,"Received,Issued,Deleted").
      END.

      if last-of(tt-rctd.i-no) then             /* Calculate average cost */
      for each rm-bin
          where rm-bin.company eq rm-rctd.company
            and rm-bin.i-no    eq rm-rctd.i-no
          no-lock use-index i-no
          break by rm-bin.i-no:

        if first(rm-bin.i-no) then
          assign
           v-i-qty = 0
           cost    = 0.

        v-r-qty = rm-bin.qty.

        if v-r-qty lt 0 then v-r-qty = v-r-qty * -1.

        assign
         v-i-qty = v-i-qty + v-r-qty
         cost    = cost    + (v-r-qty * rm-bin.cost).

        IF cost EQ ? THEN cost = 0.

        if last(rm-bin.i-no) and v-i-qty ne 0 AND cost NE 0 THEN item.avg-cost = cost / v-i-qty.

      end. /* each rm-bin */      
     
      /* gdm - 10280903 - Assign prep code received date */
      RUN assign-prep-info. 
      RUN final-steps.

      FIND CURRENT rm-rctd NO-LOCK NO-ERROR.
      FIND CURRENT item NO-LOCK NO-ERROR.
      FIND CURRENT loadtag NO-LOCK NO-ERROR.
      FIND CURRENT rm-rcpth NO-LOCK NO-ERROR.
      FIND CURRENT rm-rdtlh NO-LOCK NO-ERROR.
      FIND CURRENT mat-act NO-LOCK NO-ERROR.
      FIND CURRENT job NO-LOCK NO-ERROR.
      FIND CURRENT job-mat NO-LOCK NO-ERROR.
    end. /* for each rm-rctd */

    
    DO TRANSACTION:
      /* gdm - 11050906 */
      REPEAT:

        FIND FIRST gl-ctrl EXCLUSIVE-LOCK
          WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
        IF AVAIL gl-ctrl THEN DO:
          ASSIGN v-trnum       = gl-ctrl.trnum + 1
                 gl-ctrl.trnum = v-trnum.

          FIND CURRENT gl-ctrl NO-LOCK NO-ERROR.
         
          RUN gl-from-work (1, v-trnum).
          RUN gl-from-work (2, v-trnum).
          LEAVE.
        END. /* IF AVAIL gl-ctrl */
      END. /* REPEAT */
      /* gdm - 11050906 */
    END. /* IF rmpostgl */

    IF can-find(FIRST tt-email) THEN 
      RUN send-rmemail (v-rmemail-file).
   
    SESSION:SET-WAIT-STATE ("").

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
  DEF INPUT PARAMETER ip-recid AS RECID NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    REPOSITION {&BROWSE-NAME} TO RECID ip-recid NO-ERROR.
    RUN dispatch ("row-changed").
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE runWhichPost B-table-Win 
PROCEDURE runWhichPost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER opRunWhichPost AS CHARACTER NO-UNDO.

  opRunWhichPost = 'rm/r-rmtpst.p'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE scan-next B-table-Win 
PROCEDURE scan-next :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
  RUN auto-add IN WIDGET-HANDLE(char-hdl).

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
  {src/adm/template/snd-list.i "loadtag"}

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
  def output parameter op-tag# as log no-undo.
  
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-from-po-line B-table-Win 
PROCEDURE update-from-po-line :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-ord-qty LIKE po-ordl.ord-qty NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}    = STRING(po-ordl.s-num)
      rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}  = po-ordl.pr-qty-uom
      rm-rctd.cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     = STRING(po-ordl.cost)  /* po-ordl.cost*/
      rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = po-ordl.pr-uom
      lv-po-wid:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}        = STRING(po-ordl.s-wid)
      lv-po-len:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}        = STRING(po-ordl.s-len)
      ll-warned                                              = NO.

      /*RUN get-remain-qty (ROWID(po-ordl), OUTPUT lv-ord-qty).
      rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(lv-ord-qty).*/
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-delete-tag B-table-Win 
PROCEDURE valid-delete-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 FIND FIRST loadtag WHERE loadtag.company = g_company
                      AND loadtag.item-type = YES
                      AND loadtag.tag-no = rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
                      NO-LOCK NO-ERROR.
 IF AVAIL loadtag THEN
    FIND FIRST rm-bin WHERE rm-bin.company = g_company
                        AND rm-bin.loc = loadtag.loc
                        AND rm-bin.i-no = loadtag.i-no
                        AND rm-bin.loc-bin = loadtag.loc-bin
                        AND rm-bin.tag = loadtag.tag-no
                        AND rm-bin.qty > 0
                        NO-LOCK NO-ERROR.
 IF NOT AVAIL rm-bin THEN DO:
    MESSAGE "Invalid Inventory Qty for the tag." 
        VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-i-no B-table-Win 
PROCEDURE valid-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-msg AS CHAR NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.

        
  DO WITH FRAME {&FRAME-NAME}:
    rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).

    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE 0 THEN DO:
      RELEASE po-ord.

      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.s-num     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.i-no      EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.
      IF NOT AVAIL po-ordl THEN v-msg = "Invalid PO Line Item, try help".
       
      ELSE
      FIND FIRST po-ord WHERE
           po-ord.company EQ po-ordl.company AND
           po-ord.po-no   EQ po-ordl.po-no AND
           po-ord.type EQ "S"
           NO-LOCK NO-ERROR.

      IF AVAIL po-ord THEN DO:
        FOR EACH rm-rcpth
            WHERE rm-rcpth.company   EQ po-ord.company
              AND rm-rcpth.vend-no   EQ po-ord.vend-no
              AND rm-rcpth.po-no     EQ TRIM(STRING(po-ord.po-no,">>>>>>>>>>"))
              AND rm-rcpth.rita-code EQ "I"
            USE-INDEX vend NO-LOCK,
            EACH rm-rdtlh
            WHERE rm-rdtlh.r-no             EQ rm-rcpth.r-no
              AND rm-rdtlh.rita-code        EQ rm-rcpth.rita-code
              /*AND SUBSTR(rm-rdtlh.BOL,1,30) EQ po-ordl.i-no
              AND SUBSTR(rm-rdtlh.BOL,31,3) EQ STRING(po-ordl.line,"999")*/
            NO-LOCK:
          LEAVE.
        END.
        IF NOT AVAIL rm-rdtlh THEN v-msg = "No RM issued to this Sheet PO".
      END.

      IF v-msg EQ "" AND po-ordl.stat EQ "C" THEN v-msg = "Closed".
    END.

    ELSE
    IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE "" THEN DO:
      FIND FIRST job
          WHERE job.company EQ rm-rctd.company
            AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND job.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
          NO-LOCK NO-ERROR.
      IF AVAIL job THEN
      FIND FIRST job-mat
          WHERE job-mat.company EQ rm-rctd.company
            AND job-mat.job     EQ job.job
            AND job-mat.frm     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND job-mat.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          NO-LOCK NO-ERROR.
      IF NOT AVAIL job-mat THEN v-msg = "This RM does not exist on Job, try help".
    END.

    ELSE DO:
      FIND FIRST ITEM
          WHERE ITEM.company EQ cocode
            AND ITEM.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND ITEM.i-code  EQ "R"
          NO-LOCK NO-ERROR.
      IF NOT AVAIL item THEN v-msg = "This item does not exist in RM file, try help".
    END.

    IF v-msg NE "" THEN DO:
      ll = NO.
      IF v-msg EQ "Closed" THEN DO:
        IF ll-warned THEN ll = YES.
        ELSE
          MESSAGE "PO Line is closed and may be re-opened during posting, continue?..."
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE ll.
      END.
      ELSE MESSAGE TRIM(v-msg) + "..." VIEW-AS ALERT-BOX ERROR.

      IF ll THEN ll-warned = YES.
      ELSE DO:
        APPLY "entry" TO rm-rctd.tag IN BROWSE {&BROWSE-NAME}.
        RETURN ERROR.
      END.
    END.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-no B-table-Win 
PROCEDURE valid-job-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
    rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).

    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE 0 THEN DO:
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.
      IF NOT AVAIL po-ordl THEN ERROR-STATUS:ERROR = YES.
    END.

    ELSE
    IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE "" THEN DO:
      FIND FIRST job
          WHERE job.company EQ rm-rctd.company
            AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          NO-LOCK NO-ERROR.
      IF NOT AVAIL job THEN ERROR-STATUS:ERROR = YES.
    END.

    ELSE
      ASSIGN
       rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = ""
       rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}   = "".

    IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO rm-rctd.job-no IN BROWSE {&BROWSE-NAME}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-no2 B-table-Win 
PROCEDURE valid-job-no2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
    rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).

    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE 0 THEN DO:
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.
      IF NOT AVAIL po-ordl THEN ERROR-STATUS:ERROR = YES.
    END.

    ELSE
    IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE "" THEN DO:
      FIND FIRST job
          WHERE job.company EQ rm-rctd.company
            AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND job.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
          NO-LOCK NO-ERROR.
      IF NOT AVAIL job THEN ERROR-STATUS:ERROR = YES. 
    END.

    IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO rm-rctd.job-no2 IN BROWSE {&BROWSE-NAME}.
      RETURN ERROR.
    END.
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
  
  DO WITH FRAME {&FRAME-NAME}:
    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE 0 THEN DO:
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.
      IF NOT AVAIL po-ordl THEN DO:
        MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO rm-rctd.po-no IN BROWSE {&BROWSE-NAME}.
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
    ERROR-STATUS:ERROR = NO.

    IF DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) EQ 0 THEN
      ERROR-STATUS:ERROR = YES.

    IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "Receipt qty may not be zero..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO rm-rctd.qty IN BROWSE {&BROWSE-NAME}.
      RETURN ERROR.
    END.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-s-num B-table-Win 
PROCEDURE valid-s-num :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
    rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).

    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE 0 THEN DO:
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.s-num     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.
      IF NOT AVAIL po-ordl THEN ERROR-STATUS:ERROR = YES.
    END.

    ELSE
    IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE "" THEN DO:
      FIND FIRST job
          WHERE job.company EQ rm-rctd.company
            AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND job.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
          NO-LOCK NO-ERROR.
      IF AVAIL job THEN
      FIND FIRST job-mat
          WHERE job-mat.company EQ rm-rctd.company
            AND job-mat.job     EQ job.job
            AND job-mat.frm     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
          NO-LOCK NO-ERROR.
      IF NOT AVAIL job-mat THEN ERROR-STATUS:ERROR = YES.
    END.

    IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO rm-rctd.s-num IN BROWSE {&BROWSE-NAME}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tag B-table-Win 
PROCEDURE valid-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF rmrecpt-int EQ 1 THEN DO:
    FIND FIRST loadtag WHERE loadtag.company = g_company
                         AND loadtag.item-type = YES
                         AND loadtag.tag-no = rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
    IF NOT AVAIL loadtag THEN DO:
       MESSAGE "Invalid Tag#. Try help or Scan valid tag#..." VIEW-AS ALERT-BOX ERROR.
       rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = ''.
       APPLY "entry" TO rm-rctd.tag IN BROWSE {&browse-name}.
       RETURN ERROR.
    END.
  END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-uom B-table-Win 
PROCEDURE valid-uom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-int AS INT NO-UNDO.

  DEF VAR lv-uom-list AS cha INIT ["EA,TON,MSF,MSH,LB,LF"] NO-UNDO.
  DEF VAR lv-uom AS CHAR NO-UNDO.
  DEF VAR lv-uom-help AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    lv-uom = IF ip-int EQ 1 THEN rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                            ELSE rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}.

    FIND FIRST ITEM
        WHERE item.company EQ cocode
          AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
        NO-LOCK NO-ERROR.

    IF AVAIL item THEN RUN sys/ref/uom-rm.p (INPUT item.mat-type, OUTPUT lv-uom-list).

    IF AVAIL item AND INDEX("MOXY789@",ITEM.mat-type) GT 0 AND ip-int EQ 2 THEN
      lv-uom-list = lv-uom-list + ",L".

    lv-uom-help = "Must enter one of the following as the UOM: " + lv-uom-list.

    IF INDEX(lv-uom-list,lv-uom) LE 0 THEN DO:
      MESSAGE TRIM(lv-uom-help) + "..." VIEW-AS ALERT-BOX ERROR.
      IF ip-int EQ 1 THEN
        APPLY "entry" TO rm-rctd.pur-uom IN BROWSE {&BROWSE-NAME}.
      ELSE
        APPLY "entry" TO rm-rctd.cost-uom IN BROWSE {&BROWSE-NAME}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-record B-table-Win 
PROCEDURE validate-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 FIND FIRST loc WHERE loc.company = g_company
                        AND loc.loc = rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                        NO-LOCK NO-ERROR.
       IF NOT AVAIL loc THEN DO:
          MESSAGE "Invalid Warehouse. Try Help. " VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO rm-rctd.loc.
          RETURN ERROR.
  END.
  
  FIND FIRST rm-bin WHERE rm-bin.company = g_company
                      AND rm-bin.i-no = ""
                      AND rm-bin.loc = rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND rm-bin.loc-bin = rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
  IF NOT AVAIL rm-bin THEN DO:
          MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO rm-rctd.loc-bin.
          RETURN ERROR.
  END.

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
  return ext-cost.
  
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION checkWhsBin B-table-Win 
FUNCTION checkWhsBin RETURNS LOGICAL
  (ipCompany AS CHARACTER,ipLoc AS CHARACTER,ipLocBin AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN CAN-FIND(FIRST loc
                  WHERE loc.company EQ ipCompany
                    AND loc.loc EQ ipLoc) AND
         CAN-FIND(FIRST rm-bin
                  WHERE rm-bin.company EQ ipCompany
                    AND rm-bin.loc EQ ipLoc
                    AND rm-bin.i-no EQ ''
                    AND rm-bin.loc-bin EQ ipLocBin).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-dimension B-table-Win 
FUNCTION display-dimension RETURNS DECIMAL
  ( INPUT ip-dim AS char ) :
/*------------------------------------------------------------------------------
  Purpose:  from rcptdims.v  
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR ld-dim AS DEC NO-UNDO.
  DEF VAR v-wid-num AS DEC NO-UNDO.
  DEF VAR v-len-num AS DEC NO-UNDO.
  DEF BUFFER b-jm FOR job-mat.

  IF AVAIL rm-rctd THEN DO:
    FIND FIRST ITEM
        WHERE ITEM.company EQ cocode
          AND ITEM.i-no    EQ rm-rctd.i-no
          AND ITEM.i-code  EQ "R"
        NO-LOCK NO-ERROR.
    IF AVAIL ITEM THEN
      ASSIGN
       v-wid-num = IF ITEM.r-wid NE 0 THEN ITEM.r-wid ELSE ITEM.s-wid
       v-len-num = ITEM.s-len.

    IF rm-rctd.po-no <> "" THEN DO:
      find first po-ordl where po-ordl.company   eq cocode
                           and po-ordl.po-no     eq int(rm-rctd.po-no)
                           and po-ordl.i-no      eq rm-rctd.i-no
                           and po-ordl.job-no    eq rm-rctd.job-no
                           and po-ordl.job-no2   eq rm-rctd.job-no2
                           and po-ordl.item-type eq yes
                           and po-ordl.s-num     eq rm-rctd.s-num
          no-lock no-error.
      if avail po-ordl then
        ASSIGN  v-wid-num = po-ordl.s-wid
                v-len-num = po-ordl.s-len.
      else do:
        if rm-rctd.job-no ne "" then
           find first b-jm where b-jm.company eq cocode
                             and b-jm.rm-i-no eq rm-rctd.i-no
                             and b-jm.job-no  eq rm-rctd.job-no
                             and b-jm.job-no2 eq rm-rctd.job-no2
                             and b-jm.frm     eq rm-rctd.s-num
                             no-lock no-error.
        if avail b-jm THEN ASSIGN v-wid-num = b-jm.wid
                                  v-len-num = b-jm.len.
        else do:
           find first ITEM where item.company eq cocode
                             and item.i-no    eq rm-rctd.i-no
                             no-lock no-error.
           if avail item then
              if item.r-wid eq 0 then
                 ASSIGN v-wid-num = item.s-wid
                        v-len-num = item.s-len.
              ELSE ASSIGN v-wid-num = item.r-wid
                          v-len-num = 12.
        end.
      end.
    END.
      
    IF ip-dim = "W" THEN ld-dim = v-wid-num.
    ELSE IF ip-dim = "L" THEN ld-dim = v-len-num.
  END.
  
  RETURN ld-dim.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

