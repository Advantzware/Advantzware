&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  browsers/cust.w

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

/*&SCOPED-DEFINE setBrowseFocus
&SCOPED-DEFINE yellowColumnsName cust
&SCOPED-DEFINE cellColumnDat browsers-cust
&SCOPED-DEFINE useMatches
&SCOPED-DEFINE defaultWhere cust.company = gcompany */
&SCOPED-DEFINE winReSize
&SCOPED-DEFINE browseOnly
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{custom/gcompany.i}
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}

DEF VAR v-called-setCellColumns AS LOG NO-UNDO.
DEF VAR v-col-move AS LOG INIT YES NO-UNDO.
 
DEFINE VARIABLE columnCount AS INTEGER NO-UNDO.
DEFINE VARIABLE idx AS INTEGER NO-UNDO.
DEFINE VARIABLE useColors AS CHAR NO-UNDO.
DEFINE VARIABLE lvFirstRowID AS ROWID NO-UNDO.
DEFINE VARIABLE lvLastRowID AS ROWID NO-UNDO.

DEF VAR ll-first AS LOG INIT YES NO-UNDO.
DEF VAR lv-sort-by AS CHAR INIT "i-no"  NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "custor No"  NO-UNDO.
DEF VAR ll-sort-asc AS LOG INIT YES NO-UNDO.
DEF VAR char-hdl AS CHAR NO-UNDO.
DEF VAR phandle AS HANDLE NO-UNDO.
DEF VAR lv-frst-rowid AS ROWID NO-UNDO.
DEF VAR lv-last-rowid AS ROWID NO-UNDO.
DEF VAR ll-browse-first AS LOG INIT YES NO-UNDO.
DEF VAR lv-show-prev AS LOG NO-UNDO.
DEF VAR lv-show-next AS LOG NO-UNDO.
DEF VAR lv-last-show-ord-no AS int NO-UNDO.
DEF VAR lv-first-show-ord-no AS int NO-UNDO.
DEF VAR ll-show-all AS LOG NO-UNDO.
DEF VAR lv-cust-no AS cha NO-UNDO.
DEF VAR lv-last-show-cust-no AS cha NO-UNDO.
DEF VAR lv-first-show-cust-no AS cha NO-UNDO.
DEF VAR v-rec-key-list AS CHAR NO-UNDO.
DEF VAR lActive AS LOG NO-UNDO.

ASSIGN cocode = g_company
       locode = g_loc.

DO TRANSACTION:
     {sys/ref/CustList.i NEW}
    {sys/inc/custlistform.i ""AF1"" }
END.

&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

&IF defined (UIB_is_Running) = 1
  &THEN {methods\defines\phone.i}
  &ELSE {methods\defines\phone.i &NEW="NEW"}
&ENDIF

&SCOPED-DEFINE key-phrase cust.company EQ cocode

&SCOPED-DEFINE for-each1                          ~
    FOR EACH cust                               ~
        WHERE {&key-phrase}                       ~
          AND cust.cust-no      BEGINS fi_cust-no     ~
          AND cust.NAME   BEGINS fi_i-name   ~
          AND cust.city   BEGINS fi_city   ~
          AND cust.state  BEGINS fi_stat   ~
          AND cust.zip    BEGINS fi_zip   ~
          AND cust.TYPE   BEGINS fi_type    ~
          AND cust.terr   BEGINS fi_terr  ~
          AND cust.sman   BEGINS fi_sman  ~
          AND ( lookup(cust.cust-no,custcount) <> 0 OR custcount = "") ~
          AND ((CAN-DO("A,X,S,E,",cust.active) AND tb_act) OR (cust.ACTIVE = "I" AND tb_in-act))

&SCOPED-DEFINE for-each2                          ~
    FOR EACH cust                               ~
        WHERE {&key-phrase}                       ~
          AND (IF fi_cust-no BEGINS '*' THEN cust.cust-no MATCHES  fi_cust-no     ~
               ELSE cust.cust-no BEGINS  fi_cust-no)   ~
          AND (IF fi_i-name BEGINS '*' THEN cust.NAME   MATCHES fi_i-name  ~
               ELSE cust.NAME   BEGINS fi_i-name) ~
          AND (IF fi_city BEGINS '*' THEN cust.city   MATCHES fi_city  ~
               ELSE cust.city   BEGINS fi_city)  ~
          AND (IF fi_stat BEGINS '*' THEN cust.state MATCHES fi_stat  ~
               ELSE cust.state    BEGINS fi_stat)   ~
          AND cust.zip    BEGINS fi_zip   ~
          AND (IF fi_type BEGINS '*' THEN cust.TYPE     MATCHES fi_type    ~
               ELSE cust.TYPE     BEGINS fi_type)  ~
          AND (IF fi_terr BEGINS '*' THEN cust.terr    MATCHES fi_terr   ~
               ELSE cust.terr    BEGINS fi_terr) ~
          AND (IF fi_sman BEGINS '*' THEN cust.sman    MATCHES fi_sman   ~
              ELSE cust.sman    BEGINS fi_sman) ~
          AND ( lookup(cust.cust-no,custcount) <> 0 OR custcount = "") ~
          AND ((CAN-DO("A,X,S,E,",cust.active) AND tb_act) OR (cust.ACTIVE = "I" AND tb_in-act))

&SCOPED-DEFINE for-eachblank                      ~
    FOR EACH cust                               ~
        WHERE {&key-phrase} ~
        AND ( lookup(cust.cust-no,custcount) <> 0 OR custcount = "") ~
        AND ((CAN-DO("A,X,S,E,",cust.active) AND tb_act) OR (cust.ACTIVE = "I" AND tb_in-act))

&SCOPED-DEFINE sortby-log                                                                                                                                  ~
    IF lv-sort-by EQ "cust-no"  THEN cust.cust-no ELSE ~
    IF lv-sort-by EQ "name"     THEN cust.name    ELSE ~
    IF lv-sort-by EQ "type"     THEN cust.type ELSE ~
    IF lv-sort-by EQ "active"   THEN cust.active   ELSE ~
    IF lv-sort-by EQ "area-code"     THEN cust.area-code      ELSE ~
    IF lv-sort-by EQ "phone"    THEN cust.phone      ELSE ~
    IF lv-sort-by EQ "fax"      THEN cust.fax      ELSE ~
    IF lv-sort-by EQ "city"     THEN cust.city      ELSE ~
    IF lv-sort-by EQ "state"    THEN cust.state      ELSE ~
    IF lv-sort-by EQ "zip"      THEN cust.zip      ELSE ~
    IF lv-sort-by EQ "sman"     THEN cust.sman      ELSE ~
    IF lv-sort-by EQ "terr"     THEN cust.terr  ELSE ~
    IF lv-sort-by EQ "spare-char-2"     THEN cust.spare-char-2  ELSE ""

&SCOPED-DEFINE sortby BY cust.cust-no

&SCOPED-DEFINE sortby-phrase-asc  ~
    BY ({&sortby-log})            ~
    {&sortby}

&SCOPED-DEFINE sortby-phrase-desc  ~
    BY ({&sortby-log}) DESC        ~
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
&Scoped-define INTERNAL-TABLES cust

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table cust.company cust.cust-no ~
cust.name cust.city cust.state cust.zip cust.area-code cust.phone cust.type cust.sman cust.terr cust.spare-char-2 ~
cust.rec_key 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH cust WHERE ~{&KEY-PHRASE} ~
      AND cust.company = gcompany NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH cust WHERE ~{&KEY-PHRASE} ~
      AND cust.company = gcompany NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table cust
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table cust


/* Definitions for FRAME F-Main                                         */
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tb_act fi_cust-no fi_i-name fi_city fi_stat ~
fi_zip fi_type fi_terr fi_sman tb_in-act btn_go btn_prev btn_next btn_show ~
Browser-Table 
&Scoped-Define DISPLAYED-OBJECTS tb_act fi_cust-no fi_i-name fi_city ~
fi_stat fi_zip fi_type fi_terr fi_sman tb_in-act fi_sort-by FI_moveCol 

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

/* ***********************  Control Definitions  ********************** */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_go 
     LABEL "&Go" 
     SIZE 12 BY 1
     FONT 6.

DEFINE BUTTON btn_next 
     LABEL "Show &Next" 
     SIZE 15 BY 1
     FONT 6.

DEFINE BUTTON btn_prev 
     LABEL "Show &Previous" 
     SIZE 20 BY 1
     FONT 6.

DEFINE BUTTON btn_show 
     LABEL "&Show All" 
     SIZE 12 BY 1
     FONT 6.

DEFINE VARIABLE fi_terr AS CHARACTER FORMAT "X(3)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_sman AS CHARACTER FORMAT "X(3)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_city AS CHARACTER FORMAT "X(13)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_i-name AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI_moveCol AS CHARACTER FORMAT "X(4)":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE fi_sort-by AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34.6 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE fi_stat AS CHARACTER FORMAT "X(5)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_type AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_cust-no AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_zip AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_act AS LOGICAL INITIAL YES 
     LABEL "Active" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81
     FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE VARIABLE tb_in-act AS LOGICAL INITIAL no 
     LABEL "Inactive" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81
     FGCOLOR 9 FONT 6 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      cust
    FIELDS(cust.company
      cust.cust-no
      cust.name
      cust.city
      cust.state
      cust.zip
      cust.area-code
      cust.phone
      cust.type
      cust.sman
      cust.terr
      cust.spare-char-2
      cust.rec_key) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      cust.company FORMAT "x(3)":U
      cust.cust-no FORMAT "x(8)":U WIDTH 11.2 LABEL-BGCOLOR 14
      cust.name COLUMN-LABEL "Name" FORMAT "x(30)":U LABEL-BGCOLOR 14
      cust.city FORMAT "x(15)":U LABEL-BGCOLOR 14
      cust.state FORMAT "x(2)":U WIDTH 6.2 LABEL-BGCOLOR 14
      cust.zip COLUMN-LABEL "Zip" FORMAT "x(10)":U LABEL-BGCOLOR 14
      cust.area-code FORMAT "(999)":U LABEL-BGCOLOR 14
      cust.phone COLUMN-LABEL "Phone #" FORMAT "999-9999":U LABEL-BGCOLOR 14
      cust.type COLUMN-LABEL "Type" FORMAT "x(8)":U LABEL-BGCOLOR 14
      cust.sman COLUMN-LABEL "SalesRep" FORMAT "x(3)":U LABEL-BGCOLOR 14
      cust.terr COLUMN-LABEL "Territory" FORMAT "x(3)":U LABEL-BGCOLOR 14
      cust.spare-char-2 COLUMN-LABEL "Group" FORMAT "x(8)":U LABEL-BGCOLOR 14
      cust.rec_key FORMAT "X(20)":U
      ENABLE
      cust.company          
      cust.cust-no               
      cust.name                  
      cust.city                  
      cust.state                 
      cust.zip      
      cust.area-code
      cust.phone
      cust.type                  
      cust.sman                  
      cust.terr                  
      cust.spare-char-2
      cust.rec_key

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 150 BY 17.90
         FONT 2.


/* ************************  Frame Definitions  *********************** */
DEFINE FRAME F-Main
     tb_act AT ROW 1.33 COL 133.8 WIDGET-ID 48
     fi_cust-no AT ROW 2.19 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     fi_i-name AT ROW 2.19 COL 14.6 COLON-ALIGNED NO-LABEL WIDGET-ID 40
     fi_city AT ROW 2.19 COL 45.2 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     fi_stat AT ROW 2.19 COL 65.8 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     fi_zip AT ROW 2.19 COL 76.2 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     fi_type AT ROW 2.19 COL 93.2 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     fi_sman AT ROW 2.19 COL 109.8 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     fi_terr AT ROW 2.19 COL 120.2 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     tb_in-act AT ROW 2.48 COL 133.8 WIDGET-ID 50
     btn_go AT ROW 3.62 COL 1.8 WIDGET-ID 4
     btn_prev AT ROW 3.62 COL 13.8 WIDGET-ID 8
     btn_next AT ROW 3.62 COL 34 WIDGET-ID 6
     btn_show AT ROW 3.62 COL 49.2 WIDGET-ID 10
     fi_sort-by AT ROW 3.62 COL 78 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     FI_moveCol AT ROW 3.62 COL 141 COLON-ALIGNED NO-LABEL WIDGET-ID 46
     Browser-Table AT ROW 5.29 COL 2.4 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     "Name" VIEW-AS TEXT
          SIZE 18 BY .71 AT ROW 1.24 COL 19.8 WIDGET-ID 42
          FGCOLOR 9 FONT 6
     "Customer#" VIEW-AS TEXT
          SIZE 13 BY .71 AT ROW 1.24 COL 3 WIDGET-ID 24
          FGCOLOR 9 FONT 6
     "Click on Yellow Field to Sort" VIEW-AS TEXT
          SIZE 28 BY .95 AT ROW 3.62 COL 114 WIDGET-ID 14
     "Type" VIEW-AS TEXT
          SIZE 12 BY .71 AT ROW 1.24 COL 97.8 WIDGET-ID 34
          FGCOLOR 9 FONT 6
     "Terr" VIEW-AS TEXT
          SIZE 10 BY .71 AT ROW 1.24 COL 123 WIDGET-ID 38
          FGCOLOR 9 FONT 6
     "Rep" VIEW-AS TEXT
          SIZE 10 BY .71 AT ROW 1.24 COL 112.20 WIDGET-ID 38
          FGCOLOR 9 FONT 6
     "City" VIEW-AS TEXT
          SIZE 13 BY .71 AT ROW 1.24 COL 50.6 WIDGET-ID 22
          FGCOLOR 9 FONT 6
     "Sorted By:" VIEW-AS TEXT
          SIZE 12 BY 1 AT ROW 3.62 COL 68 WIDGET-ID 30
          FONT 6
     "Zip" VIEW-AS TEXT
          SIZE 12 BY .71 AT ROW 1.24 COL 81 WIDGET-ID 28
          FGCOLOR 9 FONT 6
     /*"Browser Col. Mode:" VIEW-AS TEXT
          SIZE 22.6 BY .62 AT ROW 18.38 COL 99.6 WIDGET-ID 6
          FONT 6*/
     "State" VIEW-AS TEXT
          SIZE 7.2 BY .71 AT ROW 1.24 COL 69.8 WIDGET-ID 26
          FGCOLOR 9 FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 
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
         WIDTH              = 150.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

/*{src/adm/method/browser.i}
{src/adm/method/query.i}
{methods/template/browser.i}
{custom/yellowColumns.i}*/

{src/adm/method/navbrows.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB Browser-Table TEXT-2 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "2"
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

ASSIGN 
       cust.rec_key:VISIBLE IN BROWSE Browser-Table = FALSE.

/* SETTINGS FOR FILL-IN FI_moveCol IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-ENABLE                                                            */
/*ASSIGN 
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.
  */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.cust"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED"
     _Where[1]         = "cust.company = gcompany"
     _FldNameList[1]   = ASI.cust.company
     _FldNameList[2]   > ASI.cust.cust-no
"cust.cust-no" ? ? "character" ? ? ? 14 ? ? no ? no no "11.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.cust.name
"cust.name" "Name" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.cust.city
"cust.city" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.cust.state
"cust.state" ? ? "character" ? ? ? 14 ? ? no ? no no "6.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.cust.zip
"cust.zip" "Zip" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
    _FldNameList[7]   > ASI.cust.area-code
"cust.area-code" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.cust.phone
"cust.phone" "Phone #" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.cust.type
"cust.type" "Type" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.cust.sman
"cust.sman" "SalesRep" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.cust.terr
"cust.terr" "Territory" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
 _FldNameList[11]   > ASI.cust.spare-char-2
"cust.spare-char-2" "Group" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.cust.rec_key
"cust.rec_key" ? ? "character" ? ? ? ? ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
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
&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main Browser-Table
ON HELP OF FRAME F-Main
DO:
   def var lv-handle as handle no-undo.
   def var char-val as cha no-undo.
   DEF VAR rec-val as recid no-undo. 
   DEF VAR city-val as cha no-undo. 
   DEF VAR state-val as cha no-undo. 

   CASE FOCUS:NAME:
     WHEN "fi_sman" THEN DO:
         
        run windows/l-sman.w  (g_company, output char-val). 
            if char-val <> "" then 
              focus:screen-value in frame {&frame-name} = entry(1,char-val).                
     END. 
      WHEN "fi_city" THEN DO:
       RUN windows/l-city.w (FOCUS:SCREEN-VALUE,OUTPUT char-val,OUTPUT rec-val).
            if char-val <> "" then 
              focus:screen-value in frame {&frame-name} = entry(1,char-val).                
     END.
     WHEN "fi_stat" THEN DO:
        run windows/l-state.w ("", output char-val).
            if char-val <> "" then 
              focus:screen-value in frame {&frame-name} = entry(1,char-val).                
     END.
     WHEN "fi_zip" THEN DO:
        RUN windows/l-zipcod.w (FOCUS:SCREEN-VALUE,OUTPUT char-val,OUTPUT city-val,OUTPUT state-val,OUTPUT rec-val).
            if char-val <> "" then 
              focus:screen-value in frame {&frame-name} = entry(1,char-val).                
     END.
     WHEN "fi_type" THEN DO:
         run windows/l-csttyp.w  (g_company,FOCUS:SCREEN-VALUE, output char-val). 
            if char-val <> "" then 
              focus:screen-value in frame {&frame-name} = entry(1,char-val).             
     END.
     WHEN "fi_terr" THEN DO:
        run windows/l-terr.w  (g_company,FOCUS:SCREEN-VALUE, output char-val). 
            if char-val <> "" then 
              focus:screen-value in frame {&frame-name} = entry(1,char-val).                
     END.
     otherwise do:                                                  
           lv-handle = focus:handle.                                
           run applhelp.p.                                           
                                                                    
           if g_lookup-var <> "" then do:                           
              lv-handle:screen-value = g_lookup-var.               
        
           end.   /* g_lookup-var <> "" */
           apply "entry" to lv-handle.
           return no-apply.

     end.  /* otherwise */
      
    END CASE.
    /*APPLY "CHOOSE" TO btn_go.*/
    RETURN NO-APPLY.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define BROWSE-NAME Browser-Table
&Scoped-define SELF-NAME fi_i-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-name B-table-Win
ON HELP OF fi_i-name IN FRAME F-Main
DO:
   DEF VAR char-val AS cha NO-UNDO.

   run windows/l-cust.w (g_company,"", output char-val).
   if char-val <> "" then DO:
       assign fi_i-name:SCREEN-VALUE = entry(2,char-val)
           /*ocat-desc:screen-value = entry(2,char-val)*/ .
       APPLY 'tab' TO fi_i-name.
   END.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fi_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cust-no B-table-Win
ON HELP OF fi_cust-no IN FRAME F-Main
DO:
   DEF VAR char-val AS cha NO-UNDO.

   run windows/l-cust.w (g_company,fi_cust-no:SCREEN-VALUE, output char-val).
   if char-val <> "" then DO:
       assign fi_cust-no:SCREEN-VALUE = entry(1,char-val)
           /*ocat-desc:screen-value = entry(2,char-val)*/ .
       APPLY 'tab' TO fi_cust-no.
   END.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Browser-Table
&Scoped-define SELF-NAME Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON DEFAULT-ACTION OF Browser-Table IN FRAME F-Main
DO:
  DEF VAR phandle AS HANDLE NO-UNDO.
  DEF VAR char-hdl AS cha NO-UNDO.
  {methods/run_link.i "container-source" "select-page" "(2)"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Go */
DO:
  DEF BUFFER bf-cust FOR cust .
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     fi_cust-no
     fi_i-name                                               
     fi_city                                                 
     fi_stat                                                 
     fi_zip                                                  
     fi_type                                                 
     fi_sman                                                 
     fi_terr                                                 
     tb_act
     tb_in-act
     ll-first = NO.
    
    RUN dispatch ("open-query").
    
    GET FIRST Browser-Table .
    IF NOT AVAIL cust THEN DO: 
        FIND FIRST bf-cust WHERE bf-cust.company = cocode
                 AND ((CAN-DO("A,X,S,E,",bf-cust.active) AND tb_act) OR (bf-cust.ACTIVE = "I" AND tb_in-act))
                 AND (bf-cust.cust-no EQ fi_cust-no OR fi_cust-no = "")
                 AND (bf-cust.NAME BEGINS fi_i-name OR fi_i-name = "")
                 AND (bf-cust.city BEGINS fi_city OR fi_city = "")
                 AND (bf-cust.state BEGINS fi_stat OR fi_stat = "")
                 AND (bf-cust.zip = string(fi_zip) OR fi_zip = "")
                 AND (bf-cust.TYPE BEGINS fi_type OR fi_type = "")
                 AND (bf-cust.terr BEGINS fi_terr OR fi_terr = "")
                 AND (bf-cust.sman BEGINS fi_sman OR fi_sman = "") NO-LOCK NO-ERROR.
             
         IF AVAIL bf-cust AND ou-log AND LOOKUP(bf-cust.cust-no,custcount) = 0 THEN
             MESSAGE "Customer is not on Users Customer List.  "  SKIP
              "Please add customer to Network Admin - Users Customer List."  VIEW-AS ALERT-BOX WARNING BUTTONS OK.
         ELSE
        MESSAGE "No Customer Found, please update your Search Criteria."
                VIEW-AS ALERT-BOX ERROR.
    END.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_next B-table-Win
ON CHOOSE OF btn_next IN FRAME F-Main /* Show Next */
DO:
   SESSION:SET-WAIT-STATE("general").
  DO WITH FRAME {&FRAME-NAME}:
    
    ASSIGN
    lv-show-next = YES
    ll-show-all = NO.
    ENABLE btn_prev.
    APPLY "choose" TO btn_go.
  END.

  SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_prev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_prev B-table-Win
ON CHOOSE OF btn_prev IN FRAME F-Main /* Show Previous */
DO:
   SESSION:SET-WAIT-STATE("general").
  DO WITH FRAME {&FRAME-NAME}:
    
    ASSIGN
    lv-show-prev = YES
    ll-show-all = NO.
    ENABLE btn_next .
    APPLY "choose" TO btn_go.
  END.

  SESSION:SET-WAIT-STATE("").
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
     DEF VAR lh-column AS HANDLE NO-UNDO.
  DEF VAR lv-column-nam AS CHAR NO-UNDO.
  DEF VAR lv-column-lab AS CHAR NO-UNDO.

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
  
  assign s-rec_key = cust.rec_key when avail cust.
  RUN spec-book-image-proc .  /* task 10221306 */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

&SCOPED-DEFINE cellColumnDat browsers-cust

{methods/browsers/setCellColumns.i}

RUN sys/ref/CustList.p (INPUT cocode,
                            INPUT 'AF1',
                            INPUT YES,
                            OUTPUT lActive).
{sys/inc/chblankcust.i ""AF1""}

   /* gdm - 03090901 */
FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name EQ "RELCREDT" NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company = cocode
        sys-ctrl.name    = "RELCREDT"
        sys-ctrl.log-fld = NO
        sys-ctrl.descrip = "Check credit limit for past due invoices when adding release?".
END.

FI_moveCol = "Sort".
DISPLAY FI_moveCol WITH FRAME {&FRAME-NAME}.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE first-query B-table-Win 
PROCEDURE first-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF ll-first THEN DO:
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
  /*IF v-called-setCellColumns = NO THEN DO:
     RUN setCellColumns.
     v-called-setCellColumns = YES.
  END.        */
  DO WITH FRAME {&FRAME-NAME}:
    fi_sort-by:SCREEN-VALUE = TRIM(lv-sort-by-lab)               + " " +
                              TRIM(STRING(ll-sort-asc,"As/Des")) + "cending".
  END.
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
  IF AVAIL cust THEN APPLY "value-changed" TO BROWSE {&browse-name}.

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
  ASSIGN cust.company:READ-ONLY IN BROWSE {&browse-name} = YES
         cust.cust-no:READ-ONLY IN BROWSE {&browse-name} = YES
         cust.name:READ-ONLY IN BROWSE {&browse-name} = YES
         cust.city:READ-ONLY IN BROWSE {&browse-name} = YES
         cust.state:READ-ONLY IN BROWSE {&browse-name} = YES
         cust.zip:READ-ONLY IN BROWSE {&browse-name} = YES
         cust.area-code:READ-ONLY IN BROWSE {&browse-name} = YES
         cust.phone:READ-ONLY IN BROWSE {&browse-name} = YES
         cust.type:READ-ONLY IN BROWSE {&browse-name} = YES
         cust.sman:READ-ONLY IN BROWSE {&browse-name} = YES
         cust.terr:READ-ONLY IN BROWSE {&browse-name} = YES
         cust.spare-char-2:READ-ONLY IN BROWSE {&browse-name} = YES 
         cust.rec_key:READ-ONLY IN BROWSE {&browse-name} = YES
         FI_moveCol = "Sort".
  
  DISPLAY FI_moveCol WITH FRAME {&FRAME-NAME}.

   APPLY 'ENTRY':U TO fi_cust-no IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  IF ll-show-all THEN DO:
      {fg/j-custinq.i}
  END.
  ELSE IF lv-show-prev OR lv-show-next THEN RUN show-prev-next.
  ELSE RUN first-query.

  IF AVAIL {&first-table-in-query-{&browse-name}} THEN DO:
      RUN dispatch ("display-fields").
      RUN dispatch ("row-changed").

      /*RUN dispatch ('get-last':U).*/

      GET LAST {&browse-name}.
      IF AVAIL cust THEN DO:
         IF ll-sort-asc THEN
            ASSIGN lv-last-rowid  = ROWID(cust)
                 lv-last-show-cust-no = cust.cust-no
                 lvLastRowID  = ROWID(cust).
         ELSE
            ASSIGN lv-frst-rowid = ROWID(cust)
                   lv-first-show-cust-no = cust.cust-no
                   lvFirstRowID  = ROWID(cust).

      END.
      /*RUN dispatch ('get-first':U).*/
      GET FIRST {&browse-name}.
      IF AVAIL cust THEN DO:
        IF ll-sort-asc THEN
          ASSIGN lv-frst-rowid  = ROWID(cust)
                 lv-first-show-cust-no = cust.cust-no
                 lvFirstRowID  = ROWID(cust).
        ELSE
          ASSIGN lv-last-rowid  = ROWID(cust)
                 lv-last-show-cust-no = cust.cust-no
                 lvLastRowID  = ROWID(cust).
      END.
  END.

  ASSIGN
      lv-show-prev = NO
      lv-show-next = NO.

  /*RUN set-rec_key.*/
  
  APPLY "value-changed" TO BROWSE {&browse-name}.
  

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
     Browser-Table:COLUMN-MOVABLE = v-col-move
     Browser-Table:COLUMN-RESIZABLE = v-col-move
     v-col-move = NOT v-col-move
     FI_moveCol = IF v-col-move = NO THEN "Move" ELSE "Sort".
  DISPLAY FI_moveCol.
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
    
  IF ROWID(cust) EQ lvLastRowID THEN
  opNavType = 'L'.
      
  IF ROWID(cust) EQ lvFirstRowID THEN
  opNavType = IF opNavType EQ 'L' THEN 'B' ELSE 'F'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCRM B-table-Win 
PROCEDURE pCRM :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN CRM/crmCustomers.w (g_company).
    RUN query-go.
    APPLY "VALUE-CHANGED":U TO BROWSE {&BROWSE-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCRM B-table-Win 
PROCEDURE pCRMType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcCRMType AS CHARACTER NO-UNDO INITIAL "crmCustomers.".

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
  DEF VAR li AS INT NO-UNDO.
  
  find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "custBrowse"
                        no-lock no-error.
  if not avail sys-ctrl then do transaction:
        create sys-ctrl.
        assign sys-ctrl.company = cocode
               sys-ctrl.name    = "custBrowse"
               sys-ctrl.descrip = "# of Records to be displayed in custor Item browser"
               sys-ctrl.log-fld = YES
               sys-ctrl.char-fld = "cust"
               sys-ctrl.int-fld = 30.
  end.

  {&for-eachblank} NO-LOCK:
    ASSIGN
    li = li + 1
    lv-cust-no = cust.cust-no.
    IF li GE sys-ctrl.int-fld THEN LEAVE.
  END.
  
  &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
        {&for-eachblank}                      ~
          AND cust.cust-no <= lv-cust-no NO-LOCK
            
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
  DEF VAR li AS INT NO-UNDO.
  find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "custBrowse"
                        no-lock no-error.
  if not avail sys-ctrl then do transaction:
        create sys-ctrl.
        assign sys-ctrl.company = cocode
               sys-ctrl.name    = "custBrowse"
               sys-ctrl.descrip = "# of Records to be displayed in custor Item browser"
               sys-ctrl.log-fld = YES
               sys-ctrl.char-fld = "cust"
               sys-ctrl.int-fld = 30.
  end.

  
  IF fi_cust-no NE "" AND fi_cust-no BEGINS '*' THEN DO:  


     {&for-each2} NO-LOCK
             /*USE-INDEX cust-no*/
             BY cust.cust-no :
            ASSIGN
            li = li + 1
            lv-cust-no = cust.cust-no.
            IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
          OPEN QUERY {&browse-name}               ~
            {&for-each2}                          ~
          AND cust.cust-no <= lv-cust-no NO-LOCK

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.


  END.  

  ELSE IF fi_cust-no NE "" AND NOT fi_cust-no BEGINS '*' THEN DO:  

   {&for-each1} NO-LOCK
      /* USE-INDEX cust-no */
       BY cust.cust-no :
      ASSIGN
      li = li + 1
      lv-cust-no = cust.cust-no.
      IF li GE sys-ctrl.int-fld THEN LEAVE.
   END.

   &SCOPED-DEFINE open-query                   ~
       OPEN QUERY {&browse-name}               ~
         {&for-each1}                          ~
           AND cust.cust-no <= lv-cust-no NO-LOCK

   IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                  ELSE {&open-query} {&sortby-phrase-desc}.

  END.
  ELSE IF fi_i-name NE "" AND fi_i-name BEGINS '*'  THEN DO: 

    {&for-each2} NO-LOCK
         /*USE-INDEX cust-part*/
         BY cust.cust-no:
        ASSIGN
        li = li + 1
        lv-cust-no = cust.cust-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2}                          ~
             AND cust.cust-no <= lv-cust-no NO-LOCK

            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_i-name NE "" AND NOT fi_i-name BEGINS '*'  THEN DO: 
      {&for-each1} NO-LOCK
           /*USE-INDEX cust-part*/
           BY cust.cust-no:
          ASSIGN
          li = li + 1
          lv-cust-no = cust.cust-no.
          IF li GE sys-ctrl.int-fld THEN LEAVE.
       END.

       &SCOPED-DEFINE open-query                   ~
           OPEN QUERY {&browse-name}               ~
             {&for-each1}                          ~
               AND cust.cust-no <= lv-cust-no NO-LOCK


       IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                      ELSE {&open-query} {&sortby-phrase-desc}.
  
  END.
  ELSE IF fi_city NE "" AND fi_city BEGINS '*' THEN DO:  
    
        {&for-each2} NO-LOCK
            /* USE-INDEX customer*/
             BY cust.cust-no:
            ASSIGN
            li = li + 1
            lv-cust-no = cust.cust-no.
            IF li GE sys-ctrl.int-fld THEN LEAVE.
         END.

         &SCOPED-DEFINE open-query                   ~
             OPEN QUERY {&browse-name}               ~
               {&for-each2}                          ~
                 AND cust.cust-no <= lv-cust-no NO-LOCK
                        
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_city NE "" AND NOT fi_city BEGINS '*' THEN DO:  
      {&for-each1} NO-LOCK
         /*USE-INDEX customer*/
         BY cust.cust-no:
        ASSIGN
        li = li + 1
        lv-cust-no = cust.cust-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each1}                          ~
             AND cust.cust-no <= lv-cust-no NO-LOCK

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.

  END.
  ELSE IF fi_stat NE "" AND fi_stat BEGINS '*' THEN DO:  
  
    {&for-each2} NO-LOCK
        BY cust.cust-no :
        ASSIGN
        li = li + 1
        lv-cust-no = cust.cust-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2}                          ~
             AND cust.cust-no <= lv-cust-no NO-LOCK
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_stat NE "" AND NOT fi_stat BEGINS '*' THEN DO:  

      {&for-each1} NO-LOCK
          BY cust.cust-no :
          ASSIGN
          li = li + 1
          lv-cust-no = cust.cust-no.
          IF li GE sys-ctrl.int-fld THEN LEAVE.
       END.

       &SCOPED-DEFINE open-query                   ~
           OPEN QUERY {&browse-name}               ~
             {&for-each1}                          ~
               AND cust.cust-no <= lv-cust-no NO-LOCK

       IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                      ELSE {&open-query} {&sortby-phrase-desc}.

  END.
  ELSE IF fi_zip NE "" AND fi_zip BEGINS '*' THEN DO:  

    {&for-each2} NO-LOCK
        /* USE-INDEX procat*/
         BY cust.cust-no :
        ASSIGN
        li = li + 1
        lv-cust-no = cust.cust-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2}                          ~
             AND cust.cust-no <= lv-cust-no NO-LOCK

            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_zip NE "" AND NOT fi_zip BEGINS '*' THEN DO:  
     {&for-each1} NO-LOCK
         /*USE-INDEX procat*/
         BY cust.cust-no :
        ASSIGN
        li = li + 1
        lv-cust-no = cust.cust-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each1}                          ~
             AND cust.cust-no <= lv-cust-no NO-LOCK

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.

  END.
  ELSE IF fi_type NE "" AND fi_type BEGINS '*' THEN DO:  
  
    {&for-each2} NO-LOCK 
        BY cust.cust-no:
        ASSIGN
        li = li + 1
        lv-cust-no = cust.cust-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2}                          ~
             AND cust.cust-no <= lv-cust-no NO-LOCK  
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_type NE "" AND NOT fi_type BEGINS '*' THEN DO:  
      {&for-each1} NO-LOCK 
          BY cust.cust-no:
          ASSIGN
          li = li + 1
          lv-cust-no = cust.cust-no.
          IF li GE sys-ctrl.int-fld THEN LEAVE.
       END.

       &SCOPED-DEFINE open-query                   ~
           OPEN QUERY {&browse-name}               ~
             {&for-each1}                          ~
               AND cust.cust-no <= lv-cust-no NO-LOCK


       IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                      ELSE {&open-query} {&sortby-phrase-desc}.

  END.
  ELSE IF fi_sman NE "" AND fi_sman BEGINS '*' THEN DO:
  
         {&for-each2} NO-LOCK
             /*USE-INDEX estimate*/
             BY cust.cust-no :
            ASSIGN
            li = li + 1
            lv-cust-no = cust.cust-no.
            IF li GE sys-ctrl.int-fld THEN LEAVE.
         END.
    
         &SCOPED-DEFINE open-query                   ~
             OPEN QUERY {&browse-name}               ~
               {&for-each2}                          ~
                 AND cust.cust-no <= lv-cust-no NO-LOCK  
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_sman NE "" AND NOT fi_sman BEGINS '*' THEN DO:

          {&for-each1} NO-LOCK
              /*USE-INDEX estimate*/
              BY cust.cust-no :
             ASSIGN
             li = li + 1
             lv-cust-no = cust.cust-no.
             IF li GE sys-ctrl.int-fld THEN LEAVE.
          END.

          &SCOPED-DEFINE open-query                   ~
              OPEN QUERY {&browse-name}               ~
                {&for-each1}                          ~
                  AND (cust.cust-no <= lv-cust-no) NO-LOCK



      IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                     ELSE {&open-query} {&sortby-phrase-desc}.

  END.
  ELSE IF fi_terr NE "" AND fi_terr BEGINS '*' THEN DO:
  
         {&for-each2} NO-LOCK
             /*USE-INDEX estimate*/
             BY cust.cust-no :
            ASSIGN
            li = li + 1
            lv-cust-no = cust.cust-no.
            IF li GE sys-ctrl.int-fld THEN LEAVE.
         END.
    
         &SCOPED-DEFINE open-query                   ~
             OPEN QUERY {&browse-name}               ~
               {&for-each2}                          ~
                 AND cust.cust-no <= lv-cust-no NO-LOCK  
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_terr NE "" AND NOT fi_terr BEGINS '*' THEN DO:

          {&for-each1} NO-LOCK
              /*USE-INDEX estimate*/
              BY cust.cust-no :
             ASSIGN
             li = li + 1
             lv-cust-no = cust.cust-no.
             IF li GE sys-ctrl.int-fld THEN LEAVE.
          END.

          &SCOPED-DEFINE open-query                   ~
              OPEN QUERY {&browse-name}               ~
                {&for-each1}                          ~
                  AND cust.cust-no <= lv-cust-no NO-LOCK



      IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                     ELSE {&open-query} {&sortby-phrase-desc}.

  END.
  ELSE DO:  
    {&for-eachblank} NO-LOCK
       BREAK BY cust.cust-no :
       IF FIRST-OF(cust.cust-no) THEN li = li + 1.
       lv-cust-no = cust.cust-no.
       IF li GE sys-ctrl.int-fld THEN LEAVE.
    END.

    &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
           {&for-eachblank}                     ~
             AND cust.cust-no <= lv-cust-no NO-LOCK
            
    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                   ELSE {&open-query} {&sortby-phrase-desc}.

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
  def input parameter ip-rowid as rowid no-undo.
 
  run dispatch in this-procedure ("open-query").
  
  reposition {&browse-name} to rowid ip-rowid no-error.

  run dispatch in this-procedure ("row-changed").
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
def input parameter ip-rowid as rowid no-undo.

DEF VAR li AS INT NO-UNDO.

RUN sys/ref/CustList.p (INPUT cocode,
                            INPUT 'AF1',
                            INPUT YES,
                            OUTPUT lActive).
{sys/inc/chblankcust.i ""AF1""}

RUN set-defaults.

{fg/j-custinq.i}

reposition {&browse-name} to rowid ip-rowid no-error.

run dispatch in this-procedure ("row-changed").
 
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
  {src/adm/template/snd-list.i "cust"}

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
     tb_act:SCREEN-VALUE = "yes"
     tb_in-act:SCREEN-VALUE = "no"
     fi_cust-no:SCREEN-VALUE = ""
     fi_i-name:SCREEN-VALUE    = ""
     fi_city:SCREEN-VALUE = ""
     fi_stat:SCREEN-VALUE  = ""
     fi_zip:SCREEN-VALUE  = ""
     fi_type:SCREEN-VALUE  = ""
     fi_sman:SCREEN-VALUE  = ""
     fi_terr:SCREEN-VALUE  = "" 
     .

    DISP tb_act
         tb_in-act
        WITH FRAME {&FRAME-NAME}.
    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-prev-next B-table-Win 
PROCEDURE show-prev-next :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-cust-no AS cha NO-UNDO.
  
  find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "custBrowse"
                        no-lock no-error.
  if not avail sys-ctrl then do transaction:
        create sys-ctrl.
        assign sys-ctrl.company = cocode
               sys-ctrl.name    = "custBrowse"
               sys-ctrl.descrip = "# of Records to be displayed in custor Item browser"
               sys-ctrl.log-fld = YES
               sys-ctrl.char-fld = "cust"
               sys-ctrl.int-fld = 30.
  end.

  IF lv-show-prev THEN DO:

     IF fi_cust-no EQ "" AND fi_i-name EQ "" AND fi_city EQ "" AND
        fi_stat EQ "" AND fi_zip EQ "" AND fi_type EQ "" AND
        fi_terr EQ "" AND fi_sman EQ "" THEN
     DO:
        {&for-eachblank} 
         and cust.cust-no <= lv-first-show-cust-no NO-LOCK BY cust.cust-no DESC :
         ASSIGN
         li = li + 1
         lv-cust-no = cust.cust-no.
         IF li GE sys-ctrl.int-fld THEN LEAVE.       
        END.

        &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
         {&for-eachblank}                          ~
             AND cust.cust-no gE lv-cust-no          ~
             AND cust.cust-no lE lv-first-show-cust-no NO-LOCK

     END.
     ELSE
     DO:
        {&for-each1} 
           and cust.cust-no <= lv-first-show-cust-no NO-LOCK BY cust.cust-no DESC :
           ASSIGN
           li = li + 1
           lv-cust-no = cust.cust-no.
          IF li GE sys-ctrl.int-fld THEN LEAVE.       
        END.
       
        &SCOPED-DEFINE open-query                   ~
            OPEN QUERY {&browse-name}               ~
            {&for-each1}                          ~
                AND cust.cust-no gE lv-cust-no          ~
                AND cust.cust-no lE lv-first-show-cust-no NO-LOCK
      END.

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
     ELSE {&open-query} {&sortby-phrase-desc}.
  
  END.  /* lv-show-prev */
  ELSE IF lv-show-next THEN DO:
      IF fi_cust-no EQ "" AND fi_i-name EQ "" AND fi_city EQ "" AND
        fi_stat EQ "" AND fi_zip EQ "" AND fi_type EQ "" AND
        fi_terr EQ "" AND fi_sman EQ "" THEN
      DO:
         {&for-eachblank} 
         and cust.cust-no >= lv-last-show-cust-no  NO-LOCK:
         ASSIGN
            li = li + 1
            lv-cust-no = cust.cust-no.
         IF li GE sys-ctrl.int-fld THEN LEAVE.
       END.

       &SCOPED-DEFINE open-query                 ~
         OPEN QUERY {&browse-name}               ~
         {&for-eachblank}                        ~
             AND cust.cust-no LE lv-cust-no          ~
             AND cust.cust-no GE lv-last-show-cust-no NO-LOCK

      END.
      ELSE
      DO:
      
      {&for-each1} 
         and cust.cust-no >= lv-last-show-cust-no  NO-LOCK:
         ASSIGN
            li = li + 1
            lv-cust-no = cust.cust-no.
         IF li GE sys-ctrl.int-fld THEN LEAVE.
       END.

       &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
         {&for-each1}                          ~
             AND cust.cust-no LE lv-cust-no          ~
             AND cust.cust-no GE lv-last-show-cust-no NO-LOCK
      END.

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
     ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE DO: /*show all*/
      &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each1} NO-LOCK                         
            
      IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                     ELSE {&open-query} {&sortby-phrase-desc}.
    
  END. /*show all*/


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



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-xl B-table-Win 
PROCEDURE export-xl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR first-cust AS CHAR NO-UNDO.
DEF VAR last-cust AS CHAR NO-UNDO.

GET FIRST Browser-Table .
ASSIGN first-cust = cust.cust-no .
GET LAST Browser-Table .
ASSIGN last-cust = cust.cust-no .

RUN fg/cust-exp.w (first-cust ,last-cust).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spec-book-image-proc B-table-Win 
PROCEDURE spec-book-image-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-spec AS LOG NO-UNDO.
   DEF VAR char-hdl AS CHAR NO-UNDO.
  
  IF AVAIL cust THEN
      v-spec = CAN-FIND(FIRST notes WHERE
               notes.rec_key = cust.rec_key AND
               notes.note_type <> "o").
   
   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'attachcust-target':U, OUTPUT char-hdl).
  
   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN spec-book-image IN WIDGET-HANDLE(char-hdl) (INPUT v-spec).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
