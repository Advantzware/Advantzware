&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  browsers/vend.w

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
&SCOPED-DEFINE yellowColumnsName vend
&SCOPED-DEFINE cellColumnBrowserName b-vend*/
&SCOPED-DEFINE winReSize
&SCOPED-DEFINE browseOnly
/*&SCOPED-DEFINE useMatches*/
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{custom/gcompany.i}

{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}
{sys/inc/varasgn.i}
  
DEFINE VARIABLE columnCount AS INTEGER NO-UNDO.
DEFINE VARIABLE idx AS INTEGER NO-UNDO.
DEFINE VARIABLE useColors AS CHAR NO-UNDO.
DEFINE VARIABLE lvFirstRowID AS ROWID NO-UNDO.
DEFINE VARIABLE lvLastRowID AS ROWID NO-UNDO.

DEF VAR v-col-move AS LOG INIT YES NO-UNDO.
DEF VAR v-called-setCellColumns AS LOG INIT NO NO-UNDO.
DEF VAR ll-first AS LOG INIT YES NO-UNDO.
DEF VAR lv-sort-by AS CHAR INIT "i-no"  NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "Vendor No"  NO-UNDO.
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
DEF VAR lv-vend-no AS cha NO-UNDO.
DEF VAR lv-last-show-vend-no AS cha NO-UNDO.
DEF VAR lv-first-show-vend-no AS cha NO-UNDO.
DEF VAR v-rec-key-list AS CHAR NO-UNDO.


&IF DEFINED (UIB_Is_Running) = 1
  &THEN {methods\defines\phone.i}
  &ELSE {methods\defines\phone.i &NEW="NEW"}
&ENDIF

&SCOPED-DEFINE key-phrase vend.company EQ cocode

&SCOPED-DEFINE for-each1                          ~
    FOR EACH vend                               ~
        WHERE {&key-phrase}                       ~
          AND vend.vend-no      BEGINS fi_vend-no     ~
          AND vend.NAME   BEGINS fi_i-name   ~
          AND vend.city   BEGINS fi_city   ~
          AND vend.state    BEGINS fi_stat   ~
          AND vend.zip    BEGINS fi_zip   ~
          AND vend.TYPE     BEGINS fi_type    ~
          AND vend.buyer    BEGINS fi_buyer  ~
         AND ((vend.ACTIVE = "A" AND tb_act) OR (vend.ACTIVE = "I" AND tb_in-act))
               

&SCOPED-DEFINE for-each2                          ~
    FOR EACH vend                               ~
        WHERE {&key-phrase}                       ~
          AND (IF fi_vend-no BEGINS '*' THEN vend.vend-no MATCHES  fi_vend-no     ~
               ELSE vend.vend-no BEGINS  fi_vend-no)   ~
          AND (IF fi_i-name BEGINS '*' THEN vend.NAME   MATCHES fi_i-name  ~
               ELSE vend.NAME   BEGINS fi_i-name) ~
          AND (IF fi_city BEGINS '*' THEN vend.city   MATCHES fi_city  ~
               ELSE vend.city   BEGINS fi_city)  ~
          AND (IF fi_stat BEGINS '*' THEN vend.state MATCHES fi_stat  ~
               ELSE vend.state    BEGINS fi_stat)   ~
          AND vend.zip    BEGINS fi_zip   ~
          AND (IF fi_type BEGINS '*' THEN vend.TYPE     MATCHES fi_type    ~
               ELSE vend.TYPE     BEGINS fi_type)  ~
          AND (IF fi_buyer BEGINS '*' THEN vend.buyer    MATCHES fi_buyer   ~
               ELSE vend.buyer    BEGINS fi_buyer) ~
          AND ((vend.ACTIVE = "A" AND tb_act) OR (vend.ACTIVE = "I" AND tb_in-act))


&SCOPED-DEFINE for-eachblank                      ~
    FOR EACH vend                               ~
        WHERE {&key-phrase} ~
         AND ((vend.ACTIVE = "A" AND tb_act) OR (vend.ACTIVE = "I" AND tb_in-act))

&SCOPED-DEFINE sortby-log                                                                                                                                  ~
    IF lv-sort-by EQ "vend-no"    THEN vend.vend-no ELSE ~
    IF lv-sort-by EQ "name"    THEN vend.name    ELSE ~
    IF lv-sort-by EQ "type"    THEN vend.type ELSE ~
    IF lv-sort-by EQ "active"   THEN vend.active   ELSE ~
    IF lv-sort-by EQ "area-code"     THEN vend.area-code      ELSE ~
    IF lv-sort-by EQ "phone"     THEN vend.phone      ELSE ~
    IF lv-sort-by EQ "fax-area"     THEN vend.fax-area      ELSE ~
    IF lv-sort-by EQ "fax"     THEN vend.fax      ELSE ~
    IF lv-sort-by EQ "buyer"     THEN vend.buyer  ELSE ~
    IF lv-sort-by EQ "city"     THEN vend.city  ELSE ~
    IF lv-sort-by EQ "state"     THEN vend.state  ELSE ~
    IF lv-sort-by EQ "zip"     THEN vend.zip  ELSE ""

&SCOPED-DEFINE sortby BY vend.vend-no

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
&Scoped-define INTERNAL-TABLES vend

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table vend.vend-no vend.name ~
vend.type vend.active vend.area-code vend.phone vend.fax-area vend.fax ~
vend.buyer vend.city vend.state vend.zip
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH vend WHERE ~{&KEY-PHRASE} ~
      AND vend.company = gcompany NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH vend WHERE ~{&KEY-PHRASE} ~
      AND vend.company = gcompany NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table vend
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table vend


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tb_act fi_vend-no fi_i-name fi_city fi_stat ~
fi_zip fi_type fi_buyer tb_in-act btn_go btn_prev btn_next btn_show ~
Browser-Table 
&Scoped-Define DISPLAYED-OBJECTS tb_act fi_vend-no fi_i-name fi_city ~
fi_stat fi_zip fi_type fi_buyer tb_in-act fi_sort-by FI_moveCol 

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

DEFINE VARIABLE fi_buyer AS CHARACTER FORMAT "X(3)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_city AS CHARACTER FORMAT "X(8)":U 
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

DEFINE VARIABLE fi_vend-no AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
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

DEFINE VARIABLE tb_in-act AS LOGICAL INITIAL YES 
     LABEL "Inactive" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81
     FGCOLOR 9 FONT 6 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      vend
    FIELDS(vend.vend-no
      vend.name
      vend.type
      vend.active
      vend.area-code
      vend.phone
      vend.fax-area
      vend.fax
      vend.buyer
      vend.city
      vend.state
      vend.zip) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      vend.vend-no COLUMN-LABEL "Vendor" FORMAT "x(8)":U LABEL-BGCOLOR 14
      vend.name FORMAT "x(30)":U LABEL-BGCOLOR 14
      vend.type COLUMN-LABEL "Type" FORMAT "x(8)":U LABEL-BGCOLOR 14
      vend.active COLUMN-LABEL "Status" FORMAT "!":U LABEL-BGCOLOR 14
      vend.area-code FORMAT "(999)":U LABEL-BGCOLOR 14
      vend.phone COLUMN-LABEL "Phone #" FORMAT "999-9999":U LABEL-BGCOLOR 14
      vend.fax-area COLUMN-LABEL "Fax Area Code" FORMAT "(999)":U
            LABEL-BGCOLOR 14
      vend.fax FORMAT "999-9999":U LABEL-BGCOLOR 14
      vend.buyer COLUMN-LABEL "Buyer" FORMAT "x(3)":U LABEL-BGCOLOR 14
      vend.city FORMAT "x(16)":U LABEL-BGCOLOR 14
      vend.state FORMAT "x(5)":U LABEL-BGCOLOR 14
      vend.zip FORMAT "x(10)":U LABEL-BGCOLOR 14
    ENABLE
      vend.vend-no 
      vend.name 
      vend.type 
      vend.active 
      vend.area-code
      vend.phone 
      vend.fax-area 
      vend.fax 
      vend.buyer 
      vend.city
      vend.state
      vend.zip
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 146.6 BY 16.19
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     tb_act AT ROW 1.33 COL 133.8 WIDGET-ID 48
     fi_vend-no AT ROW 2.19 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     fi_i-name AT ROW 2.19 COL 17.6 COLON-ALIGNED NO-LABEL WIDGET-ID 40
     fi_city AT ROW 2.19 COL 48.8 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     fi_stat AT ROW 2.19 COL 70.2 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     fi_zip AT ROW 2.19 COL 81.8 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     fi_type AT ROW 2.19 COL 99.2 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     fi_buyer AT ROW 2.19 COL 116.4 COLON-ALIGNED NO-LABEL WIDGET-ID 36
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
          SIZE 18 BY .71 AT ROW 1.24 COL 20.8 WIDGET-ID 42
          FGCOLOR 9 FONT 6
     "Vendor#" VIEW-AS TEXT
          SIZE 13 BY .71 AT ROW 1.24 COL 5 WIDGET-ID 24
          FGCOLOR 9 FONT 6
     "Click on Yellow Field to Sort" VIEW-AS TEXT
          SIZE 28 BY .95 AT ROW 3.62 COL 114 WIDGET-ID 14
     "Type" VIEW-AS TEXT
          SIZE 12 BY .71 AT ROW 1.24 COL 103 WIDGET-ID 34
          FGCOLOR 9 FONT 6
     "Buyer" VIEW-AS TEXT
          SIZE 12 BY .71 AT ROW 1.24 COL 119 WIDGET-ID 38
          FGCOLOR 9 FONT 6
     "City" VIEW-AS TEXT
          SIZE 13 BY .71 AT ROW 1.24 COL 51.4 WIDGET-ID 22
          FGCOLOR 9 FONT 6
     "Sorted By:" VIEW-AS TEXT
          SIZE 12 BY 1 AT ROW 3.62 COL 68 WIDGET-ID 30
          FONT 6
     "Zip" VIEW-AS TEXT
          SIZE 12 BY .71 AT ROW 1.24 COL 84.8 WIDGET-ID 28
          FGCOLOR 9 FONT 6
     /*"Browser Col. Mode:" VIEW-AS TEXT
          SIZE 22.6 BY .62 AT ROW 18.38 COL 99.6 WIDGET-ID 6
          FONT 6*/
     "State" VIEW-AS TEXT
          SIZE 7.2 BY .71 AT ROW 1.24 COL 72.4 WIDGET-ID 26
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
         HEIGHT             = 20.38
         WIDTH              = 142.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

/*{src/adm/method/browser.i}
{src/adm/method/query.i}
{methods/template/browser.i}
{custom/yellowcolumns.i}*/

{src/adm/method/navbrows.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB Browser-Table FI_moveCol F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "2"
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR FILL-IN FI_moveCol IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_sort-by IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_sort-by:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.vend"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED"
     _Where[1]         = "vend.company = gcompany"
     _FldNameList[1]   > ASI.vend.vend-no
"vend.vend-no" "Vendor" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.vend.name
"vend.name" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.vend.type
"vend.type" "Type" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.vend.active
"vend.active" "Status" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.vend.area-code
"vend.area-code" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.vend.phone
"vend.phone" "Phone #" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.vend.fax-area
"vend.fax-area" "Fax Area Code" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.vend.fax
"vend.fax" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.vend.buyer
"vend.buyer" "Buyer" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
    _FldNameList[10]   > ASI.vend.city
"vend.city" "City" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
    _FldNameList[11]   > ASI.vend.state
"vend.state" "State" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
    _FldNameList[12]   > ASI.vend.zip
"vend.zip" "Zip" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fi_city
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_city V-table-Win
ON HELP OF fi_city IN FRAME F-Main /* City */
DO:
  DEF VAR char-val AS cha NO-UNDO.
  DEF VAR rec-val AS RECID NO-UNDO.
  
       RUN windows/l-city.w (FOCUS:SCREEN-VALUE,OUTPUT char-val,OUTPUT rec-val).
       IF char-val NE "" THEN fi_city:SCREEN-VALUE = ENTRY(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_zip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_zip V-table-Win
ON HELP OF fi_zip IN FRAME F-Main /* Zip */
DO:
  DEF VAR char-val AS cha NO-UNDO.
  DEF VAR city-val AS cha NO-UNDO.
  DEF VAR state-val AS cha NO-UNDO.
  DEF VAR rec-val AS RECID NO-UNDO.
 
       RUN windows/l-zipcod.w (FOCUS:SCREEN-VALUE,OUTPUT char-val,OUTPUT city-val,OUTPUT state-val,OUTPUT rec-val).
       IF char-val NE "" THEN fi_zip:SCREEN-VALUE = ENTRY(1,char-val).
   /*    IF city-val NE "" THEN fi.city:SCREEN-VALUE = ENTRY(1,city-val).
       IF state-val NE "" THEN fi.state:SCREEN-VALUE = ENTRY(1,state-val).

       RUN vend-zip. */

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_stat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_stat V-table-Win
ON HELP OF fi_stat IN FRAME F-Main /* Zip */
DO:
  DEF VAR char-val AS cha NO-UNDO.

       run windows/l-state.w ("", output char-val).
       IF char-val NE "" THEN fi_stat:SCREEN-VALUE = ENTRY(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fi_type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_type V-table-Win
ON HELP OF fi_type IN FRAME F-Main /* Zip */
DO:
  DEF VAR char-val AS cha NO-UNDO.

       run windows/l-vendtyp.w ("", output char-val).
       IF char-val NE "" THEN fi_type:SCREEN-VALUE = ENTRY(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fi_buyer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_buyer V-table-Win
ON HELP OF fi_buyer IN FRAME F-Main /* Whs */
DO:
     DEF VAR char-val AS CHAR.
     run windows/l-buyer.w (cocode,"", output char-val).
     if char-val <> "" then 
        assign fi_buyer:SCREEN-VALUE = entry(1,char-val).
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
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     fi_vend-no
     fi_i-name
     fi_city
     fi_stat
     fi_zip
     fi_type
     fi_buyer
     tb_act
     tb_in-act
     ll-first = NO.
    
    RUN dispatch ("open-query").
    
    GET FIRST Browser-Table .
    IF NOT AVAIL vend THEN DO:  /* task 07311402 */
        /*ASSIGN
            fi_vend-no   = ""
            fi_i-name    = ""
            fi_city      = "" 
            fi_stat      = ""
            fi_zip       = ""
            fi_type      = ""
            fi_buyer     = "" 
            .
    RUN dispatch ("open-query").*/ /* task 12031408 */
    MESSAGE "No Vendor Items Found, please update your Search Criteria."
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
  ASSIGN s-rec_key = vend.rec_key WHEN AVAIL vend.
  RUN spec-book-image-proc .  /* task 11071401 */
  RUN dept-pan-image-proc.
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

&SCOPED-DEFINE cellColumnDat b-vend
{methods/browsers/setCellColumns.i}

FI_moveCol = "Sort".
DISPLAY FI_moveCol WITH FRAME {&FRAME-NAME}.


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-xl B-table-Win 
PROCEDURE export-xl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VAR vfirst-vend AS CHAR NO-UNDO . 
    DEFINE VAR vlast-vend AS CHAR NO-UNDO . 
    
    GET FIRST Browser-Table .
      ASSIGN vfirst-vend = vend.vend-no .
    GET LAST Browser-Table .
      ASSIGN vlast-vend = vend.vend-no .
      
RUN fg/vend-exp.w (vfirst-vend ,vlast-vend).


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

  /*IF v-called-setCellColumns = NO THEN
  DO:
     RUN setCellColumns.
     v-called-setCellColumns = YES.
  END.          */
  
  DO WITH FRAME {&FRAME-NAME}:
    fi_sort-by:SCREEN-VALUE = TRIM(lv-sort-by-lab)               + " " +
                              TRIM(STRING(ll-sort-asc,"As/Des")) + "cending".
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
  ASSIGN vend.vend-no:READ-ONLY IN BROWSE {&browse-name} = YES
         vend.name:READ-ONLY IN BROWSE {&browse-name} = YES
         vend.type:READ-ONLY IN BROWSE {&browse-name} = YES
         vend.active:READ-ONLY IN BROWSE {&browse-name} = YES
         vend.area-code:READ-ONLY IN BROWSE {&browse-name} = YES
         vend.phone:READ-ONLY IN BROWSE {&browse-name} = YES
         vend.fax-area:READ-ONLY IN BROWSE {&browse-name} = YES
         vend.fax:READ-ONLY IN BROWSE {&browse-name} = YES
         vend.buyer:READ-ONLY IN BROWSE {&browse-name} = YES
         vend.city:READ-ONLY IN BROWSE {&browse-name} = YES
         vend.state:READ-ONLY IN BROWSE {&browse-name} = YES
         vend.zip:READ-ONLY IN BROWSE {&browse-name} = YES   
         FI_moveCol = "Sort".
  
  DISPLAY FI_moveCol WITH FRAME {&FRAME-NAME}.

   APPLY 'ENTRY':U TO fi_vend-no IN FRAME {&FRAME-NAME}.

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
      {fg/j-vendinq.i}
  END.
  ELSE IF lv-show-prev OR lv-show-next THEN RUN show-prev-next.
  ELSE RUN first-query.

  IF AVAIL {&first-table-in-query-{&browse-name}} THEN DO:
      RUN dispatch ("display-fields").
      RUN dispatch ("row-changed").

      /*RUN dispatch ('get-last':U).*/

      GET LAST {&browse-name}.
      IF AVAIL vend THEN DO:
         IF ll-sort-asc THEN
            ASSIGN lv-last-rowid  = ROWID(vend)
                 lv-last-show-vend-no = vend.vend-no
                 lvLastRowID  = ROWID(vend).
         ELSE
            ASSIGN lv-frst-rowid = ROWID(vend)
                   lv-first-show-vend-no = vend.vend-no
                   lvFirstRowID  = ROWID(vend).

      END.
      /*RUN dispatch ('get-first':U).*/
      GET FIRST {&browse-name}.
      IF AVAIL vend THEN DO:
        IF ll-sort-asc THEN
          ASSIGN lv-frst-rowid  = ROWID(vend)
                 lv-first-show-vend-no = vend.vend-no
                 lvFirstRowID  = ROWID(vend).
        ELSE
          ASSIGN lv-last-rowid  = ROWID(vend)
                 lv-last-show-vend-no = vend.vend-no
                 lvLastRowID  = ROWID(vend).
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
    
  IF ROWID(vend) EQ lvLastRowID THEN
  opNavType = 'L'.
      
  IF ROWID(vend) EQ lvFirstRowID THEN
  opNavType = IF opNavType EQ 'L' THEN 'B' ELSE 'F'.

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
                      and sys-ctrl.name    eq "VendBrowse"
                        no-lock no-error.
  if not avail sys-ctrl then do transaction:
        create sys-ctrl.
        assign sys-ctrl.company = cocode
               sys-ctrl.name    = "VendBrowse"
               sys-ctrl.descrip = "# of Records to be displayed in Vendor Item browser"
               sys-ctrl.log-fld = YES
               sys-ctrl.char-fld = "Vend"
               sys-ctrl.int-fld = 30.
  end.

  {&for-eachblank} NO-LOCK:
    ASSIGN
    li = li + 1
    lv-vend-no = vend.vend-no.
    IF li GE sys-ctrl.int-fld THEN LEAVE.
  END.
  
  &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
        {&for-eachblank}                      ~
          AND vend.vend-no <= lv-vend-no NO-LOCK
            
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
                      and sys-ctrl.name    eq "VendBrowse"
                        no-lock no-error.
  if not avail sys-ctrl then do transaction:
        create sys-ctrl.
        assign sys-ctrl.company = cocode
               sys-ctrl.name    = "VendBrowse"
               sys-ctrl.descrip = "# of Records to be displayed in Vendor Item browser"
               sys-ctrl.log-fld = YES
               sys-ctrl.char-fld = "Vend"
               sys-ctrl.int-fld = 30.
  end.

  
  IF fi_vend-no NE "" AND fi_vend-no BEGINS '*' THEN DO:  


     {&for-each2} NO-LOCK
             /*USE-INDEX i-no*/
             BY vend.vend-no :
            ASSIGN
            li = li + 1
            lv-vend-no = vend.vend-no.
            IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
          OPEN QUERY {&browse-name}               ~
            {&for-each2}                          ~
          AND vend.vend-no <= lv-vend-no NO-LOCK

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.


  END.  

  ELSE IF fi_vend-no NE "" AND NOT fi_vend-no BEGINS '*' THEN DO:  

   {&for-each1} NO-LOCK
       /*USE-INDEX i-no*/
       BY vend.vend-no :
      ASSIGN
      li = li + 1
      lv-vend-no = vend.vend-no.
      IF li GE sys-ctrl.int-fld THEN LEAVE.
   END.

   &SCOPED-DEFINE open-query                   ~
       OPEN QUERY {&browse-name}               ~
         {&for-each1}                          ~
           AND vend.vend-no <= lv-vend-no NO-LOCK

   IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                  ELSE {&open-query} {&sortby-phrase-desc}.

  END.
  ELSE IF fi_i-name NE "" AND fi_i-name BEGINS '*'  THEN DO: 

    {&for-each2} NO-LOCK
         /*USE-INDEX cust-part*/
         BY vend.NAME:
        ASSIGN
        li = li + 1
        lv-vend-no = vend.vend-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2}                          ~
             AND vend.vend-no <= lv-vend-no NO-LOCK

            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_i-name NE "" AND NOT fi_i-name BEGINS '*'  THEN DO: 
      {&for-each1} NO-LOCK
           /*USE-INDEX cust-part*/
           BY vend.NAME:
          ASSIGN
          li = li + 1
          lv-vend-no = vend.vend-no.
          IF li GE sys-ctrl.int-fld THEN LEAVE.
       END.

       &SCOPED-DEFINE open-query                   ~
           OPEN QUERY {&browse-name}               ~
             {&for-each1}                          ~
               AND vend.vend-no <= lv-vend-no NO-LOCK


       IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                      ELSE {&open-query} {&sortby-phrase-desc}.
  
  END.
  ELSE IF fi_city NE "" AND fi_city BEGINS '*' THEN DO:  
    
        {&for-each2} NO-LOCK
            /* USE-INDEX customer*/
             BY vend.city:
            ASSIGN
            li = li + 1
            lv-vend-no = vend.vend-no.
            IF li GE sys-ctrl.int-fld THEN LEAVE.
         END.

         &SCOPED-DEFINE open-query                   ~
             OPEN QUERY {&browse-name}               ~
               {&for-each2}                          ~
                 AND vend.vend-no <= lv-vend-no NO-LOCK
                        
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_city NE "" AND NOT fi_city BEGINS '*' THEN DO:  
      {&for-each1} NO-LOCK
         /*USE-INDEX customer*/
         BY vend.city:
        ASSIGN
        li = li + 1
        lv-vend-no = vend.vend-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each1}                          ~
             AND vend.vend-no <= lv-vend-no NO-LOCK

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.

  END.
  ELSE IF fi_stat NE "" AND fi_stat BEGINS '*' THEN DO:  
  
    {&for-each2} NO-LOCK
        BY vend.state :
        ASSIGN
        li = li + 1
        lv-vend-no = vend.vend-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2}                          ~
             AND vend.vend-no <= lv-vend-no NO-LOCK
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_stat NE "" AND NOT fi_stat BEGINS '*' THEN DO:  

      {&for-each1} NO-LOCK
          BY vend.state :
          ASSIGN
          li = li + 1
          lv-vend-no = vend.vend-no.
          IF li GE sys-ctrl.int-fld THEN LEAVE.
       END.

       &SCOPED-DEFINE open-query                   ~
           OPEN QUERY {&browse-name}               ~
             {&for-each1}                          ~
               AND vend.vend-no <= lv-vend-no NO-LOCK

       IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                      ELSE {&open-query} {&sortby-phrase-desc}.

  END.
  ELSE IF fi_zip NE "" AND fi_zip BEGINS '*' THEN DO:  

    {&for-each2} NO-LOCK
        /* USE-INDEX procat*/
         BY vend.zip :
        ASSIGN
        li = li + 1
        lv-vend-no = vend.vend-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2}                          ~
             AND vend.vend-no <= lv-vend-no NO-LOCK

            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_zip NE "" AND NOT fi_zip BEGINS '*' THEN DO:  
     {&for-each1} NO-LOCK
         /*USE-INDEX procat*/
         BY vend.zip :
        ASSIGN
        li = li + 1
        lv-vend-no = vend.vend-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each1}                          ~
             AND vend.vend-no <= lv-vend-no NO-LOCK

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.

  END.
  ELSE IF fi_type NE "" AND fi_type BEGINS '*' THEN DO:  
  
    {&for-each2} NO-LOCK 
        BY vend.TYPE:
        ASSIGN
        li = li + 1
        lv-vend-no = vend.vend-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2}                          ~
             AND vend.vend-no <= lv-vend-no NO-LOCK  
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_type NE "" AND NOT fi_type BEGINS '*' THEN DO:  
      {&for-each1} NO-LOCK 
          BY vend.TYPE:
          ASSIGN
          li = li + 1
          lv-vend-no = vend.vend-no.
          IF li GE sys-ctrl.int-fld THEN LEAVE.
       END.

       &SCOPED-DEFINE open-query                   ~
           OPEN QUERY {&browse-name}               ~
             {&for-each1}                          ~
               AND vend.vend-no <= lv-vend-no NO-LOCK


       IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                      ELSE {&open-query} {&sortby-phrase-desc}.

  END.
  ELSE IF fi_buyer NE "" AND fi_buyer BEGINS '*' THEN DO:
  
         {&for-each2} NO-LOCK
             /*USE-INDEX estimate*/
             BY vend.buyer :
            ASSIGN
            li = li + 1
            lv-vend-no = vend.vend-no.
            IF li GE sys-ctrl.int-fld THEN LEAVE.
         END.
    
         &SCOPED-DEFINE open-query                   ~
             OPEN QUERY {&browse-name}               ~
               {&for-each2}                          ~
                 AND vend.vend-no <= lv-vend-no NO-LOCK  
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_buyer NE "" AND NOT fi_buyer BEGINS '*' THEN DO:

          {&for-each1} NO-LOCK
              /*USE-INDEX estimate*/
              BY vend.buyer :
             ASSIGN
             li = li + 1
             lv-vend-no = vend.vend-no.
             IF li GE sys-ctrl.int-fld THEN LEAVE.
          END.

          &SCOPED-DEFINE open-query                   ~
              OPEN QUERY {&browse-name}               ~
                {&for-each1}                          ~
                  AND vend.vend-no <= lv-vend-no NO-LOCK



      IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                     ELSE {&open-query} {&sortby-phrase-desc}.

  END.
  ELSE DO:  
      
    {&for-eachblank} NO-LOCK
       BREAK BY vend.vend-no :
       IF FIRST-OF(vend.vend-no) THEN li = li + 1.
       lv-vend-no = vend.vend-no.
       IF li GE sys-ctrl.int-fld THEN LEAVE.
    END.

    &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
           {&for-eachblank}                     ~
             AND vend.vend-no <= lv-vend-no NO-LOCK
            
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

RUN set-defaults.

{fg/j-vendinq.i}

reposition {&browse-name} to rowid ip-rowid no-error.

run dispatch in this-procedure ("row-changed").
 
APPLY "value-changed" TO BROWSE {&browse-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repositionToNew B-table-Win 
PROCEDURE repositionToNew :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Force to reposition to newly added record (called from window)
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcNewVend AS CHARACTER   NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN
    fi_buyer:SCREEN-VALUE = ""
    fi_city:SCREEN-VALUE = ""
    fi_i-name:SCREEN-VALUE = ""
    fi_stat:SCREEN-VALUE = ""
    fi_type:SCREEN-VALUE = ""
    fi_vend-no:SCREEN-VALUE = ipcNewVend
    tb_act:SCREEN-VALUE = "yes"
    fi_zip:SCREEN-VALUE = "".

  ASSIGN
     fi_vend-no
     fi_i-name
     fi_city
     fi_stat
     fi_zip
     fi_type
     fi_buyer
     tb_act
     tb_in-act
     ll-first = NO.
  APPLY 'GO' TO btn_go.

   ASSIGN
     fi_vend-no
     fi_i-name
     fi_city
     fi_stat
     fi_zip
     fi_type
     fi_buyer
     tb_act
     tb_in-act
     ll-first = NO.
    
    RUN dispatch ("open-query").


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
  {src/adm/template/snd-list.i "vend"}

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
     fi_vend-no:SCREEN-VALUE = ""
     fi_i-name:SCREEN-VALUE    = ""
     fi_city:SCREEN-VALUE = ""
     fi_stat:SCREEN-VALUE  = ""
     fi_zip:SCREEN-VALUE  = ""
     fi_type:SCREEN-VALUE  = ""  
     fi_buyer:SCREEN-VALUE  = ""
     fi_vend-no   = ""
     fi_i-name    = ""
     fi_city      = "" 
     fi_stat      = ""
     fi_zip       = ""
     fi_type      = ""
     fi_buyer     = ""  .

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
  DEF VAR lv-i-no AS cha NO-UNDO.
  
  find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "VendBrowse"
                        no-lock no-error.
  if not avail sys-ctrl then do transaction:
        create sys-ctrl.
        assign sys-ctrl.company = cocode
               sys-ctrl.name    = "VendBrowse"
               sys-ctrl.descrip = "# of Records to be displayed in Vendor Item browser"
               sys-ctrl.log-fld = YES
               sys-ctrl.char-fld = "Vend"
               sys-ctrl.int-fld = 30.
  end.

  IF lv-show-prev THEN DO:

     IF fi_vend-no EQ "" AND fi_i-name EQ "" AND fi_city EQ "" AND
        fi_stat EQ "" AND fi_zip EQ "" AND fi_type EQ "" AND
        fi_buyer EQ "" THEN
     DO:
        {&for-eachblank} 
         and vend.vend-no <= lv-first-show-vend-no NO-LOCK BY vend.vend-no DESC :
         ASSIGN
         li = li + 1
         lv-vend-no = vend.vend-no.
         IF li GE sys-ctrl.int-fld THEN LEAVE.       
        END.

        &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
         {&for-eachblank}                          ~
             AND vend.vend-no gE lv-vend-no          ~
             AND vend.vend-no lE lv-first-show-vend-no NO-LOCK

     END.
     ELSE
     DO:
        {&for-each1} 
           and vend.vend-no <= lv-first-show-vend-no NO-LOCK BY vend.vend-no DESC :
           ASSIGN
           li = li + 1
           lv-vend-no = vend.vend-no.
          IF li GE sys-ctrl.int-fld THEN LEAVE.       
        END.
       
        &SCOPED-DEFINE open-query                   ~
            OPEN QUERY {&browse-name}               ~
            {&for-each1}                          ~
                AND vend.vend-no gE lv-vend-no          ~
                AND vend.vend-no lE lv-first-show-vend-no NO-LOCK
      END.

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
     ELSE {&open-query} {&sortby-phrase-desc}.
  
  END.  /* lv-show-prev */
  ELSE IF lv-show-next THEN DO:
      IF fi_vend-no EQ "" AND fi_i-name EQ "" AND fi_city EQ "" AND
        fi_stat EQ "" AND fi_zip EQ "" AND fi_type EQ "" AND
        fi_buyer EQ "" THEN
      DO:
         {&for-eachblank} 
         and vend.vend-no >= lv-last-show-vend-no  NO-LOCK:
         ASSIGN
            li = li + 1
            lv-vend-no = vend.vend-no.
         IF li GE sys-ctrl.int-fld THEN LEAVE.
       END.

       &SCOPED-DEFINE open-query                 ~
         OPEN QUERY {&browse-name}               ~
         {&for-eachblank}                        ~
             AND vend.vend-no LE lv-vend-no          ~
             AND vend.vend-no GE lv-last-show-vend-no NO-LOCK

      END.
      ELSE
      DO:
      
      {&for-each1} 
         and vend.vend-no >= lv-last-show-vend-no  NO-LOCK:
         ASSIGN
            li = li + 1
            lv-vend-no = vend.vend-no.
         IF li GE sys-ctrl.int-fld THEN LEAVE.
       END.

       &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
         {&for-each1}                          ~
             AND vend.vend-no LE lv-vend-no          ~
             AND vend.vend-no GE lv-last-show-vend-no NO-LOCK
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



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spec-book-image-proc B-table-Win 
PROCEDURE spec-book-image-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-spec AS LOG NO-UNDO.
   DEF VAR char-hdl AS CHAR NO-UNDO.
  
  IF AVAIL vend THEN
      v-spec = CAN-FIND(FIRST notes WHERE
               notes.rec_key = vend.rec_key AND
               notes.note_type <> "o").
   
   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'attachvend-target':U, OUTPUT char-hdl).
  
   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN spec-book-image-change IN WIDGET-HANDLE(char-hdl) (INPUT v-spec).
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
   IF AVAIL vend THEN
    FIND FIRST notes WHERE notes.rec_key = vend.rec_key
       NO-LOCK NO-ERROR.

   IF AVAIL notes THEN
      v-spec = TRUE.
   ELSE v-spec = FALSE.

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'attachvend-target':U, OUTPUT char-hdl).

   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN dept-pen-image IN WIDGET-HANDLE(char-hdl) (INPUT v-spec).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
