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
DEF VAR lv-sort-by-lab AS CHAR INIT "Ship To"  NO-UNDO.
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
DEF VAR lv-shipto-no AS cha NO-UNDO.
DEF VAR lv-last-show-cust-no AS cha NO-UNDO.
DEF VAR lv-first-show-cust-no AS cha NO-UNDO.
DEF VAR v-rec-key-list AS CHAR NO-UNDO.
DEF VAR cust-rep AS CHAR NO-UNDO.

&IF defined (UIB_is_Running) = 1
  &THEN {methods\defines\phone.i}
  &ELSE {methods\defines\phone.i &NEW="NEW"}
&ENDIF

&SCOPED-DEFINE key-phrase shipto.company EQ cocode

&SCOPED-DEFINE for-each1                          ~
    FOR EACH shipto                               ~
        WHERE {&key-phrase}                       ~
          AND shipto.ship-id      BEGINS fi_ship-id /*"00006"*/     ~
          AND shipto.ship-name   BEGINS fi_i-name   ~
          AND shipto.ship-city   BEGINS fi_city   ~
          AND shipto.ship-state    BEGINS fi_stat   ~
          AND shipto.ship-zip    BEGINS fi_zip      

&SCOPED-DEFINE for-each2                          ~
    FOR EACH shipto                               ~
        WHERE {&key-phrase}                       ~
          AND (IF fi_ship-id BEGINS '*' THEN shipto.ship-id MATCHES  fi_ship-id     ~
               ELSE shipto.ship-id BEGINS  fi_ship-id)   ~
          AND (IF fi_i-name BEGINS '*' THEN shipto.ship-name   MATCHES fi_i-name  ~
               ELSE shipto.ship-NAME   BEGINS fi_i-name) ~
          AND (IF fi_city BEGINS '*' THEN shipto.ship-city   MATCHES fi_city  ~
               ELSE shipto.ship-city   BEGINS fi_city)  ~
          AND (IF fi_stat BEGINS '*' THEN shipto.ship-state MATCHES fi_stat  ~
               ELSE shipto.ship-state    BEGINS fi_stat)   ~
          AND shipto.ship-zip    BEGINS fi_zip   
          

&SCOPED-DEFINE for-eachblank                      ~
    FOR EACH shipto                               ~
        WHERE {&key-phrase} 
        

&SCOPED-DEFINE sortby-log                                                                                                                                  ~
    IF lv-sort-by EQ "ship-id"    THEN shipto.ship-id ELSE ~
    IF lv-sort-by EQ "cust-no"    THEN shipto.cust-no ELSE ~
    IF lv-sort-by EQ "spare-char-1" THEN shipto.spare-char-1 ELSE ~
    IF lv-sort-by EQ "ship-name"    THEN shipto.ship-name    ELSE ~
    IF lv-sort-by EQ "ship-city"    THEN shipto.ship-city ELSE ~
    IF lv-sort-by EQ "ship-state"   THEN shipto.ship-state   ELSE ~
    IF lv-sort-by EQ "cust-rep"      THEN string(display-rep())  ELSE ~
    IF lv-sort-by EQ "ship-zip"     THEN shipto.ship-zip      ELSE ""
    
&SCOPED-DEFINE sortby BY shipto.ship-id

&SCOPED-DEFINE sortby-phrase-asc  ~
    BY ({&sortby-log})            ~
    {&sortby}

&SCOPED-DEFINE sortby-phrase-desc  ~
    BY ({&sortby-log}) DESC        ~
    {&sortby}

ASSIGN cocode = g_company
       locode = g_loc.

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
&Scoped-define INTERNAL-TABLES shipto

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table shipto.cust-no shipto.ship-id ~
shipto.spare-char-1 shipto.ship-name shipto.ship-city shipto.ship-state ~
shipto.ship-zip shipto.company display-rep() @ cust-rep
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH shipto WHERE ~{&KEY-PHRASE} ~
      AND shipto.company = gcompany AND shipto.cust-no <> "" NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH shipto WHERE ~{&KEY-PHRASE} ~
      AND shipto.company = gcompany AND shipto.cust-no <> "" NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table shipto
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table shipto


/* Definitions for FRAME F-Main                                         */
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi_ship-id fi_i-name fi_city fi_stat ~
fi_zip btn_go btn_prev btn_next btn_show ~
Browser-Table 
&Scoped-Define DISPLAYED-OBJECTS fi_ship-id fi_i-name fi_city ~
fi_stat fi_zip fi_sort-by FI_moveCol 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-rep B-table-Win 
FUNCTION display-rep RETURNS CHAR
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
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

DEFINE VARIABLE fi_ship-id AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_zip AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      shipto
    FIELDS(shipto.company
           shipto.cust-no
           shipto.ship-id 
           shipto.spare-char-1
           shipto.ship-name
           shipto.ship-city
           shipto.ship-state 
           shipto.ship-zip ) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      shipto.company FORMAT "x(3)":U
      shipto.cust-no COLUMN-LABEL "Cust #" FORMAT "x(8)":U WIDTH 11.2 LABEL-BGCOLOR 14
      shipto.ship-id COLUMN-LABEL "Ship To" FORMAT "x(8)":U LABEL-BGCOLOR 14
      shipto.spare-char-1 COLUMN-LABEL "Ship Rep" FORMAT "x(8)":U LABEL-BGCOLOR 14
      shipto.ship-name COLUMN-LABEL "Name" FORMAT "x(30)":U LABEL-BGCOLOR 14
      shipto.ship-city FORMAT "x(15)":U LABEL-BGCOLOR 14
      shipto.ship-state FORMAT "x(2)":U WIDTH 6.2 LABEL-BGCOLOR 14
      shipto.ship-zip COLUMN-LABEL "Zip" FORMAT "x(10)":U LABEL-BGCOLOR 14
      display-rep() @ cust-rep COLUMN-LABEL "Cust Rep" FORMAT "x(8)":U LABEL-BGCOLOR 14
       
    ENABLE
     shipto.company  
     shipto.cust-no 
     shipto.ship-id 
     shipto.spare-char-1
     shipto.ship-name
     shipto.ship-city
     shipto.ship-state
     shipto.ship-zip  

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 150 BY 17.90
         FONT 2.


/* ************************  Frame Definitions  *********************** */
DEFINE FRAME F-Main
     fi_ship-id AT ROW 2.19 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     fi_i-name AT ROW 2.19 COL 14.6 COLON-ALIGNED NO-LABEL WIDGET-ID 40
     fi_city AT ROW 2.19 COL 45.2 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     fi_stat AT ROW 2.19 COL 65.8 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     fi_zip AT ROW 2.19 COL 76.2 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     btn_go AT ROW 3.62 COL 1.8 WIDGET-ID 4
     btn_prev AT ROW 3.62 COL 13.8 WIDGET-ID 8
     btn_next AT ROW 3.62 COL 34 WIDGET-ID 6
     btn_show AT ROW 3.62 COL 49.2 WIDGET-ID 10
     fi_sort-by AT ROW 3.62 COL 78 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     FI_moveCol AT ROW 3.62 COL 141 COLON-ALIGNED NO-LABEL WIDGET-ID 46
     Browser-Table AT ROW 5.29 COL 2.4 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     "Ship Name" VIEW-AS TEXT
          SIZE 18 BY .71 AT ROW 1.24 COL 19.8 WIDGET-ID 42
          FGCOLOR 9 FONT 6
     "Shipto Code" VIEW-AS TEXT
          SIZE 14 BY .71 AT ROW 1.24 COL 3 WIDGET-ID 24
          FGCOLOR 9 FONT 6
     "Click on Yellow Field to Sort" VIEW-AS TEXT
          SIZE 28 BY .95 AT ROW 3.62 COL 114 WIDGET-ID 14
     /*"Cust #" VIEW-AS TEXT
          SIZE 12 BY .71 AT ROW 1.24 COL 95.8 WIDGET-ID 34
          FGCOLOR 9 FONT 6
     "Terr" VIEW-AS TEXT
          SIZE 10 BY .71 AT ROW 1.24 COL 123 WIDGET-ID 38
          FGCOLOR 9 FONT 6
     "Rep" VIEW-AS TEXT
          SIZE 10 BY .71 AT ROW 1.24 COL 112.20 WIDGET-ID 38
          FGCOLOR 9 FONT 6*/
     "City" VIEW-AS TEXT
          SIZE 13 BY .71 AT ROW 1.24 COL 50.6 WIDGET-ID 22
          FGCOLOR 9 FONT 6
     "Sorted By:" VIEW-AS TEXT
          SIZE 12 BY 1 AT ROW 3.62 COL 68 WIDGET-ID 30
          FONT 6
     "Ship Zip" VIEW-AS TEXT
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

/*ASSIGN 
       shipto.rec_key:VISIBLE IN BROWSE Browser-Table = FALSE.*/

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
     _TblList          = "ASI.shipto"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED"
     _Where[1]         = "shipto.company = gcompany"
     _FldNameList[1]   = ASI.shipto.company
     _FldNameList[2]   > ASI.shipto.cust-no
"shipto.cust-no" "Cust #" ? "character" ? ? ? 14 ? ? no ? no no "11.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
    _FldNameList[3]   > ASI.shipto.ship-id
"shipto.ship-id" ? ? "character" ? ? ? 14 ? ? no ? no no "11.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.shipto.ship-name
"shipto.ship-name" "Name" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.shipto.spare-char-1
"shipto.spare-char-1" "Ship Rep" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.shipto.ship-city
"shipto.ship-city" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.shipto.ship-state
"shipto.ship-state" ? ? "character" ? ? ? 14 ? ? no ? no no "6.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.shipto.ship-zip
"shipto.ship-zip" "Zip" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
    _FldNameList[9]   > "_<CALC>"
"display-rep() @ cust-rep" "Cust Rep" "character" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
&Scoped-define SELF-NAME fi_i-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-name B-table-Win
ON HELP OF fi_i-name IN FRAME F-Main
DO:
   DEF VAR char-val AS cha NO-UNDO.

   run windows/l-shipt4.w (g_company,"","","", output char-val).
   if char-val <> "" then DO:
       assign fi_i-name:SCREEN-VALUE = entry(2,char-val)
           /*ocat-desc:screen-value = entry(2,char-val)*/ .
       APPLY 'tab' TO fi_i-name.
   END.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fi_ship-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_ship-id B-table-Win
ON HELP OF fi_ship-id IN FRAME F-Main
DO:
   DEF VAR char-val AS cha NO-UNDO.

   run windows/l-shipt4.w (g_company,"","","", output char-val).
   if char-val <> "" then DO:
       assign fi_ship-id:SCREEN-VALUE = entry(1,char-val)
           /*ocat-desc:screen-value = entry(2,char-val)*/ .
       APPLY 'tab' TO fi_ship-id.
   END.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*
&Scoped-define SELF-NAME fi_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cust-no B-table-Win
ON HELP OF fi_cust-no IN FRAME F-Main
DO:
   DEF VAR char-val AS cha NO-UNDO.

   run windows/l-cust.w (g_company,"", output char-val).
   if char-val <> "" then DO:
       assign fi_cust-no:SCREEN-VALUE = entry(1,char-val)
           /*ocat-desc:screen-value = entry(2,char-val)*/ .
       APPLY 'tab' TO fi_cust-no.
   END.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME*/


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

&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Go */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      
    ASSIGN
     fi_ship-id
     fi_i-name
     fi_city
     fi_stat
     fi_zip
     ll-first = NO.
    
    RUN dispatch ("open-query").
    
    GET FIRST Browser-Table .
    IF NOT AVAIL shipto THEN DO:  /* task 07311402 */
        MESSAGE "No Shipto Found, please update your Search Criteria."
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
  
  assign s-rec_key = shipto.rec_key when avail shipto.
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

&SCOPED-DEFINE cellColumnDat browsers-ship

{methods/browsers/setCellColumns.i}


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
  IF AVAIL shipto THEN APPLY "value-changed" TO BROWSE {&browse-name}.

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
  ASSIGN shipto.company:READ-ONLY IN BROWSE {&browse-name} = YES
         shipto.cust-no:READ-ONLY IN BROWSE {&browse-name} = YES
         shipto.ship-id:READ-ONLY IN BROWSE {&browse-name} = YES
         shipto.spare-char-1:READ-ONLY IN BROWSE {&browse-name} = YES
         shipto.ship-name:READ-ONLY IN BROWSE {&browse-name} = YES
         shipto.ship-city:READ-ONLY IN BROWSE {&browse-name} = YES
         shipto.ship-state:READ-ONLY IN BROWSE {&browse-name} = YES
         shipto.ship-zip:READ-ONLY IN BROWSE {&browse-name} = YES
         FI_moveCol = "Sort".
  
  DISPLAY FI_moveCol WITH FRAME {&FRAME-NAME}.

   APPLY 'ENTRY':U TO fi_ship-id IN FRAME {&FRAME-NAME}.

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
      {fg/j-shipinq.i}
  END.
  ELSE IF lv-show-prev OR lv-show-next THEN RUN show-prev-next.
  ELSE RUN first-query.

  IF AVAIL {&first-table-in-query-{&browse-name}} THEN DO:
      RUN dispatch ("display-fields").
      RUN dispatch ("row-changed").

      /*RUN dispatch ('get-last':U).*/

      GET LAST {&browse-name}.
      IF AVAIL shipto THEN DO:
         IF ll-sort-asc THEN
            ASSIGN lv-last-rowid  = ROWID(shipto)
                 lv-last-show-cust-no = shipto.ship-id
                 lvLastRowID  = ROWID(shipto).
         ELSE
            ASSIGN lv-frst-rowid = ROWID(shipto)
                   lv-first-show-cust-no = shipto.ship-id
                   lvFirstRowID  = ROWID(shipto).

      END.
      /*RUN dispatch ('get-first':U).*/
      GET FIRST {&browse-name}.
      IF AVAIL shipto THEN DO:
        IF ll-sort-asc THEN
          ASSIGN lv-frst-rowid  = ROWID(shipto)
                 lv-first-show-cust-no = shipto.ship-id
                 lvFirstRowID  = ROWID(shipto).
        ELSE
          ASSIGN lv-last-rowid  = ROWID(shipto)
                 lv-last-show-cust-no = shipto.ship-id
                 lvLastRowID  = ROWID(shipto).
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
      
  IF ROWID(shipto) EQ lvFirstRowID THEN
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
                      and sys-ctrl.name    eq "shipBrowse"
                        no-lock no-error.
  if not avail sys-ctrl then do transaction:
        create sys-ctrl.
        assign sys-ctrl.company = cocode
               sys-ctrl.name    = "shipBrowse"
               sys-ctrl.descrip = "# of Records to be displayed in Ship to Item browser"
               sys-ctrl.log-fld = YES
               sys-ctrl.char-fld = "ship"
               sys-ctrl.int-fld = 30.
  end.

  {&for-eachblank} NO-LOCK:
    ASSIGN
    li = li + 1
    lv-shipto-no = ship.cust-no.
    IF li GE sys-ctrl.int-fld THEN LEAVE.
  END.
  
  &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
        {&for-eachblank}                      ~
          AND shipto.ship-id <= lv-shipto-no NO-LOCK
            
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
                      and sys-ctrl.name    eq "shipBrowse"
                        no-lock no-error.
  if not avail sys-ctrl then do transaction:
        create sys-ctrl.
        assign sys-ctrl.company = cocode
               sys-ctrl.name    = "shipBrowse"
               sys-ctrl.descrip = "# of Records to be displayed in Shipto Item browser"
               sys-ctrl.log-fld = YES
               sys-ctrl.char-fld = "ship"
               sys-ctrl.int-fld = 30.
  end.

  
  IF fi_ship-id NE "" AND fi_ship-id BEGINS '*' THEN DO:  


     {&for-each2} NO-LOCK
             /*USE-INDEX cust-no*/
             BY shipto.ship-id :
            ASSIGN
            li = li + 1
            lv-shipto-no = shipto.ship-id.
            IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
          OPEN QUERY {&browse-name}               ~
            {&for-each2}                          ~
          AND shipto.ship-id <= lv-shipto-no NO-LOCK

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.


  END.  

  ELSE IF fi_ship-id NE "" AND NOT fi_ship-id BEGINS '*' THEN DO:  
    
      {&for-each1} NO-LOCK
      /* USE-INDEX cust-no */
       BY shipto.ship-id :
      ASSIGN
      li = li + 1
      lv-shipto-no = shipto.ship-id.
      IF li GE sys-ctrl.int-fld THEN LEAVE.
   END.

   &SCOPED-DEFINE open-query                   ~
       OPEN QUERY {&browse-name}               ~
         {&for-each1}                          ~
           AND shipto.ship-id <= lv-shipto-no NO-LOCK

   IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                  ELSE {&open-query} {&sortby-phrase-desc}.

  END.
  
  ELSE IF fi_i-name NE "" AND fi_i-name BEGINS '*'  THEN DO: 

    {&for-each2} NO-LOCK
         /*USE-INDEX cust-part*/
         BY shipto.ship-id:
        ASSIGN
        li = li + 1
        lv-shipto-no = shipto.ship-id.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2}                          ~
             AND shipto.ship-id <= lv-shipto-no NO-LOCK

            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_i-name NE "" AND NOT fi_i-name BEGINS '*'  THEN DO: 
      {&for-each1} NO-LOCK
           /*USE-INDEX cust-part*/
           BY shipto.ship-id:
          ASSIGN
          li = li + 1
          lv-shipto-no = shipto.ship-id.
          IF li GE sys-ctrl.int-fld THEN LEAVE.
       END.

       &SCOPED-DEFINE open-query                   ~
           OPEN QUERY {&browse-name}               ~
             {&for-each1}                          ~
               AND shipto.ship-id <= lv-shipto-no NO-LOCK


       IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                      ELSE {&open-query} {&sortby-phrase-desc}.
  
  END.
  ELSE IF fi_city NE "" AND fi_city BEGINS '*' THEN DO:  
    
        {&for-each2} NO-LOCK
            /* USE-INDEX customer*/
             BY shipto.ship-id:
            ASSIGN
            li = li + 1
            lv-shipto-no = shipto.ship-id.
            IF li GE sys-ctrl.int-fld THEN LEAVE.
         END.

         &SCOPED-DEFINE open-query                   ~
             OPEN QUERY {&browse-name}               ~
               {&for-each2}                          ~
                 AND shipto.ship-id <= lv-shipto-no NO-LOCK
                        
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_city NE "" AND NOT fi_city BEGINS '*' THEN DO:  
      {&for-each1} NO-LOCK
         /*USE-INDEX customer*/
         BY shipto.ship-id:
        ASSIGN
        li = li + 1
        lv-shipto-no = shipto.ship-id.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each1}                          ~
             AND shipto.ship-id <= lv-shipto-no NO-LOCK

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.

  END.
  ELSE IF fi_stat NE "" AND fi_stat BEGINS '*' THEN DO:  
  
    {&for-each2} NO-LOCK
        BY shipto.ship-id :
        ASSIGN
        li = li + 1
        lv-shipto-no = shipto.ship-id.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2}                          ~
             AND shipto.ship-id <= lv-shipto-no NO-LOCK
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_stat NE "" AND NOT fi_stat BEGINS '*' THEN DO:  

      {&for-each1} NO-LOCK
          BY shipto.ship-id :
          ASSIGN
          li = li + 1
          lv-shipto-no = shipto.ship-id.
          IF li GE sys-ctrl.int-fld THEN LEAVE.
       END.

       &SCOPED-DEFINE open-query                   ~
           OPEN QUERY {&browse-name}               ~
             {&for-each1}                          ~
               AND shipto.ship-id <= lv-shipto-no NO-LOCK

       IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                      ELSE {&open-query} {&sortby-phrase-desc}.

  END.
  ELSE IF fi_zip NE "" AND fi_zip BEGINS '*' THEN DO:  

    {&for-each2} NO-LOCK
        /* USE-INDEX procat*/
         BY shipto.ship-id :
        ASSIGN
        li = li + 1
        lv-shipto-no = shipto.ship-id.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2}                          ~
             AND shipto.ship-id <= lv-shipto-no NO-LOCK

            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_zip NE "" AND NOT fi_zip BEGINS '*' THEN DO:  
     {&for-each1} NO-LOCK
         /*USE-INDEX procat*/
         BY shipto.ship-id :
        ASSIGN
        li = li + 1
        lv-shipto-no = shipto.ship-id.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each1}                          ~
             AND shipto.ship-id <= lv-shipto-no NO-LOCK

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.

  END.

  
  /*ELSE IF fi_sman NE "" AND fi_sman BEGINS '*' THEN DO:
  
         {&for-each2} NO-LOCK
             /*USE-INDEX estimate*/
             BY shipto.ship-id :
            ASSIGN
            li = li + 1
            lv-shipto-no = shipto.ship-id.
            IF li GE sys-ctrl.int-fld THEN LEAVE.
         END.
    
         &SCOPED-DEFINE open-query                   ~
             OPEN QUERY {&browse-name}               ~
               {&for-each2}                          ~
                 AND shipto.ship-id <= lv-shipto-no NO-LOCK  
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_sman NE "" AND NOT fi_sman BEGINS '*' THEN DO:

          {&for-each1} NO-LOCK
              /*USE-INDEX estimate*/
              BY cust.cust-no :
             ASSIGN
             li = li + 1
             lv-shipto-no = cust.cust-no.
             IF li GE sys-ctrl.int-fld THEN LEAVE.
          END.

          &SCOPED-DEFINE open-query                   ~
              OPEN QUERY {&browse-name}               ~
                {&for-each1}                          ~
                  AND (cust.cust-no <= lv-shipto-no) NO-LOCK



      IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                     ELSE {&open-query} {&sortby-phrase-desc}.

  END.
  ELSE IF fi_terr NE "" AND fi_terr BEGINS '*' THEN DO:
  
         {&for-each2} NO-LOCK
             /*USE-INDEX estimate*/
             BY shipto.ship-id :
            ASSIGN
            li = li + 1
            lv-shipto-no = shipto.ship-id.
            IF li GE sys-ctrl.int-fld THEN LEAVE.
         END.
    
         &SCOPED-DEFINE open-query                   ~
             OPEN QUERY {&browse-name}               ~
               {&for-each2}                          ~
                 AND shipto.ship-id <= lv-shipto-no NO-LOCK  
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.*/
  
  ELSE DO:  
    {&for-eachblank} NO-LOCK
       BREAK BY shipto.ship-id :
       IF FIRST-OF(shipto.ship-id) THEN li = li + 1.
       lv-shipto-no = shipto.ship-id.
       IF li GE sys-ctrl.int-fld THEN LEAVE.
    END.

    &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
           {&for-eachblank}                     ~
             AND shipto.ship-id <= lv-shipto-no NO-LOCK
            
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

{fg/j-shipinq.i}

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
  {src/adm/template/snd-list.i "shipto"}

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
     fi_ship-id:SCREEN-VALUE = ""
     fi_i-name:SCREEN-VALUE    = ""
     fi_city:SCREEN-VALUE = ""
     fi_stat:SCREEN-VALUE  = ""
     fi_zip:SCREEN-VALUE  = ""
    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-prev-next B-table-Win 
PROCEDURE show-prev-next :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-shipto-no AS cha NO-UNDO.
  
  find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "shipBrowse"
                        no-lock no-error.
  if not avail sys-ctrl then do transaction:
        create sys-ctrl.
        assign sys-ctrl.company = cocode
               sys-ctrl.name    = "shipBrowse"
               sys-ctrl.descrip = "# of Records to be displayed in shipto Item browser"
               sys-ctrl.log-fld = YES
               sys-ctrl.char-fld = "ship"
               sys-ctrl.int-fld = 30.
  end.

  IF lv-show-prev THEN DO:

     IF fi_ship-id EQ "" AND fi_i-name EQ "" AND fi_city EQ "" AND
        fi_stat EQ "" AND fi_zip EQ ""  THEN
     DO:
        {&for-eachblank} 
         and shipto.ship-id <= lv-first-show-cust-no NO-LOCK BY shipto.ship-id DESC :
         ASSIGN
         li = li + 1
         lv-shipto-no = shipto.ship-id.
         IF li GE sys-ctrl.int-fld THEN LEAVE.       
        END.

        &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
         {&for-eachblank}                          ~
             AND shipto.ship-id gE lv-shipto-no          ~
             AND shipto.ship-id lE lv-first-show-cust-no NO-LOCK

     END.
     ELSE
     DO:
        {&for-each1} 
           and shipto.ship-id <= lv-first-show-cust-no NO-LOCK BY shipto.ship-id DESC :
           ASSIGN
           li = li + 1
           lv-shipto-no = shipto.ship-id.
          IF li GE sys-ctrl.int-fld THEN LEAVE.       
        END.
       
        &SCOPED-DEFINE open-query                   ~
            OPEN QUERY {&browse-name}               ~
            {&for-each1}                          ~
                AND shipto.ship-id gE lv-shipto-no          ~
                AND shipto.ship-id lE lv-first-show-cust-no NO-LOCK
      END.

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
     ELSE {&open-query} {&sortby-phrase-desc}.
  
  END.  /* lv-show-prev */
  ELSE IF lv-show-next THEN DO:
      IF fi_ship-id EQ "" AND fi_i-name EQ "" AND fi_city EQ "" AND
        fi_stat EQ "" AND fi_zip EQ ""  THEN
      DO:
         {&for-eachblank} 
         and shipto.ship-id >= lv-last-show-cust-no  NO-LOCK:
         ASSIGN
            li = li + 1
            lv-shipto-no = shipto.ship-id.
         IF li GE sys-ctrl.int-fld THEN LEAVE.
       END.

       &SCOPED-DEFINE open-query                 ~
         OPEN QUERY {&browse-name}               ~
         {&for-eachblank}                        ~
             AND shipto.ship-id LE lv-shipto-no          ~
             AND shipto.ship-id GE lv-last-show-cust-no NO-LOCK

      END.
      ELSE
      DO:
      
      {&for-each1} 
         and shipto.ship-id >= lv-last-show-cust-no  NO-LOCK:
         ASSIGN
            li = li + 1
            lv-shipto-no = shipto.ship-id.
         IF li GE sys-ctrl.int-fld THEN LEAVE.
       END.

       &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
         {&for-each1}                          ~
             AND shipto.ship-id LE lv-shipto-no          ~
             AND shipto.ship-id GE lv-last-show-cust-no NO-LOCK
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
DEFINE VARIABLE cfirst-ship AS CHARACTER NO-UNDO.
DEFINE VARIABLE clast-ship AS CHARACTER NO-UNDO.

GET FIRST Browser-Table .
ASSIGN cfirst-ship = shipto.ship-id .
GET LAST Browser-Table .
ASSIGN clast-ship = shipto.ship-id .
       
RUN fg/ship-exp.w ( "","","").


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

/* ************************  Function Implementations ***************** */


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-rep B-table-Win 
FUNCTION display-rep RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND FIRST cust
          WHERE cust.company EQ cocode
            AND cust.cust-no    EQ shipto.cust-no NO-LOCK NO-ERROR.
  IF AVAIL cust THEN
      RETURN STRING(cust.sman)    .
  ELSE
      RETURN "".
  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
