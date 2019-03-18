&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: fg\b-itemfg.w

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
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
{sys/inc/varasgn.i}

/* DEFINE VARIABLE cellColumn AS HANDLE NO-UNDO EXTENT 20. */
DEFINE VARIABLE columnCount AS INTEGER NO-UNDO.
DEFINE VARIABLE idx AS INTEGER NO-UNDO.
DEFINE VARIABLE useColors AS CHAR NO-UNDO.
DEFINE VARIABLE gcompany AS CHAR NO-UNDO.
DEFINE VARIABLE lvFirstRowID AS ROWID NO-UNDO.
DEFINE VARIABLE lvLastRowID AS ROWID NO-UNDO.

DEF VAR ll-first AS LOG INIT YES NO-UNDO.
DEF VAR lv-sort-by AS CHAR INIT "i-no"  NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "Item No"  NO-UNDO.
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
DEF VAR lv-i-no AS cha NO-UNDO.
DEF VAR lv-last-show-i-no AS cha NO-UNDO.
DEF VAR lv-first-show-i-no AS cha NO-UNDO.
DEF VAR v-rec-key-list AS CHAR NO-UNDO.
DEF VAR v-col-move AS LOG NO-UNDO INIT TRUE.
DEF VAR lActive AS LOG NO-UNDO.
DO TRANSACTION:
     {sys/ref/CustList.i NEW}
    {sys/inc/custlistform.i ""IF1"" }
END.

&SCOPED-DEFINE key-phrase itemfg.company EQ cocode

&SCOPED-DEFINE for-each1                          ~
    FOR EACH itemfg                               ~
        WHERE {&key-phrase}                       ~
          AND itemfg.i-no      BEGINS fi_i-no     ~
          AND itemfg.part-no   BEGINS fi_part-no  ~
          AND ( (lookup(itemfg.cust-no,custcount) <> 0  AND itemfg.cust-no <> "") OR custcount = "") ~
          AND itemfg.cust-no   BEGINS fi_cust-no  ~
          AND (IF fi_i-name BEGINS '*' THEN itemfg.i-name MATCHES fi_i-name  ~
             ELSE itemfg.i-name    BEGINS fi_i-name)   ~
          AND itemfg.est-no    BEGINS fi_est-no   ~
          AND itemfg.style     BEGINS fi_style    ~
          AND itemfg.procat    BEGINS fi_procat   ~
          AND (((itemfg.stat = "A" AND tb_act) OR (itemfg.stat = "" AND tb_act)) OR (itemfg.stat = "I" AND tb_in-act))

&SCOPED-DEFINE for-each2                          ~
    FOR EACH itemfg                               ~
        WHERE {&key-phrase}                       ~
          AND (IF fi_i-no BEGINS '*' THEN itemfg.i-no MATCHES  fi_i-no     ~
               ELSE itemfg.i-no BEGINS  fi_i-no)   ~
          AND ( (lookup(itemfg.cust-no,custcount) <> 0 AND itemfg.cust-no <> "") OR custcount = "")   ~
          AND (IF fi_part-no BEGINS '*' THEN itemfg.part-no   MATCHES fi_part-no  ~
               ELSE itemfg.part-no   BEGINS fi_part-no) ~
          AND (IF fi_cust-no BEGINS '*' THEN itemfg.cust-no   MATCHES fi_cust-no  ~
               ELSE itemfg.cust-no   BEGINS fi_cust-no)  ~
          AND (IF fi_i-name BEGINS '*' THEN itemfg.i-name MATCHES fi_i-name  ~
               ELSE itemfg.i-name    BEGINS fi_i-name)   ~
          AND itemfg.est-no    BEGINS fi_est-no   ~
          AND (IF fi_style BEGINS '*' THEN itemfg.style     MATCHES fi_style    ~
               ELSE itemfg.style     BEGINS fi_style)  ~
          AND (IF fi_procat BEGINS '*' THEN itemfg.procat    MATCHES fi_procat   ~
               ELSE itemfg.procat    BEGINS fi_procat)  ~
          AND (((itemfg.stat = "A" AND tb_act) OR (itemfg.stat = "" AND tb_act)) OR (itemfg.stat = "I" AND tb_in-act))

&SCOPED-DEFINE for-eachblank                      ~
    FOR EACH itemfg                               ~
        WHERE {&key-phrase}                       ~
        AND ( (lookup(itemfg.cust-no,custcount) <> 0 AND itemfg.cust-no <> "") OR custcount = "") ~
        AND (((itemfg.stat = "A" AND tb_act) OR (itemfg.stat = "" AND tb_act)) OR (itemfg.stat = "I" AND tb_in-act))

&SCOPED-DEFINE sortby-log                                                                                                                                  ~
    IF lv-sort-by EQ "i-no"    THEN itemfg.i-no ELSE ~
    IF lv-sort-by EQ "part-no"    THEN itemfg.part-no    ELSE ~
    IF lv-sort-by EQ "i-name"    THEN itemfg.i-name ELSE ~
    IF lv-sort-by EQ "cust-no"   THEN itemfg.cust-no   ELSE ~
    IF lv-sort-by EQ "est-no"     THEN itemfg.est-no      ELSE ~
    IF lv-sort-by EQ "style"     THEN itemfg.style      ELSE ~
    IF lv-sort-by EQ "cad-no"     THEN itemfg.cad-no      ELSE ~
    IF lv-sort-by EQ "spc-no"     THEN itemfg.spc-no      ELSE ~
    IF lv-sort-by EQ "i-code"     THEN itemfg.i-code      ELSE ~
    IF lv-sort-by EQ "stocked"     THEN string(itemfg.stocked)      ELSE ~
    IF lv-sort-by EQ "q-onh"     THEN string(itemfg.q-onh)      ELSE ~
    IF lv-sort-by EQ "procat"   THEN itemfg.procat       ELSE ""

&SCOPED-DEFINE sortby BY itemfg.i-no

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
&Scoped-define BROWSE-NAME r_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES itemfg

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE r_table                                       */
&Scoped-define FIELDS-IN-QUERY-r_table itemfg.i-no itemfg.i-name ~
itemfg.part-dscr1 itemfg.cust-no itemfg.style itemfg.procat itemfg.i-code ~
itemfg.est-no itemfg.cad-no itemfg.spc-no itemfg.stocked itemfg.q-onh ~
itemfg.part-no 
&Scoped-define ENABLED-FIELDS-IN-QUERY-r_table itemfg.i-no itemfg.i-name ~
itemfg.part-dscr1 itemfg.cust-no itemfg.style itemfg.procat itemfg.i-code ~
itemfg.est-no itemfg.cad-no itemfg.spc-no itemfg.stocked itemfg.q-onh 
&Scoped-define ENABLED-TABLES-IN-QUERY-r_table itemfg
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-r_table itemfg
&Scoped-define QUERY-STRING-r_table FOR EACH itemfg WHERE ~{&KEY-PHRASE} ~
      AND itemfg.company eq g_company and ~
asi.itemfg.i-no eq "###" NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-r_table OPEN QUERY r_table FOR EACH itemfg WHERE ~{&KEY-PHRASE} ~
      AND itemfg.company eq g_company and ~
asi.itemfg.i-no eq "###" NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-r_table itemfg
&Scoped-define FIRST-TABLE-IN-QUERY-r_table itemfg


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tb_act fi_i-no fi_part-no fi_cust-no ~
fi_i-name fi_est-no fi_style fi_procat tb_in-act btn_go btn_next btn_show ~
fi_movecol r_table 
&Scoped-Define DISPLAYED-OBJECTS tb_act fi_i-no fi_part-no fi_cust-no ~
fi_i-name fi_est-no fi_style fi_procat tb_in-act fi_sort-by fi_movecol 

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

DEFINE VARIABLE fi_cust-no AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_est-no AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_i-name AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_i-no AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_movecol AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi_part-no AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_procat AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_sort-by AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE fi_style AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_act AS LOGICAL INITIAL yes 
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
DEFINE QUERY r_table FOR 
      itemfg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE r_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS r_table B-table-Win _STRUCTURED
  QUERY r_table NO-LOCK DISPLAY
      itemfg.i-no FORMAT "x(15)":U LABEL-BGCOLOR 14
      itemfg.i-name FORMAT "x(30)":U LABEL-BGCOLOR 14
      itemfg.part-dscr1 COLUMN-LABEL "Description" FORMAT "x(30)":U
            LABEL-BGCOLOR 14
      itemfg.cust-no FORMAT "x(8)":U LABEL-BGCOLOR 14
      itemfg.style COLUMN-LABEL "Style" FORMAT "x(6)":U LABEL-BGCOLOR 14
      itemfg.procat FORMAT "x(5)":U LABEL-BGCOLOR 14
      itemfg.i-code COLUMN-LABEL "Stock/!Custom" FORMAT "X":U LABEL-BGCOLOR 14
      itemfg.est-no COLUMN-LABEL "Estimate" FORMAT "x(8)":U LABEL-BGCOLOR 14
      itemfg.cad-no COLUMN-LABEL "CAD #" FORMAT "x(15)":U LABEL-BGCOLOR 14
      itemfg.spc-no COLUMN-LABEL "Quality/SPC #" FORMAT "x(15)":U
            LABEL-BGCOLOR 14
      itemfg.stocked COLUMN-LABEL "Stocked?" FORMAT "Yes/No":U
            LABEL-BGCOLOR 14
      itemfg.q-onh FORMAT "->>,>>>,>>9":U LABEL-BGCOLOR 14
      itemfg.part-no FORMAT "x(20)":U WIDTH 22 LABEL-BGCOLOR 14
  ENABLE
      itemfg.i-no
      itemfg.i-name
      itemfg.part-dscr1
      itemfg.cust-no
      itemfg.style
      itemfg.procat
      itemfg.i-code
      itemfg.est-no
      itemfg.cad-no
      itemfg.spc-no
      itemfg.stocked
      itemfg.q-onh
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 156 BY 20.95
         BGCOLOR 8 FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     tb_act AT ROW 1.43 COL 142.8 WIDGET-ID 48
     fi_i-no AT ROW 2.19 COL 2.6 NO-LABEL WIDGET-ID 2
     fi_part-no AT ROW 2.19 COL 24.6 NO-LABEL WIDGET-ID 40
     fi_cust-no AT ROW 2.19 COL 45.6 NO-LABEL WIDGET-ID 16
     fi_i-name AT ROW 2.19 COL 62.4 NO-LABEL WIDGET-ID 20
     fi_est-no AT ROW 2.19 COL 98.2 NO-LABEL WIDGET-ID 18
     fi_style AT ROW 2.19 COL 113.2 NO-LABEL WIDGET-ID 32
     fi_procat AT ROW 2.19 COL 124.2 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     tb_in-act AT ROW 2.57 COL 142.8 WIDGET-ID 50
     btn_go AT ROW 3.62 COL 1.8 WIDGET-ID 4
     btn_prev AT ROW 3.62 COL 13.8 WIDGET-ID 8
     btn_next AT ROW 3.62 COL 34 WIDGET-ID 6
     btn_show AT ROW 3.62 COL 49.2 WIDGET-ID 10
     fi_sort-by AT ROW 3.62 COL 78 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     fi_movecol AT ROW 3.62 COL 141 COLON-ALIGNED NO-LABEL WIDGET-ID 46
     r_table AT ROW 4.81 COL 1
     "Customer Part#" VIEW-AS TEXT
          SIZE 18 BY .71 AT ROW 1.24 COL 25.8 WIDGET-ID 42
          FGCOLOR 9 FONT 6
     "Estimate#" VIEW-AS TEXT
          SIZE 12 BY .71 AT ROW 1.24 COL 100 WIDGET-ID 28
          FGCOLOR 9 FONT 6
     "Sorted By:" VIEW-AS TEXT
          SIZE 12 BY 1 AT ROW 3.62 COL 68 WIDGET-ID 30
          FONT 6
     "Customer#" VIEW-AS TEXT
          SIZE 13 BY .71 AT ROW 1.24 COL 49.2 WIDGET-ID 22
          FGCOLOR 9 FONT 6
     "Category" VIEW-AS TEXT
          SIZE 12 BY .71 AT ROW 1.24 COL 127 WIDGET-ID 38
          FGCOLOR 9 FONT 6
     "Style" VIEW-AS TEXT
          SIZE 10 BY .71 AT ROW 1.24 COL 116 WIDGET-ID 34
          FGCOLOR 9 FONT 6
     "Click on Yellow Field to Sort" VIEW-AS TEXT
          SIZE 28 BY .95 AT ROW 3.62 COL 114 WIDGET-ID 14
     "Name" VIEW-AS TEXT
          SIZE 7.2 BY .71 AT ROW 1.24 COL 77.4 WIDGET-ID 26
          FGCOLOR 9 FONT 6
     "FG Item#" VIEW-AS TEXT
          SIZE 13 BY .71 AT ROW 1.24 COL 6.6 WIDGET-ID 24
          FGCOLOR 9 FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 
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
         HEIGHT             = 25.1
         WIDTH              = 159.2.
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
/* BROWSE-TAB r_table fi_movecol F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btn_prev IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_cust-no IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi_est-no IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi_i-name IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi_i-no IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi_part-no IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi_sort-by IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_style IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       r_table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 1
       r_table:PRIVATE-DATA IN FRAME F-Main           = 
                "2"
       r_table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE r_table
/* Query rebuild information for BROWSE r_table
     _TblList          = "asi.itemfg"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "asi.itemfg.company eq g_company and
asi.itemfg.i-no eq ""###"""
     _FldNameList[1]   > asi.itemfg.i-no
"itemfg.i-no" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.itemfg.i-name
"itemfg.i-name" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > asi.itemfg.part-dscr1
"itemfg.part-dscr1" "Description" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > asi.itemfg.cust-no
"itemfg.cust-no" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > asi.itemfg.style
"itemfg.style" "Style" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > asi.itemfg.procat
"itemfg.procat" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > asi.itemfg.i-code
"itemfg.i-code" "Stock/!Custom" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > asi.itemfg.est-no
"itemfg.est-no" "Estimate" "x(8)" "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > asi.itemfg.cad-no
"itemfg.cad-no" "CAD #" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > asi.itemfg.spc-no
"itemfg.spc-no" "Quality/SPC #" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > asi.itemfg.stocked
"itemfg.stocked" "Stocked?" ? "logical" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > asi.itemfg.q-onh
"itemfg.q-onh" ? "->>,>>>,>>9" "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > asi.itemfg.part-no
"itemfg.part-no" ? "x(20)" "character" ? ? ? 14 ? ? no ? no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE r_table */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main B-table-Win
ON HELP OF FRAME F-Main
DO:
   DEF VAR char-val AS CHAR NO-UNDO.
   DEFINE BUFFER b-itemfg FOR itemfg.

   CASE FOCUS:NAME:
      WHEN "fi_i-no" THEN DO:
         RUN windows/l-itmfg2.w (INPUT g_company, INPUT fi_cust-no, INPUT fi_i-no:SCREEN-VALUE, INPUT 1, OUTPUT char-val).
         IF char-val <> "" THEN DO:
            ASSIGN 
               fi_i-no:SCREEN-VALUE = ENTRY(1,char-val).
              APPLY "entry" TO fi_i-no.
         END.                           
      END. 
      WHEN "fi_part-no" THEN DO:
         RUN windows/l-itemfg.w (cocode,fi_cust-no:SCREEN-VALUE,fi_part-no:SCREEN-VALUE,OUTPUT char-val) NO-ERROR.
         IF char-val <> "" THEN DO:
            FIND FIRST b-itemfg WHERE b-itemfg.company = g_company  
                                  AND b-itemfg.i-no = ENTRY(1,char-val) NO-LOCK NO-ERROR.
            IF AVAILABLE(b-itemfg) THEN
            ASSIGN fi_part-no:SCREEN-VALUE = b-itemfg.part-no.
            APPLY "entry" TO fi_cust-no.
         END.                           
      END.  
      WHEN "fi_cust-no" THEN DO:
         RUN windows/l-cust2.w (INPUT g_company, INPUT fi_cust-no:SCREEN-VALUE, "", OUTPUT char-val).
         IF char-val <> "" THEN DO:
            ASSIGN 
               fi_cust-no:SCREEN-VALUE = ENTRY(1,char-val).
              APPLY "entry" TO fi_cust-no.
         END.                           
      END.  
      WHEN "fi_i-name" THEN DO:
         RUN windows/l-itmfg2.w (INPUT g_company, INPUT fi_cust-no, INPUT fi_i-name:SCREEN-VALUE, INPUT 2, OUTPUT char-val).
         IF char-val <> "" THEN DO:
            ASSIGN 
               fi_i-name:SCREEN-VALUE = ENTRY(2,char-val).
              APPLY "entry" TO fi_i-name.
         END.                           
      END.  
      WHEN "fi_est-no" THEN DO:
         RUN windows/l-itmfg2.w (INPUT g_company, INPUT fi_cust-no, INPUT fi_est-no:SCREEN-VALUE, INPUT 3, OUTPUT char-val).
         IF char-val <> "" THEN DO:
            ASSIGN 
               fi_est-no:SCREEN-VALUE = TRIM(ENTRY(3,char-val)).
              APPLY "entry" TO fi_est-no.
         END.                           
      END.  
      WHEN "fi_style" THEN DO:
         RUN windows/l-itmfg2.w (INPUT g_company, INPUT fi_cust-no, INPUT fi_style:SCREEN-VALUE, INPUT 4, OUTPUT char-val).
         IF char-val <> "" THEN DO:
            ASSIGN 
               fi_style:SCREEN-VALUE = ENTRY(4,char-val).
              APPLY "entry" TO fi_style.
         END.                           
      END.  
      WHEN "fi_procat" THEN DO:
         RUN windows/l-itmfg2.w (INPUT g_company, INPUT fi_cust-no, INPUT fi_procat:SCREEN-VALUE, INPUT 5, OUTPUT char-val).
         IF char-val <> "" THEN DO:
            ASSIGN 
               fi_procat:SCREEN-VALUE = ENTRY(5,char-val).
              APPLY "entry" TO fi_procat.
         END.                           
      END.  
    END CASE.
    APPLY "CHOOSE" TO btn_go.
    RETURN NO-APPLY.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Go */
DO:
    DEF VAR v-cust-no AS CHAR NO-UNDO.
    DEF BUFFER bf-itemfg FOR itemfg .
    DEF BUFFER bff-itemfg FOR itemfg .
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     fi_i-no
     fi_i-name
     fi_cust-no
     fi_est-no
     fi_style
     fi_procat
     fi_part-no
     tb_act 
     tb_in-act
     ll-first = NO.

    IF fi_est-no NE "" THEN fi_est-no = FILL(" ",8 - LENGTH(TRIM(fi_est-no))) + TRIM(fi_est-no).
    IF fi_i-no = "" AND fi_part-no <> "" AND fi_cust-no = "" AND fi_est-no = "" THEN do:
        FIND FIRST bff-itemfg WHERE bff-itemfg.company = cocode 
            AND bff-itemfg.part-no = fi_part-no NO-LOCK NO-ERROR.
        IF NOT AVAIL bff-itemfg THEN DO:
            FIND FIRST cust-part WHERE cust-part.company EQ cocode 
                AND cust-part.part-no EQ fi_part-no NO-LOCK NO-ERROR .
            IF AVAIL cust-part THEN
                ASSIGN fi_i-no = cust-part.i-no
                fi_part-no = "" .
            END.
    END.


    RUN dispatch ("open-query").

    GET FIRST r_table .
     IF NOT AVAIL itemfg THEN do:
         IF fi_cust-no <> "" THEN DO:
            v-cust-no  = fi_cust-no .
         END.
         ELSE DO:
             FIND FIRST bf-itemfg WHERE bf-itemfg.company = cocode
                 AND (bf-itemfg.i-no BEGINS fi_i-no OR fi_i-no = "")
                 AND (bf-itemfg.cust-no BEGINS fi_cust-no OR fi_cust-no = "")
                 AND (bf-itemfg.i-name BEGINS fi_i-name OR fi_i-name = "")
                 AND (bf-itemfg.est-no BEGINS fi_est-no OR fi_est-no = "")
                 AND (bf-itemfg.style BEGINS fi_style OR fi_style = "")
                 AND (bf-itemfg.procat BEGINS fi_procat OR fi_procat = "")
                 AND (bf-itemfg.part-no BEGINS fi_part-no OR fi_part-no = "") NO-LOCK NO-ERROR.
             IF AVAIL bf-itemfg THEN
                 v-cust-no = bf-itemfg.cust-no .
             ELSE v-cust-no = "" .
         END.

         FIND FIRST cust WHERE cust.company = cocode 
             AND cust.cust-no = v-cust-no NO-LOCK NO-ERROR.
         IF AVAIL cust AND ou-log AND LOOKUP(cust.cust-no,custcount) = 0 THEN
             MESSAGE "Customer is not on Users Customer List.  "  SKIP
              "Please add customer to Network Admin - Users Customer List."  VIEW-AS ALERT-BOX WARNING BUTTONS OK.
         ELSE
         MESSAGE "No Items Found, please update your Search Criteria."
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


&Scoped-define SELF-NAME fi_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cust-no B-table-Win
ON VALUE-CHANGED OF fi_cust-no IN FRAME F-Main
DO:
  IF LASTKEY <> 32 THEN {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_est-no B-table-Win
ON LEAVE OF fi_est-no IN FRAME F-Main
DO:
   IF LASTKEY <> 32 THEN 
    IF fi_est-no NE "" THEN
     {&self-name}:SCREEN-VALUE = FILL(" ",8 - LENGTH(TRIM({&self-name}:SCREEN-VALUE))) + TRIM({&self-name}:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_i-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-name B-table-Win
ON VALUE-CHANGED OF fi_i-name IN FRAME F-Main
DO:
  /*IF LASTKEY <> 32 THEN {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).*/
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-no B-table-Win
ON VALUE-CHANGED OF fi_i-no IN FRAME F-Main
DO:
  IF LASTKEY <> 32 THEN {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_part-no B-table-Win
ON VALUE-CHANGED OF fi_part-no IN FRAME F-Main
DO:
   IF LASTKEY <> 32 THEN {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE). 
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME r_table
&Scoped-define SELF-NAME r_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL r_table B-table-Win
ON DEFAULT-ACTION OF r_table IN FRAME F-Main
DO:
  {methods/run_link.i "container-source" "select-page" "(2)"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL r_table B-table-Win
ON ROW-DISPLAY OF r_table IN FRAME F-Main
DO:   
  find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "FGBrowse"
                        no-lock no-error.
  IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "Bronze/Maroon" THEN
            RUN set-row-bgcolor.
  find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "FGBrowseIA"
                        no-lock no-error.
  if not avail sys-ctrl then do transaction:
        create sys-ctrl.
        assign sys-ctrl.company = cocode
               sys-ctrl.name    = "FGBrowseIA"
               sys-ctrl.descrip = "Select colors for stocked boxes on FG browser"
               sys-ctrl.log-fld = NO
               sys-ctrl.char-fld = "TEAL"
               sys-ctrl.int-fld = 30.
  end.
  IF AVAIL sys-ctrl AND (sys-ctrl.char-fld BEGINS "TEAL" OR sys-ctrl.char-fld = "")
       AND sys-ctrl.log-fld EQ YES AND itemfg.stat = "I" THEN
            RUN set-teal-bgcolor.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL r_table B-table-Win
ON ROW-ENTRY OF r_table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL r_table B-table-Win
ON ROW-LEAVE OF r_table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL r_table B-table-Win
ON START-SEARCH OF r_table IN FRAME F-Main
DO:
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL r_table B-table-Win
ON VALUE-CHANGED OF r_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
 /*IF NOT AVAIL itemfg THEN
     MESSAGE "No Items Found, please update your Search Criteria. " VIEW-AS ALERT-BOX ERROR.*/
 IF AVAIL itemfg THEN do:
     FIND CURRENT itemfg NO-LOCK.

  {methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header"
    "(itemfg.rec_key,{methods/headers/itemfg.i})"}
  {methods/run_link.i "CONTAINER-SOURCE" "Notes-Message"
    "(CAN-FIND(FIRST notes WHERE notes.rec_key = itemfg.rec_key))"}
  {methods/run_link.i "CONTAINER-SOURCE" "MF-Message"
    "(CAN-FIND(FIRST mfvalues WHERE mfvalues.rec_key = itemfg.rec_key))"}     

  RUN paper-clip-image-proc(INPUT itemfg.rec_key).
  RUN spec-image-proc.

  /* disable/enable set parts tab */
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE, "container-source", OUTPUT char-hdl).
  RUN get-link-handle IN adm-broker-hdl(WIDGET-HANDLE(char-hdl), "page-source", OUTPUT char-hdl).
  IF itemfg.isaset THEN RUN enable-folder-page IN WIDGET-HANDLE(char-hdl) (INPUT 6) NO-ERROR.
  ELSE RUN disable-folder-page IN WIDGET-HANDLE(char-hdl) (INPUT 6) NO-ERROR.
 END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

RUN sys/ref/CustList.p (INPUT cocode,
                            INPUT 'IF1',
                            INPUT YES,
                            OUTPUT lActive).
{sys/inc/chblankcust.i ""IF1""}

/* {methods/browsers/setCellColumns.i} */
{methods/winReSize.i}
&SCOPED-DEFINE cellColumnDat b-itemfg.w 
 {methods/browsers/setCellColumns.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE browse-rowid B-table-Win 
PROCEDURE browse-rowid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-rowid AS ROWID NO-UNDO.

  IF AVAIL {&FIRST-TABLE-IN-QUERY-{&browse-name}} THEN
    op-rowid = ROWID({&FIRST-TABLE-IN-QUERY-{&browse-name}}).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changeRecord B-table-Win 
PROCEDURE changeRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipMove AS CHARACTER NO-UNDO.

  RUN dispatch ("get-" + ipMove).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Disable-Navigation B-table-Win 
PROCEDURE Disable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{methods/run_link.i "NAVIGATION-SOURCE" "dispatch" "('disable':U) NO-ERROR"}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Enable-Navigation B-table-Win 
PROCEDURE Enable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 {methods/run_link.i "NAVIGATION-SOURCE" "dispatch" "('enable':U) NO-ERROR"}
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
DEFINE VARIABLE lcItemFrom AS CHAR NO-UNDO.
DEFINE VARIABLE lcItemTo   AS CHAR NO-UNDO.
DEFINE VARIABLE lcItemNameFrom AS CHAR NO-UNDO.
DEFINE VARIABLE lcItemNameTo   AS CHAR NO-UNDO.
DEFINE VARIABLE lcCustPartFrom AS CHAR NO-UNDO.
DEFINE VARIABLE lcCustPartTo   AS CHAR NO-UNDO.
DEFINE VARIABLE lcCustFrom AS CHAR NO-UNDO.
DEFINE VARIABLE lcCustTo   AS CHAR NO-UNDO.
DEFINE VARIABLE lcEstFrom AS CHAR NO-UNDO.
DEFINE VARIABLE lcEstTo   AS CHAR NO-UNDO.
DEFINE VARIABLE lcStyleFrom AS CHAR NO-UNDO.
DEFINE VARIABLE lcStyleTo   AS CHAR NO-UNDO.
DEFINE VARIABLE lcCategoryFrom AS CHAR NO-UNDO.
DEFINE VARIABLE lcCategoryTo   AS CHAR NO-UNDO.

IF fi_i-no NE "" THEN
    ASSIGN
        lcItemFrom = fi_i-no
        lcItemTo = lcItemFrom.
IF fi_i-name NE "" THEN
    ASSIGN
        lcItemNameFrom = fi_i-name
        lcItemNameTo = lcItemNameFrom.
IF fi_part-no NE "" THEN
    ASSIGN
        lcCustPartFrom = fi_part-no
        lcCustPartTo = lcCustPartFrom.
IF fi_cust-no NE "" THEN
    ASSIGN
        lcCustFrom = fi_cust-no
        lcCustTo = lcCustFrom.
IF fi_est-no NE "" THEN
    ASSIGN
        lcEstFrom = fi_est-no
        lcEstTo = lcEstFrom.
IF fi_style NE "" THEN
    ASSIGN
        lcStyleFrom = fi_style
        lcStyleTo = lcStyleFrom.
IF fi_procat NE "" THEN
    ASSIGN
        lcCategoryFrom = fi_procat
        lcCategoryTo = lcCategoryFrom.

    RUN fg/rd-fgexp.w (lcItemFrom,
                       lcItemTo,
                       lcItemNameFrom,
                       lcItemNameTo,
                       lcCustPartFrom,
                       lcCustPartTo,
                       lcCustFrom,
                       lcCustTo,
                       lcEstFrom,
                       lcEstTo,
                       lcStyleFrom,
                       lcStyleTo,
                       lcCategoryFrom,
                       lcCategoryTo).

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
  ASSIGN itemfg.i-no:READ-ONLY IN BROWSE {&browse-name} = YES
         itemfg.i-name:READ-ONLY IN BROWSE {&browse-name} = YES
         itemfg.part-dscr1:READ-ONLY IN BROWSE {&browse-name} = YES
         itemfg.cust-no:READ-ONLY IN BROWSE {&browse-name} = YES
         itemfg.style:READ-ONLY IN BROWSE {&browse-name} = YES
         itemfg.procat:READ-ONLY IN BROWSE {&browse-name} = YES
         itemfg.i-code:READ-ONLY IN BROWSE {&browse-name} = YES
         itemfg.est-no:READ-ONLY IN BROWSE {&browse-name} = YES
         itemfg.cad-no:READ-ONLY IN BROWSE {&browse-name} = YES
         itemfg.spc-no:READ-ONLY IN BROWSE {&browse-name} = YES
         itemfg.stocked:READ-ONLY IN BROWSE {&browse-name} = YES
         itemfg.q-onh:READ-ONLY IN BROWSE {&browse-name} = YES                  
         FI_moveCol = "Sort".

  DISPLAY FI_moveCol WITH FRAME {&FRAME-NAME}.

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
  DEF VAR li AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  IF ll-show-all THEN DO:
      {fg/j-itminq.i}
  END.
  ELSE IF lv-show-prev OR lv-show-next THEN RUN show-prev-next.
  ELSE RUN first-query.

  IF AVAIL {&first-table-in-query-{&browse-name}} THEN DO:
      RUN dispatch ("display-fields").
      RUN dispatch ("row-changed").

      /*RUN dispatch ('get-last':U).*/

      GET LAST {&browse-name}.
      IF AVAIL itemfg THEN DO:
         IF ll-sort-asc THEN
            ASSIGN lv-last-rowid  = ROWID(itemfg)
                 lv-last-show-i-no = itemfg.i-no
                 lvLastRowID  = ROWID(itemfg).
         ELSE
            ASSIGN lv-frst-rowid = ROWID(itemfg)
                   lv-first-show-i-no = itemfg.i-no
                   lvFirstRowID  = ROWID(itemfg).

      END.
      /*RUN dispatch ('get-first':U).*/
      GET FIRST {&browse-name}.
      IF AVAIL itemfg THEN DO:
        IF ll-sort-asc THEN
          ASSIGN lv-frst-rowid  = ROWID(itemfg)
                 lv-first-show-i-no = itemfg.i-no
                 lvFirstRowID  = ROWID(itemfg).
        ELSE
          ASSIGN lv-last-rowid  = ROWID(itemfg)
                 lv-last-show-i-no = itemfg.i-no
                 lvLastRowID  = ROWID(itemfg).
      END.
  END.

  ASSIGN
      lv-show-prev = NO
      lv-show-next = NO.

  RUN set-rec_key.

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
      {&BROWSE-NAME}:COLUMN-MOVABLE = v-col-move
         {&BROWSE-NAME}:COLUMN-RESIZABLE = v-col-move
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
  IF AVAIL itemfg THEN
  IF ROWID(itemfg) EQ lvLastRowID THEN
  opNavType = 'L'.
  IF AVAIL itemfg THEN    
  IF ROWID(itemfg) EQ lvFirstRowID THEN
  opNavType = IF opNavType EQ 'L' THEN 'B' ELSE 'F'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE paper-clip-image-proc B-table-Win 
PROCEDURE paper-clip-image-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-rec_key AS CHAR NO-UNDO.

   DEF VAR v-i-no AS CHAR NO-UNDO.
   DEF VAR v-est-no AS cha NO-UNDO.
   DEF VAR v-att AS LOG NO-UNDO.
   DEF VAR char-hdl AS CHAR NO-UNDO.

   {sys/ref/attachlogic.i}
   IF v-i-no GT "" THEN
       v-i-no = TRIM(v-i-no, ",").

   IF v-i-no <> "" THEN
      v-att = CAN-FIND(FIRST asi.attach WHERE
              attach.company = cocode and
              ((LOOKUP(attach.rec_key,v-rec-key-list) gt 0 AND v-rec-key-list GT ""
                 AND (IF ATTACH.i-no GT "" THEN LOOKUP(attach.i-no, v-i-no) > 0 ELSE TRUE)) OR
              (LOOKUP(attach.i-no, v-i-no) > 0) AND ATTACH.i-no GT "")).
   ELSE
      v-att = CAN-FIND(FIRST asi.attach WHERE
              attach.company = cocode and
              (LOOKUP(attach.rec_key,v-rec-key-list) gt 0)).

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'attach-target':U, OUTPUT char-hdl).

   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN paper-clip-image IN WIDGET-HANDLE(char-hdl) (INPUT v-att).
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
                      and sys-ctrl.name    eq "FGBrowse"
                        no-lock no-error.
  if not avail sys-ctrl then do transaction:
        create sys-ctrl.
        assign sys-ctrl.company = cocode
               sys-ctrl.name    = "FGBrowse"
               sys-ctrl.descrip = "# of Records to be displayed in FG Item browser"
               sys-ctrl.log-fld = YES
               sys-ctrl.char-fld = "FG"
               sys-ctrl.int-fld = 30.
  end.

  {&for-eachblank} NO-LOCK:
    ASSIGN
    li = li + 1
    lv-i-no = itemfg.i-no.
    IF li GE sys-ctrl.int-fld THEN LEAVE.
  END.

  &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
        {&for-eachblank}                      ~
          AND itemfg.i-no <= lv-i-no NO-LOCK

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
                      and sys-ctrl.name    eq "FGBrowse"
                        no-lock no-error.
  if not avail sys-ctrl then do transaction:
        create sys-ctrl.
        assign sys-ctrl.company = cocode
               sys-ctrl.name    = "FGBrowse"
               sys-ctrl.descrip = "# of Records to be displayed in FG Item browser"
               sys-ctrl.log-fld = YES
               sys-ctrl.char-fld = "FG"
               sys-ctrl.int-fld = 30.
  end.

  IF fi_est-no NE "" THEN
     fi_est-no = FILL(" ",8 - LENGTH(TRIM(fi_est-no))) + TRIM(fi_est-no).

  IF fi_i-no NE "" AND fi_i-no BEGINS '*' THEN DO:  


     {&for-each2} NO-LOCK
             USE-INDEX i-no :
            ASSIGN
            li = li + 1
            lv-i-no = itemfg.i-no.
            IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
          OPEN QUERY {&browse-name}               ~
            {&for-each2}                          ~
          AND itemfg.i-no <= lv-i-no NO-LOCK

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.


  END.  

  ELSE IF fi_i-no NE "" AND NOT fi_i-no BEGINS '*' THEN DO:  

   {&for-each1} NO-LOCK
       USE-INDEX i-no :
      ASSIGN
      li = li + 1
      lv-i-no = itemfg.i-no.
      IF li GE sys-ctrl.int-fld THEN LEAVE.
   END.

   &SCOPED-DEFINE open-query                   ~
       OPEN QUERY {&browse-name}               ~
         {&for-each1}                          ~
           AND itemfg.i-no <= lv-i-no NO-LOCK

   IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                  ELSE {&open-query} {&sortby-phrase-desc}.

  END.
  ELSE IF fi_part-no NE "" AND fi_part-no BEGINS '*'  THEN DO: 

    {&for-each2} NO-LOCK
         USE-INDEX cust-part
         BY itemfg.i-no:
        ASSIGN
        li = li + 1
        lv-i-no = itemfg.i-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2}                          ~
             AND itemfg.i-no <= lv-i-no NO-LOCK


     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_part-no NE "" AND NOT fi_part-no BEGINS '*'  THEN DO: 
      {&for-each1} NO-LOCK
           USE-INDEX cust-part
           BY itemfg.i-no:
          ASSIGN
          li = li + 1
          lv-i-no = itemfg.i-no.
          IF li GE sys-ctrl.int-fld THEN LEAVE.
       END.

       &SCOPED-DEFINE open-query                   ~
           OPEN QUERY {&browse-name}               ~
             {&for-each1}                          ~
               AND itemfg.i-no <= lv-i-no NO-LOCK


       IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                      ELSE {&open-query} {&sortby-phrase-desc}.

  END.
  ELSE IF fi_cust-no NE "" AND fi_cust-no BEGINS '*' THEN DO:  

        {&for-each2} NO-LOCK
             USE-INDEX customer
             BY itemfg.i-no:
            ASSIGN
            li = li + 1
            lv-i-no = itemfg.i-no.
            IF li GE sys-ctrl.int-fld THEN LEAVE.
         END.

         &SCOPED-DEFINE open-query                   ~
             OPEN QUERY {&browse-name}               ~
               {&for-each2}                          ~
                 AND itemfg.i-no <= lv-i-no NO-LOCK

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_cust-no NE "" AND NOT fi_cust-no BEGINS '*' THEN DO:  
      {&for-each1} NO-LOCK
         USE-INDEX customer
         BY itemfg.i-no:
        ASSIGN
        li = li + 1
        lv-i-no = itemfg.i-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each1}                          ~
             AND itemfg.i-no <= lv-i-no NO-LOCK

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.

  END.
  ELSE IF fi_i-name NE "" AND fi_i-name BEGINS '*' THEN DO:  

    {&for-each2} NO-LOCK
        BY itemfg.i-no :
        ASSIGN
        li = li + 1
        lv-i-no = itemfg.i-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2}                          ~
             AND itemfg.i-no <= lv-i-no NO-LOCK

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_i-name NE "" AND NOT fi_i-name BEGINS '*' THEN DO:  

      {&for-each1} NO-LOCK
          BY itemfg.i-no :
          ASSIGN
          li = li + 1
          lv-i-no = itemfg.i-no.
          IF li GE sys-ctrl.int-fld THEN LEAVE.
       END.

       &SCOPED-DEFINE open-query                   ~
           OPEN QUERY {&browse-name}               ~
             {&for-each1}                          ~
               AND itemfg.i-no <= lv-i-no NO-LOCK

       IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                      ELSE {&open-query} {&sortby-phrase-desc}.

  END.
  ELSE IF fi_procat NE "" AND fi_procat BEGINS '*' THEN DO:  

    {&for-each2} NO-LOCK
         USE-INDEX procat
         BY itemfg.i-no :
        ASSIGN
        li = li + 1
        lv-i-no = itemfg.i-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2}                          ~
             AND itemfg.i-no <= lv-i-no NO-LOCK


     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_procat NE "" AND NOT fi_procat BEGINS '*' THEN DO:  
     {&for-each1} NO-LOCK
         USE-INDEX procat
         BY itemfg.i-no :
        ASSIGN
        li = li + 1
        lv-i-no = itemfg.i-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each1}                          ~
             AND itemfg.i-no <= lv-i-no NO-LOCK

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.

  END.
  ELSE IF fi_style NE "" AND fi_style BEGINS '*' THEN DO:  

    {&for-each2} NO-LOCK 
        BY itemfg.i-no:
        ASSIGN
        li = li + 1
        lv-i-no = itemfg.i-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2}                          ~
             AND itemfg.i-no <= lv-i-no NO-LOCK  

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_style NE "" AND NOT fi_style BEGINS '*' THEN DO:  
      {&for-each1} NO-LOCK 
          BY itemfg.i-no:
          ASSIGN
          li = li + 1
          lv-i-no = itemfg.i-no.
          IF li GE sys-ctrl.int-fld THEN LEAVE.
       END.

       &SCOPED-DEFINE open-query                   ~
           OPEN QUERY {&browse-name}               ~
             {&for-each1}                          ~
               AND itemfg.i-no <= lv-i-no NO-LOCK


       IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                      ELSE {&open-query} {&sortby-phrase-desc}.

  END.
  ELSE IF fi_est-no NE "" AND fi_est-no BEGINS '*' THEN DO:

         {&for-each2} NO-LOCK
             USE-INDEX estimate
             BY itemfg.i-no :
            ASSIGN
            li = li + 1
            lv-i-no = itemfg.i-no.
            IF li GE sys-ctrl.int-fld THEN LEAVE.
         END.

         &SCOPED-DEFINE open-query                   ~
             OPEN QUERY {&browse-name}               ~
               {&for-each2}                          ~
                 AND itemfg.i-no <= lv-i-no NO-LOCK  

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_est-no NE "" AND NOT fi_est-no BEGINS '*' THEN DO:

          {&for-each1} NO-LOCK
              USE-INDEX estimate
              BY itemfg.i-no :
             ASSIGN
             li = li + 1
             lv-i-no = itemfg.i-no.
             IF li GE sys-ctrl.int-fld THEN LEAVE.
          END.

          &SCOPED-DEFINE open-query                   ~
              OPEN QUERY {&browse-name}               ~
                {&for-each1}                          ~
                  AND itemfg.i-no <= lv-i-no NO-LOCK



      IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                     ELSE {&open-query} {&sortby-phrase-desc}.

  END.
  ELSE DO:  
    {&for-eachblank} NO-LOCK
       BREAK BY itemfg.i-no :
       IF FIRST-OF(itemfg.i-no) THEN li = li + 1.
       lv-i-no = itemfg.i-no.
       IF li GE sys-ctrl.int-fld THEN LEAVE.
    END.

    &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
           {&for-eachblank}                     ~
             AND itemfg.i-no <= lv-i-no NO-LOCK

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

{fg/j-itminq.i}

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
  {src/adm/template/snd-list.i "itemfg"}

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
     fi_i-no:SCREEN-VALUE    = ""
     fi_i-name:SCREEN-VALUE = ""
     fi_est-no:SCREEN-VALUE  = ""
     fi_cust-no = ""
     fi_i-no  = ""
     fi_i-name = ""
     fi_est-no = "" 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-rec_key B-table-Win 
PROCEDURE set-rec_key :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-itemfg FOR itemfg.

  IF AVAIL itemfg THEN DO:
    FIND b-itemfg NO-LOCK WHERE ROWID(b-itemfg) EQ ROWID(itemfg) NO-ERROR.
    {methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header"
       "(b-itemfg.rec_key,{methods/headers/itemfg.i})"}
    {methods/run_link.i "CONTAINER-SOURCE" "Notes-Message"
       "(CAN-FIND(FIRST notes WHERE notes.rec_key = itemfg.rec_key))"}
    {methods/run_link.i "CONTAINER-SOURCE" "MF-Message"
       "(CAN-FIND(FIRST mfvalues WHERE mfvalues.rec_key = itemfg.rec_key))"}
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-row-bgcolor B-table-Win 
PROCEDURE set-row-bgcolor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF AVAIL itemfg THEN DO:
    IF itemfg.stocked =  YES AND itemfg.q-onh LE 0  THEN do:

        ASSIGN 
            itemfg.i-no:FGCOLOR IN BROWSE {&BROWSE-NAME}      = 15
            itemfg.i-name:FGCOLOR IN BROWSE {&BROWSE-NAME}    = 15
            itemfg.part-dscr1:FGCOLOR IN BROWSE {&BROWSE-NAME} = 15
            itemfg.cust-no:FGCOLOR IN BROWSE {&BROWSE-NAME}   = 15
            itemfg.style:FGCOLOR IN BROWSE {&BROWSE-NAME}     = 15
            itemfg.procat:FGCOLOR IN BROWSE {&BROWSE-NAME}    = 15
            itemfg.i-code:FGCOLOR IN BROWSE {&BROWSE-NAME}    = 15
            itemfg.est-no:FGCOLOR IN BROWSE {&BROWSE-NAME}    = 15
            itemfg.cad-no:FGCOLOR IN BROWSE {&BROWSE-NAME}    = 15
            itemfg.spc-no:FGCOLOR IN BROWSE {&BROWSE-NAME}    = 15
            itemfg.stocked:FGCOLOR IN BROWSE {&BROWSE-NAME}   = 15
            itemfg.q-onh:FGCOLOR IN BROWSE {&BROWSE-NAME}     = 15
            itemfg.i-no:BGCOLOR IN BROWSE {&BROWSE-NAME}      = 4
            itemfg.i-name:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 4
            itemfg.part-dscr1:BGCOLOR IN BROWSE {&BROWSE-NAME} = 4
            itemfg.cust-no:BGCOLOR IN BROWSE {&BROWSE-NAME}   = 4
            itemfg.style:BGCOLOR IN BROWSE {&BROWSE-NAME}     = 4
            itemfg.procat:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 4
            itemfg.i-code:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 4
            itemfg.est-no:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 4
            itemfg.cad-no:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 4
            itemfg.spc-no:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 4
            itemfg.stocked:BGCOLOR IN BROWSE {&BROWSE-NAME}   = 4
            itemfg.q-onh:BGCOLOR IN BROWSE {&BROWSE-NAME}     = 4 .
        END.
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-teal-bgcolor B-table-Win 
PROCEDURE set-teal-bgcolor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF AVAIL itemfg THEN DO:
    ASSIGN 
        itemfg.i-no:BGCOLOR IN BROWSE {&BROWSE-NAME}      = 3
              .
     /*   END.*/
 END.

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

  IF fi_est-no NE "" THEN fi_est-no = FILL(" ",8 - LENGTH(TRIM(fi_est-no))) + TRIM(fi_est-no).

  find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "FGBrowse"
                        no-lock no-error.
  if not avail sys-ctrl then do transaction:
        create sys-ctrl.
        assign sys-ctrl.company = cocode
               sys-ctrl.name    = "FGBrowse"
               sys-ctrl.descrip = "# of Records to be displayed in FG Item browser"
               sys-ctrl.log-fld = YES
               sys-ctrl.char-fld = "FG"
               sys-ctrl.int-fld = 30.
  end.

  IF lv-show-prev THEN DO:

     IF fi_i-no EQ "" AND fi_part-no EQ "" AND fi_cust-no EQ "" AND
        fi_i-name EQ "" AND fi_est-no EQ "" AND fi_style EQ "" AND
        fi_procat EQ "" THEN
     DO:
        {&for-eachblank} 
         and itemfg.i-no <= lv-first-show-i-no NO-LOCK BY itemfg.i-no DESC :
         ASSIGN
         li = li + 1
         lv-i-no = itemfg.i-no.
         IF li GE sys-ctrl.int-fld THEN LEAVE.       
        END.

        &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
         {&for-eachblank}                          ~
             AND itemfg.i-no gE lv-i-no          ~
             AND itemfg.i-no lE lv-first-show-i-no NO-LOCK

     END.
     ELSE
     DO:
        {&for-each1} 
           and itemfg.i-no <= lv-first-show-i-no NO-LOCK BY itemfg.i-no DESC :
           ASSIGN
           li = li + 1
           lv-i-no = itemfg.i-no.
          IF li GE sys-ctrl.int-fld THEN LEAVE.       
        END.

        &SCOPED-DEFINE open-query                   ~
            OPEN QUERY {&browse-name}               ~
            {&for-each1}                          ~
                AND itemfg.i-no gE lv-i-no          ~
                AND itemfg.i-no lE lv-first-show-i-no NO-LOCK
      END.

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
     ELSE {&open-query} {&sortby-phrase-desc}.

  END.  /* lv-show-prev */
  ELSE IF lv-show-next THEN DO:
      IF fi_i-no EQ "" AND fi_part-no EQ "" AND fi_cust-no EQ "" AND
        fi_i-name EQ "" AND fi_est-no EQ "" AND fi_style EQ "" AND
        fi_procat EQ "" THEN
      DO:
         {&for-eachblank} 
         and itemfg.i-no >= lv-last-show-i-no  NO-LOCK:
         ASSIGN
            li = li + 1
            lv-i-no = itemfg.i-no.
         IF li GE sys-ctrl.int-fld THEN LEAVE.
       END.

       &SCOPED-DEFINE open-query                 ~
         OPEN QUERY {&browse-name}               ~
         {&for-eachblank}                        ~
             AND itemfg.i-no LE lv-i-no          ~
             AND itemfg.i-no GE lv-last-show-i-no NO-LOCK

      END.
      ELSE
      DO:

      {&for-each1} 
         and itemfg.i-no >= lv-last-show-i-no  NO-LOCK:
         ASSIGN
            li = li + 1
            lv-i-no = itemfg.i-no.
         IF li GE sys-ctrl.int-fld THEN LEAVE.
       END.

       &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
         {&for-each1}                          ~
             AND itemfg.i-no LE lv-i-no          ~
             AND itemfg.i-no GE lv-last-show-i-no NO-LOCK
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spec-image-proc B-table-Win 
PROCEDURE spec-image-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-spec AS LOG NO-UNDO.
   DEF VAR char-hdl AS CHAR NO-UNDO.

   v-spec = CAN-FIND(FIRST notes WHERE
            notes.rec_key = itemfg.rec_key AND
            notes.note_type = "S").

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'spec-target':U, OUTPUT char-hdl).

   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN spec-book-image IN WIDGET-HANDLE(char-hdl) (INPUT v-spec).
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
      APPLY "VALUE-CHANGED" TO BROWSE {&browse-name}.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

