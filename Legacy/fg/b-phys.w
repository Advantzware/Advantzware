&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  fg\b-phys.w

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

&SCOPED-DEFINE yellowColumnsName b-phys
&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}

def var char-val as cha no-undo.
def var ext-cost as decimal no-undo.
def var lv-recid as recid no-undo.
def var ls-prev-po as cha no-undo.
def var hd-post as widget-handle no-undo.
def var hd-post-child as widget-handle no-undo.
def var ll-help-run as log no-undo.  /* set on browse help, reset row-entry */
DEF VAR lv-job-no AS CHAR NO-UNDO.
DEF VAR lv-job-no2 AS CHAR NO-UNDO.
DEF VAR lv-prev-job2 AS cha NO-UNDO.
DEF VAR lv-new-job-ran AS LOG NO-UNDO.
DEF VAR lv-date LIKE fg-rctd.rct-date NO-UNDO.

DEF BUFFER bf-tmp FOR fg-rctd.  /* for tag validation */
DEF BUFFER xfg-rdtlh FOR fg-rdtlh. /* for tag validation */

&SCOPED-DEFINE item-key-phrase TRUE
DEF VAR ll-crt-transfer AS LOG NO-UNDO.
DEF VAR lv-org-loc AS cha NO-UNDO.
DEF VAR lv-org-loc-bin AS cha NO-UNDO.
DEF VAR lv-prv-i-no LIKE fg-rctd.i-no NO-UNDO.
DEFINE VARIABLE lCheckCount AS LOGICAL NO-UNDO .
ASSIGN
 cocode = g_company
 locode = g_loc.

DO:
   {sys/inc/fgrecpt.i}
END.

DEF NEW SHARED TEMP-TABLE tt-selected no-undo
        FIELD tt-rowid AS ROWID 
        FIELD tt-import-zero AS LOG .

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
&Scoped-define INTERNAL-TABLES fg-rctd

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table fg-rctd.r-no fg-rctd.rct-date ~
fg-rctd.i-no fg-rctd.i-name fg-rctd.job-no fg-rctd.job-no2 fg-rctd.loc ~
fg-rctd.loc-bin fg-rctd.tag fg-rctd.cust-no fg-rctd.cases fg-rctd.qty-case ~
fg-rctd.cases-unit fg-rctd.partial fg-rctd.t-qty fg-rctd.created-by ~
fg-rctd.updated-by fg-rctd.po-no 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table fg-rctd.rct-date ~
fg-rctd.i-no fg-rctd.i-name fg-rctd.job-no fg-rctd.job-no2 fg-rctd.loc ~
fg-rctd.loc-bin fg-rctd.tag fg-rctd.cust-no fg-rctd.cases fg-rctd.qty-case ~
fg-rctd.cases-unit fg-rctd.partial 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table fg-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table fg-rctd
&Scoped-define QUERY-STRING-Browser-Table FOR EACH fg-rctd WHERE ~{&KEY-PHRASE} ~
      AND fg-rctd.company = g_company and ~
fg-rctd.rita-code = "C" NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH fg-rctd WHERE ~{&KEY-PHRASE} ~
      AND fg-rctd.company = g_company and ~
fg-rctd.rita-code = "C" NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table fg-rctd
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table fg-rctd


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-4 RECT-5 Browser-Table browse-order ~
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


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     LABEL "Auto Find" 
     VIEW-AS FILL-IN 
     SIZE 117 BY .95 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 93 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 2.62.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 15.48.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      fg-rctd SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      fg-rctd.r-no COLUMN-LABEL "Seq#" FORMAT ">>>>>>>>":U WIDTH 12
            LABEL-BGCOLOR 14
      fg-rctd.rct-date COLUMN-LABEL "Count Date" FORMAT "99/99/9999":U
            WIDTH 14 LABEL-BGCOLOR 14
      fg-rctd.i-no COLUMN-LABEL "Item" FORMAT "x(15)":U WIDTH 23
            LABEL-BGCOLOR 14
      fg-rctd.i-name COLUMN-LABEL "Name/Desc" FORMAT "x(30)":U
            LABEL-BGCOLOR 14
      fg-rctd.job-no COLUMN-LABEL "Job#" FORMAT "x(6)":U WIDTH 9
            LABEL-BGCOLOR 14
      fg-rctd.job-no2 FORMAT "99":U WIDTH 4
      fg-rctd.loc COLUMN-LABEL "Whse" FORMAT "x(5)":U LABEL-BGCOLOR 14
      fg-rctd.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U LABEL-BGCOLOR 14
      fg-rctd.tag COLUMN-LABEL "Tag#" FORMAT "x(20)":U LABEL-BGCOLOR 14
      fg-rctd.cust-no COLUMN-LABEL "Customer#" FORMAT "x(8)":U
            WIDTH 12 LABEL-BGCOLOR 14
      fg-rctd.cases COLUMN-LABEL "Units" FORMAT "->>>,>>9":U WIDTH 9
            LABEL-BGCOLOR 14
      fg-rctd.qty-case COLUMN-LABEL "Unit Count" FORMAT ">>>,>>9":U
            WIDTH 14 LABEL-BGCOLOR 14
      fg-rctd.cases-unit COLUMN-LABEL "Units/!Skid" FORMAT ">>>9":U
            WIDTH 17 LABEL-BGCOLOR 14
      fg-rctd.partial COLUMN-LABEL "Partial" FORMAT "->>>,>>9":U
            WIDTH 10 LABEL-BGCOLOR 14
      fg-rctd.t-qty COLUMN-LABEL "Quantity" FORMAT "->>,>>>,>>>,>>9":U
            WIDTH 25 LABEL-BGCOLOR 14
      fg-rctd.created-by COLUMN-LABEL "Created By" FORMAT "x(8)":U
            WIDTH 15
      fg-rctd.updated-by COLUMN-LABEL "Last Updated By" FORMAT "x(8)":U
            WIDTH 15
      fg-rctd.po-no FORMAT "x(9)":U
  ENABLE
      fg-rctd.rct-date
      fg-rctd.i-no
      fg-rctd.i-name
      fg-rctd.job-no
      fg-rctd.job-no2
      fg-rctd.loc
      fg-rctd.loc-bin
      fg-rctd.tag
      fg-rctd.cust-no
      fg-rctd.cases
      fg-rctd.qty-case
      fg-rctd.cases-unit
      fg-rctd.partial
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 143 BY 15
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1.24 COL 2 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 16.48 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     fi_sortby AT ROW 16.48 COL 98 COLON-ALIGNED NO-LABEL
     auto_find AT ROW 17.67 COL 11 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 17.67 COL 131 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 16.48 COL 2
     RECT-4 AT ROW 16.24 COL 1
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
         HEIGHT             = 18.43
         WIDTH              = 154.4.
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
/* BROWSE-TAB Browser-Table RECT-5 F-Main */
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
     _TblList          = "asi.fg-rctd"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", OUTER"
     _Where[1]         = "fg-rctd.company = g_company and
fg-rctd.rita-code = ""C"""
     _FldNameList[1]   > asi.fg-rctd.r-no
"fg-rctd.r-no" "Seq#" ">>>>>>>>" "integer" ? ? ? 14 ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.fg-rctd.rct-date
"fg-rctd.rct-date" "Count Date" ? "date" ? ? ? 14 ? ? yes ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > asi.fg-rctd.i-no
"fg-rctd.i-no" "Item" "x(15)" "character" ? ? ? 14 ? ? yes ? no no "23" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > asi.fg-rctd.i-name
"fg-rctd.i-name" "Name/Desc" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > asi.fg-rctd.job-no
"fg-rctd.job-no" "Job#" ? "character" ? ? ? 14 ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > asi.fg-rctd.job-no2
"fg-rctd.job-no2" ? ? "integer" ? ? ? ? ? ? yes ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > asi.fg-rctd.loc
"fg-rctd.loc" "Whse" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > asi.fg-rctd.loc-bin
"fg-rctd.loc-bin" "Bin" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > asi.fg-rctd.tag
"fg-rctd.tag" "Tag#" "x(20)" "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > asi.fg-rctd.cust-no
"fg-rctd.cust-no" "Customer#" ? "character" ? ? ? 14 ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > asi.fg-rctd.cases
"fg-rctd.cases" "Units" "->>>,>>9" "integer" ? ? ? 14 ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > asi.fg-rctd.qty-case
"fg-rctd.qty-case" "Unit Count" ? "integer" ? ? ? 14 ? ? yes ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > asi.fg-rctd.cases-unit
"fg-rctd.cases-unit" "Units/!Skid" ">>>9" "integer" ? ? ? 14 ? ? yes ? no no "17" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > asi.fg-rctd.partial
"fg-rctd.partial" "Partial" "->>>,>>9" "integer" ? ? ? 14 ? ? yes ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > asi.fg-rctd.t-qty
"fg-rctd.t-qty" "Quantity" "->>,>>>,>>>,>>9" "decimal" ? ? ? 14 ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > asi.fg-rctd.created-by
"fg-rctd.created-by" "Created By" ? "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > asi.fg-rctd.updated-by
"fg-rctd.updated-by" "Last Updated By" ? "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   = asi.fg-rctd.po-no
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
ON HELP OF Browser-Table IN FRAME F-Main
DO:
   def var ll-tag# as log no-undo.
   DEF VAR rec-val AS RECID NO-UNDO.
   DEF VAR char-val AS cha NO-UNDO.
   DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.


  IF NOT AVAIL fg-rctd THEN FIND fg-rctd WHERE RECID(fg-rctd) EQ lv-recid NO-LOCK NO-ERROR. 
 
  ASSIGN
   ll-help-run = YES
   lw-focus    = FOCUS.

  CASE lw-focus:NAME:
    WHEN "i-no" THEN DO:
      RUN windows/l-itemfg.w (cocode, "", lw-focus:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE "" AND lw-focus:SCREEN-VALUE NE ENTRY(1,char-val) THEN DO:
        lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
        /*APPLY "value-changed" TO lw-focus.*/
      END.         
    END.

    when "job-no" or when "job-no2" then do:
             run windows/l-jobfg.w (cocode, fg-rctd.i-no:SCREEN-VALUE in browse {&browse-name}, fg-rctd.job-no:SCREEN-VALUE in browse {&browse-name}, output char-val, OUTPUT rec-val).
             if char-val <> "" THEN DO:
                IF fg-rctd.i-no:SCREEN-VALUE = "" THEN DO:
                   fg-rctd.i-no:SCREEN-VALUE = ENTRY(7,char-val).
                   find first itemfg {sys/look/itemfgrlW.i}
                        and itemfg.i-no EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                        no-lock no-error.
                   IF AVAIL itemfg THEN RUN get-def-values.
                END.
                assign fg-rctd.job-no:screen-value in browse {&browse-name} = entry(1,char-val)
                       fg-rctd.job-no2:screen-value in browse {&browse-name} = entry(2,char-val)
                       fg-rctd.loc:SCREEN-VALUE = ENTRY(3,char-val)
                       fg-rctd.loc-bin:SCREEN-VALUE = ENTRY(4,char-val)
                       fg-rctd.cases:SCREEN-VALUE = ENTRY(10,char-val)
                       fg-rctd.qty-case:SCREEN-VALUE = ENTRY(5,char-val)
                       fg-rctd.cases-unit:SCREEN-VALUE = ENTRY(6,char-val)
                       fg-rctd.tag:SCREEN-VALUE = ENTRY(8,char-val)
                       fg-rctd.partial:SCREEN-VALUE = ENTRY(9,char-val)
                       .  
                RUN calc-t-qty.
             END.                
    end.

    when "loc" then do:
             run windows/l-loc.w (cocode,lw-focus:screen-value, output char-val).
             if char-val <> "" then do :
                assign lw-focus:SCREEN-VALUE = entry(1,char-val)
                       .

             end.  
    end.
    when "loc-bin" then do:
             run windows/l-fgbin.w (cocode,fg-rctd.loc:screen-value in browse {&browse-name}, fg-rctd.loc-bin:screen-value,output char-val).
             if char-val <> "" then do :
                assign lw-focus:screen-value  = entry(1,char-val)
                       /*fg-rctd.loc:screen-value = entry(2,char-val)
                        fg-rctd.tag:screen-value = entry(4,char-val)*/
                       .

             end.
    end.

    WHEN "tag" THEN DO:
      RUN fgbin-help.
    END.

    WHEN "loc-bin" THEN DO:
      RUN windows/l-fgbin.w (cocode, lw-focus:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE "" THEN lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
    END.

  END CASE.

  RETURN NO-APPLY.

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
ON ROW-DISPLAY OF Browser-Table IN FRAME F-Main
DO:  /* display calculated field */
  /* def var ii as int.
   ii = if avail rm-rctd then integer(rm-rctd.po-no) else 0.
   
   if avail rm-rctd then    run get-matrix (true).
*/
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
  /* {src/adm/template/brsleave.i}*/
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


&Scoped-define SELF-NAME fg-rctd.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.i-no IN BROWSE Browser-Table /* Item */
DO:
  lv-prv-i-no = {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.i-no IN BROWSE Browser-Table /* Item */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    IF {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name} NE lv-prv-i-no THEN DO:
      FIND itemfg
          {sys/look/itemfgrlW.i}
            AND itemfg.i-no EQ {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}
          NO-LOCK NO-ERROR.
      IF AVAIL itemfg THEN RUN get-def-values.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF fg-rctd.i-no IN BROWSE Browser-Table /* Item */
DO:
  /*DEF VAR li AS INT NO-UNDO.


  FIND itemfg
      {sys/look/itemfgrlW.i}
        AND itemfg.i-no EQ {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}
      NO-LOCK NO-ERROR.
  IF AVAIL itemfg THEN DO:
    RUN get-def-values.
    DO li = 1 TO LENGTH({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}) + 1:
      APPLY "cursor-right" TO {&self-name} IN BROWSE {&browse-name}.
    END.
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.job-no IN BROWSE Browser-Table /* Job# */
DO:
  lv-job-no = fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.job-no IN BROWSE Browser-Table /* Job# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.job-no2 IN BROWSE Browser-Table
DO:
  lv-job-no = fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}.
  lv-prev-job2 = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.job-no2 IN BROWSE Browser-Table
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-no2 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    IF lv-prev-job2 NE fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} THEN DO:
       RUN new-job-no.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.loc IN BROWSE Browser-Table /* Whse */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-loc NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc-bin Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.loc-bin IN BROWSE Browser-Table /* Bin */
DO:
  /*IF LASTKEY NE -1 THEN DO:
    RUN valid-loc-bin NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.tag IN BROWSE Browser-Table /* Tag# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    FIND FIRST loadtag WHERE loadtag.company = g_company
                    AND loadtag.ITEM-type = NO
                    AND loadtag.tag-no = fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF AVAIL loadtag AND loadtag.po-no GT 0 THEN
        fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(loadtag.po-no).
    
    RUN validate-count .

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF fg-rctd.tag IN BROWSE Browser-Table /* Tag# */
DO:
  IF LASTKEY NE -1 THEN DO:
   lCheckCount = NO .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.cust-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.cust-no IN BROWSE Browser-Table /* Customer# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-cust-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.cases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.cases Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.cases IN BROWSE Browser-Table /* Units */
DO:
  IF LASTKEY <> -1 THEN RUN calc-t-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.qty-case
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.qty-case Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.qty-case IN BROWSE Browser-Table /* Unit Count */
DO:
  IF LASTKEY <> -1 THEN RUN calc-t-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.cases-unit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.cases-unit Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.cases-unit IN BROWSE Browser-Table /* Units/!Skid */
DO:
  IF LASTKEY <> -1 THEN RUN calc-t-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.partial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.partial Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.partial IN BROWSE Browser-Table /* Partial */
DO:
  IF LASTKEY <> -1 THEN RUN calc-t-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.t-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.t-qty Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.t-qty IN BROWSE Browser-Table /* Quantity */
DO:
  APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-t-qty B-table-Win 
PROCEDURE calc-t-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name} =
        STRING(INT(fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}) *
               INT(fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name}) +
               INT(fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name}),
               "->>>,>>>,>>9.99").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancel-item B-table-Win 
PROCEDURE cancel-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  when exiting will cancel blank record so it will not create it     
------------------------------------------------------------------------------*/
    IF NOT AVAIL fg-rctd THEN 
        FIND fg-rctd NO-LOCK WHERE RECID(fg-rctd) EQ lv-recid  NO-ERROR. 
  IF AVAIL fg-rctd AND fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} = "" THEN
    RUN dispatch IN THIS-PROCEDURE (INPUT 'cancel-record':U).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crt-pcount B-table-Win 
PROCEDURE crt-pcount :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rno LIKE fg-rctd.r-no NO-UNDO.
  DEF BUFFER b-fg-rctd FOR fg-rctd.
  DEF VAR lv-rctd-rowid AS ROWID NO-UNDO.
  DEF VAR lv-first AS log NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  lv-rno = 0.
  FIND LAST b-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
  IF AVAIL b-fg-rctd AND b-fg-rctd.r-no GT lv-rno THEN lv-rno = b-fg-rctd.r-no.

  FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
  IF AVAIL fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN lv-rno = fg-rcpth.r-no.

  DO WHILE TRUE:
    lv-rno = lv-rno + 1.
    FIND FIRST fg-rcpth WHERE fg-rcpth.r-no EQ lv-rno USE-INDEX r-no NO-LOCK NO-ERROR.
    IF AVAIL fg-rcpth THEN NEXT.
    FIND FIRST b-fg-rctd WHERE b-fg-rctd.r-no EQ lv-rno USE-INDEX fg-rctd NO-LOCK NO-ERROR.
    IF AVAIL b-fg-rctd THEN NEXT.
    LEAVE.
  END.

  lv-first = YES.
  FOR EACH tt-selected,
      FIRST fg-bin WHERE rowid(fg-bin) = tt-selected.tt-rowid NO-LOCK:
      FIND FIRST itemfg WHERE itemfg.company = fg-bin.company
                          AND itemfg.i-no = fg-bin.i-no NO-LOCK NO-ERROR.
      CREATE b-fg-rctd.
      ASSIGN b-fg-rctd.r-no = lv-rno
             b-fg-rctd.loc = fg-bin.loc
             b-fg-rctd.loc-bin = fg-bin.loc-bin
             b-fg-rctd.company = cocode
             b-fg-rctd.r-no    = lv-rno
             b-fg-rctd.rita-code = "C"
             b-fg-rctd.s-num  = 0
             b-fg-rctd.rct-date  = lv-date
             b-fg-rctd.trans-time = TIME
             b-fg-rctd.qty-case = (fg-bin.case-count)
             b-fg-rctd.cases-unit = (fg-bin.cases-unit)
             b-fg-rctd.i-no = fg-bin.i-no
             b-fg-rctd.i-name = itemfg.i-name
             b-fg-rctd.job-no = fg-bin.job-no
             b-fg-rctd.job-no2 = fg-bin.job-no2
             b-fg-rctd.tag = fg-bin.tag
             b-fg-rctd.cust-no = fg-bin.cust-no.

       IF tt-selected.tt-import-zero THEN
          ASSIGN b-fg-rctd.std-cost = fg-bin.qty /
                                     (IF fg-bin.pur-uom EQ "M" THEN 1000 ELSE 1) *
                                     fg-bin.std-tot-cost
                 b-fg-rctd.cases = 0
                 /*b-fg-rctd.qty-case = 0
                 b-fg-rctd.cases-unit = 0*/
                 b-fg-rctd.partial = 0
                 b-fg-rctd.t-qty = 0.
       ELSE
       DO:
          ASSIGN b-fg-rctd.cases = (TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0))
                 b-fg-rctd.partial = (fg-bin.partial-count)
                 b-fg-rctd.t-qty = (fg-bin.qty)
                 b-fg-rctd.ext-cost = b-fg-rctd.t-qty /
                                     (IF fg-bin.pur-uom EQ "M" THEN 1000 ELSE 1) *
                                     fg-bin.std-tot-cost
                 b-fg-rctd.cost     = b-fg-rctd.ext-cost / b-fg-rctd.t-qty
                 b-fg-rctd.cost-uom = fg-bin.pur-uom.

          IF b-fg-rctd.ext-cost EQ ? THEN b-fg-rctd.ext-cost = 0.
          IF b-fg-rctd.cost     EQ ? THEN b-fg-rctd.cost = 0.
       END.

      lv-rno = lv-rno + 1.
  END.  /* for each fg-bin*/
  lv-rctd-rowid = ROWID(fg-rctd).
  RUN dispatch ('open-query').
  RUN repo-query (lv-rctd-rowid).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crt-transfer B-table-Win 
PROCEDURE crt-transfer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF VAR lv-rno LIKE fg-rctd.r-no NO-UNDO.
  DEF BUFFER b-fg-rctd FOR fg-rctd.
  DEF VAR lv-rctd-rowid AS ROWID NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  lv-rno = 0.
  FIND LAST b-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
  IF AVAIL b-fg-rctd AND b-fg-rctd.r-no GT lv-rno THEN lv-rno = b-fg-rctd.r-no.

  FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
  IF AVAIL fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN lv-rno = fg-rcpth.r-no.

  DO WHILE TRUE:
    lv-rno = lv-rno + 1.
    FIND FIRST fg-rcpth WHERE fg-rcpth.r-no EQ lv-rno USE-INDEX r-no NO-LOCK NO-ERROR.
    IF AVAIL fg-rcpth THEN NEXT.
    FIND FIRST b-fg-rctd WHERE b-fg-rctd.r-no EQ lv-rno USE-INDEX fg-rctd NO-LOCK NO-ERROR.
    IF AVAIL b-fg-rctd THEN NEXT.
    LEAVE.
  END.

  IF TRIM(fg-rctd.tag) NE "" THEN
  FOR EACH b-fg-rctd WHERE recid(b-fg-rctd) <> RECID(fg-rctd) 
                       AND b-fg-rctd.i-no = fg-rctd.i-no
                       AND b-fg-rctd.tag = fg-rctd.tag:
      DELETE b-fg-rctd.
  END.

  FOR EACH fg-bin WHERE fg-bin.company EQ cocode 
                      AND fg-bin.i-no    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND fg-bin.job-no = fg-rctd.job-no
                      AND fg-bin.job-no2 = fg-rctd.job-no2 
                      AND fg-bin.tag     EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
                      /*AND fg-bin.qty > 0*/  NO-LOCK:

     IF fg-bin.loc NE fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
        OR  fg-bin.loc-bin NE fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
        OR  fg-bin.cust-no NE fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}
     THEN DO:
         CREATE b-fg-rctd.
         BUFFER-COPY fg-rctd EXCEPT fg-rctd.r-no TO b-fg-rctd.
         ASSIGN b-fg-rctd.r-no = lv-rno
                b-fg-rctd.loc = fg-bin.loc
                b-fg-rctd.loc-bin = fg-bin.loc-bin
                b-fg-rctd.cust-no = fg-bin.cust-no
                b-fg-rctd.cases = 0
                b-fg-rctd.qty-case = 0
                b-fg-rctd.cases-unit = 0
                b-fg-rctd.partial = 0
                b-fg-rctd.t-qty = 0.
         lv-rno = lv-rno + 1.
     END.
  END.  /* for each fg-bin*/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-job-hdr B-table-Win 
PROCEDURE display-job-hdr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-job-hdr-recid AS RECID.

  FIND FIRST job-hdr WHERE recid(job-hdr) = ip-job-hdr-recid NO-LOCK NO-ERROR.
  IF NOT AVAIL job-hdr THEN RETURN.

  FIND FIRST fg-bin WHERE fg-bin.company  = job-hdr.company
                      and (fg-bin.job-no  = job-hdr.job-no /*or fg-bin.job-no = ""*/)
                      and (fg-bin.job-no2 = job-hdr.job-no2 /*or fg-bin.job-no2 = 0*/)
                      AND ASI.fg-bin.i-no = job-hdr.i-no                   
                      AND fg-bin.loc      = fg-rctd.loc:SCREEN-VALUE  IN BROWSE {&browse-name}
                      AND fg-bin.loc-bin  = fg-rctd.loc-bin:SCREEN-VALUE   IN BROWSE {&browse-name}
                      NO-LOCK NO-ERROR.
  IF NOT AVAIL fg-bin THEN
  FIND FIRST fg-bin WHERE fg-bin.company = job-hdr.company
                      and (fg-bin.job-no = job-hdr.job-no /*or fg-bin.job-no = ""*/)
                      and (fg-bin.job-no2 = job-hdr.job-no2 /*or fg-bin.job-no2 = 0*/)
                      AND ASI.fg-bin.i-no = job-hdr.i-no
                      /*and fg-bin.qty <> 0*/
                      NO-LOCK NO-ERROR.
  /*
  job-hdr.job-no:screen-value in browse {&browse-name} + "," +
                  job-hdr.job-no2:screen-value in browse {&browse-name} + "," +
                  fg-bin.loc + "," + 
                  fg-bin.loc-bin + "," + 
                  string(fg-bin.case-count) + "," +
                  string(fg-bin.cases-unit) + "," +
                  job-hdr.i-no + "," +
                  fg-bin.tag + "," +
                  string(fg-bin.partial-count) + "," +
                  lv-unit:SCREEN-VALUE 
  */

  IF fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = "" THEN do:
     fg-rctd.i-no:SCREEN-VALUE = job-hdr.i-no.
     find first itemfg {sys/look/itemfgrlW.i}
        and itemfg.i-no EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        no-lock no-error.
     fg-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.i-name.
  END.
  ASSIGN fg-rctd.job-no:screen-value in browse {&browse-name} = job-hdr.job-no
         fg-rctd.job-no2:screen-value in browse {&browse-name} = string(job-hdr.job-no2)
         fg-rctd.loc:SCREEN-VALUE = IF AVAIL fg-bin THEN fg-bin.loc ELSE ""
         fg-rctd.loc-bin:SCREEN-VALUE = IF AVAIL fg-bin THEN fg-bin.loc-bin ELSE ""
         fg-rctd.cases:SCREEN-VALUE = IF AVAIL fg-bin THEN string(fg-bin.case-count) ELSE ""
         fg-rctd.qty-case:SCREEN-VALUE = IF AVAIL fg-bin THEN string(fg-bin.cases-unit) ELSE ""
         fg-rctd.cases-unit:SCREEN-VALUE = IF AVAIL fg-bin THEN string(fg-bin.cases-unit) ELSE ""
         fg-rctd.tag:SCREEN-VALUE = IF AVAIL fg-bin THEN fg-bin.tag ELSE ""
         fg-rctd.cust-no:SCREEN-VALUE = IF AVAIL fg-bin THEN fg-bin.cust-no ELSE ""
         fg-rctd.partial:SCREEN-VALUE = IF AVAIL fg-bin THEN string(fg-bin.partial-count) ELSE ""
         .  
  RUN calc-t-qty.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fgbin-help B-table-Win 
PROCEDURE fgbin-help :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rowid AS ROWID NO-UNDO.

  EMPTY TEMP-TABLE tt-selected.
  DO WITH FRAME {&FRAME-NAME}:
    fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        FILL(" ",6 - LENGTH(TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
        TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    RUN windows/l-fgibn3.w (fg-rctd.company, fg-rctd.i-no:screen-value in browse {&browse-name}, fg-rctd.job-no:screen-value in browse {&browse-name}, INT(fg-rctd.job-no2:screen-value in browse {&browse-name}), fg-rctd.loc:screen-value in browse {&browse-name}, fg-rctd.loc-bin:screen-value in browse {&browse-name}, fg-rctd.tag:screen-value in browse {&browse-name}, output lv-rowid).
    /* old way : for one record selection
    FIND fg-bin WHERE ROWID(fg-bin) EQ lv-rowid NO-LOCK NO-ERROR.

    IF AVAIL fg-bin AND (fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}       NE fg-bin.job-no  OR
                         INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) NE fg-bin.job-no2 OR
                         fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}          NE fg-bin.loc     OR
                         fg-rctd.loc-bin:SCREEN-VALUE IN browse {&browse-name}      NE fg-bin.loc-bin OR
                         fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}          NE fg-bin.tag)
    THEN DO:
      ASSIGN
       fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}  = fg-bin.job-no
       fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.job-no2)
       fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = fg-bin.loc
       fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = fg-bin.loc-bin
       fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}     = fg-bin.tag
       lv-org-loc = fg-rctd.loc:SCREEN-VALUE
       lv-org-loc-bin = fg-rctd.loc-bin:SCREEN-VALUE   .

      RUN new-bin.
    END.  */

    FIND FIRST tt-selected NO-LOCK NO-ERROR.
    IF AVAIL tt-selected THEN DO:
       lv-date = DATE(fg-rctd.rct-date:SCREEN-VALUE IN BROWSE {&browse-name}).
       RUN dispatch ('cancel-record').
       RUN crt-pcount.
    END.

  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-def-values B-table-Win 
PROCEDURE get-def-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  find first itemfg
      {sys/look/itemfgrlW.i}
        and itemfg.i-no EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
      no-lock no-error.
  fg-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.i-name.
      
  IF adm-new-record THEN DO:
    find first fg-ctrl where fg-ctrl.company eq cocode no-lock no-error.
    /*assign
        /*fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(itemfg.case-count)*/
     can get following values from job's lookup
     fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}      = ""
     fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}  = "".
  */     

     IF fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}      = ""  THEN
     do:
      find first fg-bin
          where fg-bin.company eq cocode
            and fg-bin.loc     eq itemfg.def-loc
            and fg-bin.loc-bin eq itemfg.def-loc-bin
            and fg-bin.i-no    eq ""
          no-lock no-error.
      if avail fg-bin then 
        assign
         fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = itemfg.def-loc
         fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.def-loc-bin.
    end. /*else FGFILE*/

    /*if bin and warehouse are blank, goto cust "X" shipto file*/
    if fg-rctd.loc:SCREEN-VALUE eq "" and fg-rctd.loc-bin:SCREEN-VALUE eq "" then do:
      find first cust
          where cust.company eq cocode
            and cust.active  eq "X"
          no-lock no-error.
                                
      if avail cust then do:
        find first shipto
            where shipto.company eq cocode
              and shipto.cust-no eq cust.cust-no  
            no-lock no-error.
           
        if avail shipto then do:
          find first fg-bin
              where fg-bin.company eq cocode
                and fg-bin.loc     eq shipto.loc
                and fg-bin.loc-bin eq shipto.loc-bin
                and fg-bin.i-no    eq ""
              no-lock no-error.
          if avail fg-bin then
            assign
             fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = shipto.loc
             fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = shipto.loc-bin.
        end.                                  
      end.
    end. 

    RELEASE fg-bin.

    FIND FIRST fg-bin
        WHERE fg-bin.company EQ cocode
          AND fg-bin.i-no    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
          AND (fg-bin.tag = fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} OR
               trim(fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}) = "")
          AND fg-bin.qty GT 0
        NO-LOCK NO-ERROR.
    IF NOT AVAIL fg-bin THEN
    FIND FIRST fg-bin
        WHERE fg-bin.company EQ cocode
          AND fg-bin.i-no    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.qty GT 0
        NO-LOCK NO-ERROR.
    IF NOT AVAIL fg-bin THEN
    FIND FIRST fg-bin
        WHERE fg-bin.company EQ cocode
          AND fg-bin.i-no    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
          AND (fg-bin.tag = fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} OR
               trim(fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}) = "")
        NO-LOCK NO-ERROR.
    IF NOT AVAIL fg-bin THEN
    FIND fg-bin
        WHERE fg-bin.company      EQ cocode
          AND fg-bin.i-no         EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND TRIM(fg-bin.job-no) NE ""
        NO-LOCK NO-ERROR.
    IF NOT AVAIL fg-bin THEN
    FIND fg-bin
        WHERE fg-bin.company      EQ cocode
          AND fg-bin.i-no         EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND TRIM(fg-bin.job-no) EQ ""
        NO-LOCK NO-ERROR.
    IF AVAIL fg-bin THEN 
      ASSIGN
       fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = fg-bin.loc
       fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = fg-bin.loc-bin
/*        fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}     = fg-bin.tag */
       fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} = fg-bin.cust-no
       fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}  = fg-bin.job-no
       fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.job-no2).

    ASSIGN
     lv-org-loc     = fg-rctd.loc:SCREEN-VALUE
     lv-org-loc-bin = fg-rctd.loc-bin:SCREEN-VALUE.
  END.

  /*if fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}  eq "" then do:*/
     find first fg-bin where fg-bin.company eq cocode
                and fg-bin.i-no    eq fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                and fg-bin.job-no  eq fg-rctd.job-no:SCREEN-VALUE
                and ((fg-rctd.job-no:SCREEN-VALUE ne " " and
                    fg-bin.job-no2 eq int(fg-rctd.job-no2:SCREEN-VALUE) ) or
                    (fg-rctd.job-no:SCREEN-VALUE eq " "))
                and fg-bin.qty     le 0              
                no-lock no-error.
     if avail fg-bin AND  fg-rctd.loc:SCREEN-VALUE = "" then 
        ASSIGN fg-rctd.loc:SCREEN-VALUE     = fg-bin.loc
               fg-rctd.loc-bin:SCREEN-VALUE = fg-bin.loc-bin
/*                fg-rctd.tag:SCREEN-VALUE     = fg-bin.tag */
               fg-rctd.cust-no:SCREEN-VALUE = fg-bin.cust-no
               lv-org-loc = fg-rctd.loc:SCREEN-VALUE
               lv-org-loc-bin = fg-rctd.loc-bin:SCREEN-VALUE .
    ELSE if avail itemfg AND fg-rctd.loc:SCREEN-VALUE = "" THEN
        ASSIGN fg-rctd.loc:SCREEN-VALUE     = itemfg.def-loc
               fg-rctd.loc-bin:SCREEN-VALUE = itemfg.def-loc-bin
               lv-org-loc = fg-rctd.loc:SCREEN-VALUE
               lv-org-loc-bin = fg-rctd.loc-bin:SCREEN-VALUE 
               /*fg-rctd.qty-case:SCREEN-VALUE = /*IF fg-rctd.po-no:SCREEN-VALUE = "" and
                                                  fg-rctd.job-no:SCREEN-VALUE = "" 
                                                THEN   STRING(itemfg.case-count)
                                                ELSE fg-rctd.qty-case:SCREEN-VALUE
                                                */
                                                STRING(itemfg.case-count)*/
               .

  RUN new-bin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ld-t-qty LIKE fg-rctd.t-qty NO-UNDO.
  DEF VAR ld-cost AS DEC NO-UNDO.
  DEF VAR lv-uom AS CHAR NO-UNDO.

  
  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
    ld-t-qty = DEC(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}).
  END.

  IF length(fg-rctd.job-no:SCREEN-VALUE) < 6 THEN
    fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        FILL(" ",6 - LENGTH(TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
        TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

    ASSIGN 
        fg-rctd.trans-time = TIME
        fg-rctd.enteredBy  = USERID("asi")
        fg-rctd.enteredDT  = DATETIME(TODAY, MTIME) 
        .
  /* Code placed here will execute AFTER standard behavior.    */
  fg-rctd.t-qty = ld-t-qty.

  FIND FIRST fg-bin 
      WHERE fg-bin.company EQ fg-rctd.company
        AND fg-bin.i-no    EQ fg-rctd.i-no
        AND fg-bin.job-no  EQ fg-rctd.job-no
        AND fg-bin.job-no2 EQ fg-rctd.job-no2
        AND fg-bin.loc     EQ fg-rctd.loc
        AND fg-bin.loc-bin EQ fg-rctd.loc-bin
        AND fg-bin.tag     EQ fg-rctd.tag
        AND fg-bin.cust-no EQ fg-rctd.cust-no
      NO-LOCK NO-ERROR.

  IF AVAIL fg-bin THEN
     ASSIGN
        fg-rctd.ext-cost = fg-rctd.t-qty /
                           (IF fg-bin.pur-uom EQ "M" THEN 1000 ELSE 1) *
                           fg-bin.std-tot-cost
        fg-rctd.cost     = fg-rctd.ext-cost / fg-rctd.t-qty
        fg-rctd.cost-uom = fg-bin.pur-uom.
  ELSE
      RUN fg/fgrctdcst.p (INPUT ROWID(fg-rctd), INPUT fg-rctd.t-qty,
                          OUTPUT fg-rctd.cost, OUTPUT fg-rctd.cost-uom,
                          OUTPUT fg-rctd.ext-cost).

  /* Pur-uom blank was causing miscalculation during cost recalc */
  IF fg-rctd.pur-uom = "" THEN
      fg-rctd.pur-uom = fg-rctd.cost-uom.

  IF fg-rctd.ext-cost EQ ? THEN fg-rctd.ext-cost = 0.
  IF fg-rctd.cost     EQ ? THEN fg-rctd.cost = 0.

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
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"trans-source", OUTPUT char-hdl).
  RUN reset-button IN WIDGET-HANDLE(char-hdl) (yes).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rno LIKE fg-rctd.r-no NO-UNDO.
  DEF BUFFER b-fg-rctd FOR fg-rctd.

  /* gdm - */
  DEF VAR v_rct-date LIKE fg-rctd.rct-date NO-UNDO.
  
  /* Code placed here will execute PRIOR to standard behavior. */
  
  FIND LAST b-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
  IF AVAIL b-fg-rctd AND b-fg-rctd.r-no GT lv-rno THEN lv-rno = b-fg-rctd.r-no.

  FOR LAST b-fg-rctd FIELDS(company r-no rct-date) NO-LOCK 
      WHERE b-fg-rctd.company = cocode
        AND b-fg-rctd.rita-code = "C".


      ASSIGN v_rct-date = b-fg-rctd.rct-date.

  END.
  IF v_rct-date = ? THEN v_rct-date = TODAY.

  FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
  IF AVAIL fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN lv-rno = fg-rcpth.r-no.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WHILE TRUE:
    lv-rno = lv-rno + 1.
    FIND FIRST fg-rcpth WHERE fg-rcpth.r-no EQ lv-rno USE-INDEX r-no NO-LOCK NO-ERROR.
    IF AVAIL fg-rcpth THEN NEXT.
    FIND FIRST b-fg-rctd WHERE b-fg-rctd.r-no EQ lv-rno USE-INDEX fg-rctd NO-LOCK NO-ERROR.
    IF AVAIL b-fg-rctd THEN NEXT.
    LEAVE.
  END.

  ASSIGN
   fg-rctd.company   = cocode
   fg-rctd.r-no      = lv-rno
   fg-rctd.rita-code = "C"
   fg-rctd.trans-time = TIME.

  IF adm-adding-record THEN DO:
    ASSIGN
     fg-rctd.rct-date     = v_rct-date
     fg-rctd.s-num        = 0
     fg-rctd.units-pallet = 1
     fg-rctd.cases-unit   = 1.
    DISPLAY fg-rctd.rct-date WITH BROWSE {&browse-name}.
  END.

  lv-recid = recid(fg-rctd).  

/*
  run tag-method (output lv-tag-meth). 
  /*  if lv-tag-meth and fg-rctd:po-no:screen*/
  run tag-sequence.
*/  
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

  /*
  run get-link-handle in adm-broker-hdl (this-procedure,"record-target", output out-hd-lst).
  hd-post = widget-handle(out-hd-lst).  /* procedure */
  if valid-handle(widget-handle(out-hd-lst)) then do:
     hd-post-child = hd-post:current-window.    
    /*  
     do while valid-handle(hd-post-child):
        ii = ii + 1.
        hd-post-child = hd-post-child:first-child.  /* frame */
       /* if hd-post-child:type = "field-group" 
           then hd-next = hd-post-child:next-sibling.
       */
       message ii valid-handle(hd-post-child) hd-post-child:name hd-post-child:type.   
     end. 
    */ 
     hd-post-child = hd-post-child:first-child.  /* frame */
     hd-post-child = hd-post-child:first-child. /* field-group */
     hd-post-child = hd-post-child:first-child.  /* field */
/*   message valid-handle(hd-post-child) hd-post-child:name hd-post-child:type.
*/
     hd-post-child:sensitive = no.
  end.
  */
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"trans-source", OUTPUT char-hdl).
  RUN reset-button IN WIDGET-HANDLE(char-hdl) (NO).

  apply "entry" to fg-rctd.rct-date in browse {&browse-name}.
  return no-apply.
 
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
  RUN valid-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-job-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-job-no2 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-loc NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-loc-bin NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

 /*
  RUN valid-tag NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
 */
  RUN validate-count .

  RUN valid-cust-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN calc-t-qty.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO:
      /*IF fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} NE "" */
      IF fgrecpt-int = 1 THEN  ll-crt-transfer = YES.
      ELSE IF CAN-FIND(FIRST fg-bin
                    WHERE fg-bin.company EQ cocode 
                      AND fg-bin.i-no    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND fg-bin.tag     EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND fg-bin.job-no  EQ fg-rctd.job-no
                      AND fg-bin.job-no2 EQ fg-rctd.job-no2
                      AND (fg-bin.loc     NE fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                           OR  fg-bin.loc-bin NE fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name})
                      /*AND fg-bin.qty > 0*/
                    USE-INDEX tag)
       THEN MESSAGE "Reduce All Existing Bin Location To Zero Qty?"
                 VIEW-AS ALERT-BOX WARNING BUTTON YES-NO UPDATE ll-crt-transfer.
    IF ll-crt-transfer THEN DO:
       RUN crt-transfer.
    END.
  END.
  
  RUN repo-query (ROWID(fg-rctd)).
  DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
       APPLY 'cursor-left' TO {&BROWSE-NAME}.
    END.
  END.
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"trans-source", OUTPUT char-hdl).
  RUN reset-button IN WIDGET-HANDLE(char-hdl) (yes).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-bin B-table-Win 
PROCEDURE new-bin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        FILL(" ",6 - LENGTH(TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
        TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    FIND FIRST fg-bin 
        WHERE fg-bin.company EQ cocode
          AND fg-bin.i-no    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
          AND fg-bin.loc     EQ fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.loc-bin EQ fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.tag     EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.cust-no EQ fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF AVAIL fg-bin THEN DO:
      ASSIGN
       /*fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.case-count)*/
       fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}   = fg-bin.job-no
       fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(fg-bin.job-no2)
       fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}      = 
           IF fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name} = "" THEN caps(fg-bin.loc) ELSE fg-rctd.loc:SCREEN-VALUE 
       fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}  = 
           IF fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = ""  THEN caps(fg-bin.loc-bin) ELSE fg-rctd.loc-bin:SCREEN-VALUE
       fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}      = CAPS(fg-bin.tag)
       fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}      = 
           IF fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} = "" THEN caps(fg-bin.cust-no) ELSE fg-rctd.cust-no:SCREEN-VALUE 
       lv-org-loc = fg-rctd.loc:SCREEN-VALUE
       lv-org-loc-bin = fg-rctd.loc-bin:SCREEN-VALUE 
       /*ld-cost                                                = fg-bin.std-tot-cost*/.
      RUN valid-tag.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-job-no B-table-Win 
PROCEDURE new-job-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    lv-new-job-ran = YES.

    IF fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
      FIND FIRST job-hdr
          WHERE job-hdr.company EQ fg-rctd.company
            AND job-hdr.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND job-hdr.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
            AND job-hdr.i-no    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          NO-LOCK NO-ERROR.

      IF AVAIL job-hdr THEN DO:
        ASSIGN
         fg-rctd.job-no:SCREEN-VALUE   = job-hdr.job-no
         fg-rctd.job-no2:SCREEN-VALUE  = STRING(job-hdr.job-no2)
         lv-job-no                     = fg-rctd.job-no:SCREEN-VALUE
         lv-job-no2                    = fg-rctd.job-no2:SCREEN-VALUE.       

        IF CAN-FIND(FIRST fg-bin
                    WHERE fg-bin.company EQ cocode
                      AND fg-bin.i-no    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND fg-bin.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND fg-bin.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
                      AND (fg-bin.tag = fg-rctd.tag:SCREEN-VALUE OR trim(fg-rctd.tag:SCREEN-VALUE) = ""))
        THEN RUN get-def-values.
      END.
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
  {src/adm/template/snd-list.i "fg-rctd"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE trans-count B-table-Win 
PROCEDURE trans-count :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-wh-list AS cha  NO-UNDO.
  DEF BUFFER b-fg-rctd FOR fg-rctd.
  DEF BUFFER b2-fg-rctd FOR fg-rctd.

  lv-wh-list = fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name} + "," +
               fg-rctd.loc-bin:SCREEN-VALUE + "," +
               fg-rctd.loc:SCREEN-VALUE + "," +
               fg-rctd.loc-bin:SCREEN-VALUE .
             
  RUN fg/d-phys.w (INPUT-OUTPUT lv-wh-list).
  IF lv-wh-list <> "" THEN DO:
     MESSAGE "Are you sure to transfer all physical counts for item "
          fg-rctd.i-no:SCREEN-VALUE "?" SKIP
         lv-wh-list
         VIEW-AS ALERT-BOX WARNING BUTTON YES-NO UPDATE ll-ans AS LOG.
     IF ll-ans THEN DO:

        DEF VAR lv-rno LIKE fg-rctd.r-no NO-UNDO.
        DEF VAR lv-rctd-rowid AS ROWID NO-UNDO.
        
        lv-rno = 0.
        FIND LAST b-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
        IF AVAIL b-fg-rctd AND b-fg-rctd.r-no GT lv-rno THEN lv-rno = b-fg-rctd.r-no.
    
        FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
        IF AVAIL fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN lv-rno = fg-rcpth.r-no.
    
        DO WHILE TRUE:
          lv-rno = lv-rno + 1.
          FIND FIRST fg-rcpth WHERE fg-rcpth.r-no EQ lv-rno USE-INDEX r-no NO-LOCK NO-ERROR.
          IF AVAIL fg-rcpth THEN NEXT.
          FIND FIRST b-fg-rctd WHERE b-fg-rctd.r-no EQ lv-rno USE-INDEX fg-rctd NO-LOCK NO-ERROR.
          IF AVAIL b-fg-rctd THEN NEXT.
          LEAVE.
        END.
    
        /*FOR EACH b-fg-rctd WHERE recid(b-fg-rctd) <> RECID(fg-rctd) 
                           AND b-fg-rctd.i-no = fg-rctd.i-no
                           AND b-fg-rctd.tag = fg-rctd.tag:
          DELETE b-fg-rctd.
        END.
        */
        FOR EACH b-fg-rctd WHERE b-fg-rctd.company = g_company 
                             AND b-fg-rctd.rita-code = "C" 
                             AND b-fg-rctd.i-no = fg-rctd.i-no
                             AND b-fg-rctd.loc = ENTRY(1,lv-wh-list)
                             AND b-fg-rctd.loc-bin = ENTRY(2,lv-wh-list)
                      :
            /*,
          EACH fg-bin WHERE fg-bin.company EQ cocode 
                          AND fg-bin.i-no    EQ b-fg-rctd.i-no
                          AND fg-bin.job-no = b-fg-rctd.job-no
                          AND fg-bin.job-no2 = b-fg-rctd.job-no2 
                          AND fg-bin.tag     EQ b-fg-rctd.tag
                          /*AND fg-bin.qty > 0*/  NO-LOCK:  
         IF fg-bin.loc NE b-fg-rctd.loc OR  fg-bin.loc-bin NE b-fg-rctd.loc-bin
         THEN DO:
         
             CREATE b2-fg-rctd.
             BUFFER-COPY b-fg-rctd EXCEPT b-fg-rctd.r-no TO b2-fg-rctd.
             ASSIGN b2-fg-rctd.r-no = lv-rno
                    b2-fg-rctd.loc = fg-bin.loc
                    b2-fg-rctd.loc-bin = fg-bin.loc-bin
                    b2-fg-rctd.cases = 0
                    b2-fg-rctd.qty-case = 0
                    b2-fg-rctd.cases-unit = 0
                    b2-fg-rctd.partial = 0
                    b2-fg-rctd.t-qty = 0.
             lv-rno = lv-rno + 1.
             
         END. */
             CREATE b2-fg-rctd.
             BUFFER-COPY b-fg-rctd EXCEPT b-fg-rctd.r-no TO b2-fg-rctd.
             ASSIGN b2-fg-rctd.r-no = lv-rno
                    b2-fg-rctd.loc = entry(3,lv-wh-list)
                    b2-fg-rctd.loc-bin = ENTRY(4,lv-wh-list)
                    b-fg-rctd.cases = 0
                    b-fg-rctd.partial = 0
                    b-fg-rctd.t-qty = 0.
             lv-rno = lv-rno + 1.
            
        END.  /* for each fg-bin*/
        RUN repo-query (ROWID(fg-rctd)).
     END. /* ll-ans */
  END. /*lv-wh-list <> ""*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust-no B-table-Win 
PROCEDURE valid-cust-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
    IF fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" AND
       NOT CAN-FIND(FIRST cust
                    WHERE cust.company EQ cocode
                      AND cust.cust-no EQ fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name})
    THEN DO:
      MESSAGE "Invalid Customer#, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fg-rctd.cust-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
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

  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST itemfg
                    {sys/look/itemfgrlW.i}
                      AND itemfg.i-no EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name})
    THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fg-rctd.i-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-loc-bin-tag B-table-Win 
PROCEDURE valid-job-loc-bin-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-int AS INT NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        FILL(" ",6 - LENGTH(TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
        TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF NOT CAN-FIND(FIRST fg-bin 
                    WHERE fg-bin.company  EQ cocode
                      AND fg-bin.i-no     EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND (fg-bin.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}       OR ip-int LT 1)
                      AND (fg-bin.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) OR ip-int LT 2)
                      AND (fg-bin.loc     EQ fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}          OR ip-int LT 3)
                      AND (fg-bin.loc-bin EQ fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}      OR ip-int LT 4)
                      AND (fg-bin.tag     EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}          OR ip-int LT 5))
    THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX.
      IF ip-int EQ 1 THEN
        APPLY "entry" TO fg-rctd.job-no IN BROWSE {&browse-name}.
      ELSE
      IF ip-int EQ 2 THEN
        APPLY "entry" TO fg-rctd.job-no2 IN BROWSE {&browse-name}.
      ELSE
      IF ip-int EQ 3 THEN
        APPLY "entry" TO fg-rctd.loc IN BROWSE {&browse-name}.
      ELSE
        APPLY "entry" TO fg-rctd.loc-bin IN BROWSE {&browse-name}.
      RETURN ERROR.
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
  
  DO WITH FRAME {&frame-name}:
    fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        FILL(" ",6 - LENGTH(TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
        TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE TRIM(lv-job-no)  OR
       DEC(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) NE DEC(lv-job-no2) THEN
      RUN new-job-no.

    IF fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:

      FIND FIRST job-hdr
          WHERE job-hdr.company EQ fg-rctd.company
            AND job-hdr.job-no  EQ fg-rctd.job-no:SCREEN-VALUE
            AND job-hdr.i-no    EQ fg-rctd.i-no:SCREEN-VALUE
          NO-LOCK NO-ERROR.
      IF NOT AVAIL job-hdr THEN DO:
         IF fg-rctd.i-no:SCREEN-VALUE = "" THEN DO: 
            FIND FIRST job-hdr WHERE job-hdr.company EQ fg-rctd.company
                         AND job-hdr.job-no  EQ fg-rctd.job-no:SCREEN-VALUE NO-LOCK NO-ERROR.
            IF NOT AVAIL job-hdr THEN do:
               MESSAGE "Item# - " fg-rctd.i-no:SCREEN-VALUE " is not on Job# - " fg-rctd.job-no:SCREEN-VALUE "-" fg-rctd.job-no2:SCREEN-VALUE  VIEW-AS ALERT-BOX ERROR.
               RETURN ERROR.
            END.
            RUN display-job-hdr (RECID(job-hdr)).

         END.
         ELSE do:   /* components*/ 
             FIND FIRST job-hdr WHERE job-hdr.company EQ g_company
                  AND job-hdr.job-no  EQ fg-rctd.job-no:SCREEN-VALUE NO-LOCK NO-ERROR.
             IF AVAIL job-hdr THEN
                FIND FIRST reftable NO-LOCK WHERE reftable.reftable EQ "jc/jc-calc.p"
                              AND reftable.company  EQ g_company
                              AND reftable.loc      EQ ""
                              AND reftable.code     EQ STRING(job-hdr.job,"999999999")
                              AND reftable.code2    EQ fg-rctd.i-no:SCREEN-VALUE NO-ERROR.
             IF NOT AVAIL reftable THEN DO:
                MESSAGE "Item# - " fg-rctd.i-no:SCREEN-VALUE " is not on Job# - " fg-rctd.job-no:SCREEN-VALUE "-" fg-rctd.job-no2:SCREEN-VALUE  VIEW-AS ALERT-BOX ERROR.
                RETURN ERROR.
             END.
         END.
      END.
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
                                                         
  DO WITH FRAME {&frame-name}:
    IF TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE TRIM(lv-job-no)  OR
       DEC(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) NE DEC(lv-job-no2) THEN
      RUN new-job-no.

    IF fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
      FIND FIRST job-hdr
          WHERE job-hdr.company EQ fg-rctd.company
            AND job-hdr.job-no  EQ fg-rctd.job-no:SCREEN-VALUE
            AND job-hdr.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE)
            AND job-hdr.i-no    EQ fg-rctd.i-no:SCREEN-VALUE
          NO-LOCK NO-ERROR.

      IF NOT AVAIL job-hdr THEN DO:
         FIND FIRST job-hdr WHERE job-hdr.company EQ g_company
                AND job-hdr.job-no  EQ fg-rctd.job-no:SCREEN-VALUE 
                AND job-hdr.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE)
               NO-LOCK NO-ERROR.
         IF NOT AVAIL job-hdr THEN DO: 
             MESSAGE "Invalid Job#. Try Help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO fg-rctd.job-no IN BROWSE {&browse-name}.
            RETURN ERROR.
         END.
         ELSE DO:
             FIND FIRST reftable NO-LOCK WHERE reftable.reftable EQ "jc/jc-calc.p"
                              AND reftable.company  EQ g_company
                              AND reftable.loc      EQ ""
                              AND reftable.code     EQ STRING(job-hdr.job,"999999999")
                              AND reftable.code2    EQ fg-rctd.i-no:SCREEN-VALUE NO-ERROR.
             IF NOT AVAIL reftable THEN DO:
                MESSAGE "Item# - " fg-rctd.i-no:SCREEN-VALUE " is not on Job# - " fg-rctd.job-no:SCREEN-VALUE "-" fg-rctd.job-no2:SCREEN-VALUE VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO fg-rctd.job-no IN BROWSE {&browse-name}.
               RETURN ERROR.
             END.
      END.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc B-table-Win 
PROCEDURE valid-loc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST loc
                    WHERE loc.company EQ cocode
                      AND loc.loc     EQ fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name})
    THEN DO:
      MESSAGE "Invalid Warehouse, try help..." VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc-bin B-table-Win 
PROCEDURE valid-loc-bin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST fg-bin
                    WHERE fg-bin.company EQ cocode 
                      AND fg-bin.i-no    EQ ""
                      AND fg-bin.loc     EQ fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND fg-bin.loc-bin EQ fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
                    USE-INDEX co-ino)
    THEN DO:
      MESSAGE "Invalid Bin#, try help..." VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
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

  DO WITH FRAME {&FRAME-NAME}:
    /*IF fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} NE "" AND
       NOT*/
      FIND FIRST fg-bin WHERE fg-bin.company EQ cocode 
                      AND fg-bin.i-no    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND fg-bin.loc     EQ fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}                         
                      AND fg-bin.loc-bin EQ fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND fg-bin.tag     EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND fg-bin.cust-no EQ fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND fg-bin.job-no = fg-rctd.job-no:SCREEN-VALUE
                      AND fg-bin.job-no2 = int(fg-rctd.job-no2:SCREEN-VALUE)
                      NO-LOCK NO-ERROR.
    IF AVAIL fg-bin THEN DO:
     
      /*MESSAGE "Invalid Tag#, try help..." VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.*/
       ASSIGN fg-rctd.cases:SCREEN-VALUE = STRING(TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0))
              fg-rctd.qty-case:SCREEN-VALUE = string(fg-bin.case-count)
              fg-rctd.cases-unit:SCREEN-VALUE = string(fg-bin.cases-unit)
              fg-rctd.partial:SCREEN-VALUE = string(fg-bin.partial-count)
              fg-rctd.t-qty:SCREEN-VALUE = string(fg-bin.qty).

     END.
     IF fg-rctd.t-qty:SCREEN-VALUE EQ "0" 
         AND fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
         FOR EACH fg-rcpth
             WHERE fg-rcpth.company   EQ cocode
             AND fg-rcpth.i-no      EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
             AND fg-rcpth.job-no    EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
             AND fg-rcpth.job-no2   EQ int(fg-rctd.job-no2:SCREEN-VALUE)
           AND fg-rcpth.rita-code EQ "R"
             USE-INDEX i-no NO-LOCK,

             FIRST fg-rdtlh
             WHERE fg-rdtlh.r-no        EQ fg-rcpth.r-no
             AND fg-rdtlh.loc         EQ fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name} 
             AND fg-rdtlh.loc-bin     EQ fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
             AND fg-rdtlh.tag         EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
          USE-INDEX rm-rdtl NO-LOCK
             BY fg-rcpth.trans-date
             BY fg-rcpth.r-no :
             ASSIGN
                 fg-rctd.cases:SCREEN-VALUE = STRING(TRUNC((fg-rdtlh.qty - fg-rdtlh.partial) / fg-rdtlh.cases,0))
                 fg-rctd.qty-case:SCREEN-VALUE = string(fg-rdtlh.cases)
                 fg-rctd.cases-unit:SCREEN-VALUE = string(fg-rdtlh.qty-case)
                 fg-rctd.partial:SCREEN-VALUE = string(fg-rdtlh.partial)
                 fg-rctd.t-qty:SCREEN-VALUE = string(fg-rdtlh.qty).
             LEAVE.
         END.
             
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

  FIND itemfg WHERE itemfg.company = fg-rctd.company
                AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                NO-LOCK NO-ERROR.
  IF NOT AVAIL itemfg THEN DO:
     IF fg-rctd.i-no:SCREEN-VALUE = "" THEN DO:
        MESSAGE "Invalid Item. Try help. " VIEW-AS ALERT-BOX.
        APPLY "entry" TO fg-rctd.i-no .
        RETURN ERROR.
     END.
     ELSE DO:
        MESSAGE  " F/G Item is not on file.  Would you like to add it? "
                 VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
        IF NOT ll-ans THEN DO:
            APPLY "entry" TO fg-rctd.i-no .
            RETURN ERROR.           
        END.
        ELSE DO:
            RUN fg/d-crtitm.w (fg-rctd.i-no:SCREEN-VALUE).
            FIND first itemfg {sys/look/itemfgrlW.i}
                       and itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                       no-lock no-error.
            IF AVAIL itemfg THEN ASSIGN fg-rctd.i-name:SCREEN-VALUE = itemfg.i-name.
        END.
     END.
  END.
  
  FIND FIRST loc WHERE loc.company = cocode
                        AND loc.loc = fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                        NO-LOCK NO-ERROR.
       IF NOT AVAIL loc THEN DO:
          MESSAGE "Invalid Warehouse. Try Help. " VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO fg-rctd.loc.
          RETURN ERROR.
  END.
  
  FIND FIRST fg-bin WHERE fg-bin.company = cocode 
                      AND fg-bin.i-no = ""
                      AND fg-bin.loc = fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND fg-bin.loc-bin = fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
                      USE-INDEX co-ino NO-LOCK NO-ERROR.
  IF NOT AVAIL fg-bin THEN DO:
          MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO fg-rctd.loc-bin.
          RETURN ERROR.
  END.
  /* ===== tag validation =====*/
  IF fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} <> ""
  THEN DO:
  
    FIND FIRST bf-tmp WHERE bf-tmp.company = cocode AND
                            bf-tmp.tag = fg-rctd.tag:SCREEN-VALUE
                        AND RECID(bf-tmp) <> RECID(fg-rctd)
                        NO-LOCK NO-ERROR.
    IF AVAIL bf-tmp THEN DO:
       MESSAGE "This Tag Number Has Already Been Used." skip
               "Please Enter A Unique Tag Number." 
           VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO fg-rctd.tag.
       RETURN ERROR.
    END.
    ELSE DO:
        find first xfg-rdtlh
               where xfg-rdtlh.company   eq cocode
                 and xfg-rdtlh.loc       eq fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                 and xfg-rdtlh.tag       eq fg-rctd.tag:SCREEN-VALUE
                 and xfg-rdtlh.qty       gt 0
                 and xfg-rdtlh.rita-code ne "S"
               use-index tag no-lock no-error.
           if avail xfg-rdtlh THEN  DO:
               MESSAGE "This Tag Number Has Already Been Used." skip
                       "Please Enter A Unique Tag Number." 
                       VIEW-AS ALERT-BOX ERROR.
               APPLY "entry" TO fg-rctd.tag.
               RETURN error.
           END.
    END.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-count B-table-Win 
PROCEDURE validate-count :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF NOT lCheckCount THEN do:
    IF fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
        FOR EACH fg-rcpth NO-LOCK
             WHERE fg-rcpth.company EQ g_company 
               AND fg-rcpth.i-no EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
               AND fg-rcpth.rita-code EQ "C"
               AND fg-rcpth.trans-date EQ date(fg-rctd.rct-date:SCREEN-VALUE IN BROWSE {&browse-name})  , 
            EACH fg-rdtlh WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no 
            AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
            AND fg-rdtlh.tag EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK: 
            MESSAGE "Note: A count is already entered for this tag for the same date with a count of "
                    STRING(fg-rdtlh.qty-case) VIEW-AS ALERT-BOX INFO .
            lCheckCount = YES .
            LEAVE .
        END.
    END.
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


  run get-matrix (true).
  return ext-cost.
  /* 
  RETURN 0.00.   /* Function return value. */
  */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

