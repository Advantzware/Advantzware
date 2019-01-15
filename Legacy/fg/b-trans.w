&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  browsers/<table>.w

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

&SCOPED-DEFINE yellowColumnsName b-trans
&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{custom/gcompany.i}
{custom/gloc.i}

{sys/inc/VAR.i NEW SHARED}

def var char-val as cha no-undo.
def var ext-cost as decimal no-undo.
def var lv-recid as recid no-undo.
def var ls-prev-po as cha no-undo.
def var hd-post as widget-handle no-undo.
def var hd-post-child as widget-handle no-undo.
def var ll-help-run as log no-undo.  /* set on browse help, reset row-entry */
DEF VAR lv-fgrecpt-val AS INT NO-UNDO.
DEF VAR trans-time AS CHAR NO-UNDO.
DEF VAR lv-new-tag-number-chosen AS LOG NO-UNDO.
&SCOPED-DEFINE item-key-phrase TRUE

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
STRING(fg-rctd.trans-time,'HH:MM') @ trans-time fg-rctd.i-no fg-rctd.i-name ~
fg-rctd.job-no fg-rctd.job-no2 fg-rctd.loc fg-rctd.loc-bin fg-rctd.tag ~
fg-rctd.cust-no fg-rctd.cases fg-rctd.qty-case fg-rctd.partial fg-rctd.loc2 ~
fg-rctd.loc-bin2 fg-rctd.tag2 fg-rctd.created-by fg-rctd.updated-by 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table fg-rctd.rct-date ~
fg-rctd.i-no fg-rctd.i-name fg-rctd.job-no fg-rctd.job-no2 fg-rctd.loc ~
fg-rctd.loc-bin fg-rctd.tag fg-rctd.cust-no fg-rctd.cases fg-rctd.qty-case ~
fg-rctd.partial fg-rctd.loc2 fg-rctd.loc-bin2 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table fg-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table fg-rctd
&Scoped-define QUERY-STRING-Browser-Table FOR EACH fg-rctd WHERE ~{&KEY-PHRASE} ~
      AND fg-rctd.company = gcompany and ~
fg-rctd.rita-code = "T" NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH fg-rctd WHERE ~{&KEY-PHRASE} ~
      AND fg-rctd.company = gcompany and ~
fg-rctd.rita-code = "T" NO-LOCK ~
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
     SIZE 118 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 96 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 2.62.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 146 BY 18.1.

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
      fg-rctd.rct-date COLUMN-LABEL "Transfer Date" FORMAT "99/99/9999":U
            LABEL-BGCOLOR 14
      STRING(fg-rctd.trans-time,'HH:MM') @ trans-time COLUMN-LABEL "Transfer!Time"
            WIDTH 11.6
      fg-rctd.i-no COLUMN-LABEL "Item" FORMAT "x(15)":U WIDTH 23.2
            LABEL-BGCOLOR 14
      fg-rctd.i-name COLUMN-LABEL "Name/Desc" FORMAT "x(30)":U
            LABEL-BGCOLOR 14
      fg-rctd.job-no COLUMN-LABEL "Job#" FORMAT "x(6)":U LABEL-BGCOLOR 14 WIDTH 10
      fg-rctd.job-no2 FORMAT "99":U
      fg-rctd.loc COLUMN-LABEL "From!Whse" FORMAT "x(5)":U LABEL-BGCOLOR 14 WIDTH 8
      fg-rctd.loc-bin COLUMN-LABEL "From!Bin" FORMAT "x(8)":U LABEL-BGCOLOR 14
      fg-rctd.tag COLUMN-LABEL "From!Tag" FORMAT "x(20)":U LABEL-BGCOLOR 14 WIDTH 29
      fg-rctd.cust-no COLUMN-LABEL "Customer#" FORMAT "x(8)":U
            WIDTH 12 LABEL-BGCOLOR 14
      fg-rctd.cases COLUMN-LABEL "Units" FORMAT ">>>,>>9":U LABEL-BGCOLOR 14
      fg-rctd.qty-case COLUMN-LABEL "Qty/Unit" FORMAT ">>>,>>9":U
            LABEL-BGCOLOR 14
      fg-rctd.partial COLUMN-LABEL "Partial" FORMAT ">>>,>>9":U
            LABEL-BGCOLOR 14
      fg-rctd.loc2 COLUMN-LABEL "To!Whse" FORMAT "x(5)":U LABEL-BGCOLOR 14 WIDTH 8
      fg-rctd.loc-bin2 COLUMN-LABEL "To!Bin" FORMAT "x(8)":U LABEL-BGCOLOR 14
      fg-rctd.tag2 COLUMN-LABEL "To!Tag" FORMAT "x(20)":U LABEL-BGCOLOR 14 WIDTH 29
      fg-rctd.created-by COLUMN-LABEL "Created By" FORMAT "x(8)":U
            WIDTH 15 LABEL-BGCOLOR 14
      fg-rctd.updated-by COLUMN-LABEL "Last Updated By" FORMAT "x(8)":U
            WIDTH 15 LABEL-BGCOLOR 14
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
      fg-rctd.partial
      fg-rctd.loc2
      fg-rctd.loc-bin2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 144 BY 15.24
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 2 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 16.48 COL 7 HELP
          "Select Browser Sort Order" NO-LABEL
     fi_sortby AT ROW 16.48 COL 102 COLON-ALIGNED NO-LABEL
     auto_find AT ROW 17.67 COL 12 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 17.67 COL 133 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 16.48 COL 3
     RECT-4 AT ROW 16.24 COL 2
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
     _Where[1]         = "fg-rctd.company = gcompany and
fg-rctd.rita-code = ""T"""
     _FldNameList[1]   > asi.fg-rctd.r-no
"r-no" "Seq#" ">>>>>>>>" "integer" ? ? ? 14 ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.fg-rctd.rct-date
"rct-date" "Transfer Date" ? "date" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"STRING(fg-rctd.trans-time,'HH:MM') @ trans-time" "Transfer!Time" ? ? ? ? ? ? ? ? no ? no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > asi.fg-rctd.i-no
"i-no" "Item" "x(15)" "character" ? ? ? 14 ? ? yes ? no no "23.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > asi.fg-rctd.i-name
"i-name" "Name/Desc" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > asi.fg-rctd.job-no
"job-no" "Job#" ? "character" ? ? ? 14 ? ? yes ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > asi.fg-rctd.job-no2
"job-no2" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > asi.fg-rctd.loc
"loc" "From!Whse" ? "character" ? ? ? 14 ? ? yes ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > asi.fg-rctd.loc-bin
"loc-bin" "From!Bin" ? "character" ? ? ? 14 ? ? yes ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > asi.fg-rctd.tag
"tag" "From!Tag" "x(20)" "character" ? ? ? 14 ? ? yes ? no no "29" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > asi.fg-rctd.cust-no
"cust-no" "Customer#" ? "character" ? ? ? 14 ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > asi.fg-rctd.cases
"cases" "Units" ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > asi.fg-rctd.qty-case
"qty-case" "Qty/Unit" ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > asi.fg-rctd.partial
"partial" "Partial" ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > asi.fg-rctd.loc2
"loc2" "To!Whse" ? "character" ? ? ? 14 ? ? yes ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > asi.fg-rctd.loc-bin2
"loc-bin2" "To!Bin" ? "character" ? ? ? 14 ? ? yes ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > asi.fg-rctd.tag2
"tag2" "To!Tag" "x(20)" "character" ? ? ? 14 ? ? no ? no no "29" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > asi.fg-rctd.created-by
"created-by" "Created By" ? "character" ? ? ? 14 ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > asi.fg-rctd.updated-by
"updated-by" "Last Updated By" ? "character" ? ? ? 14 ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
 DEF VAR lv-rowid AS ROWID NO-UNDO.

 IF NOT avail fg-rctd then find fg-rctd where recid(fg-rctd) = lv-recid no-lock no-error. 
 
 def var ll-tag# as log no-undo.
 ll-help-run = yes.
 case focus:name :
     when "i-no" then do:
           run windows/l-itemfg.w (fg-rctd.company, "", FOCUS:SCREEN-VALUE, output char-val).
           if char-val <> "" then do :
              assign focus:screen-value in browse {&browse-name} = entry(1,char-val)
                     fg-rctd.i-name:screen-value in browse {&browse-name} = entry(2,char-val)
                     .
           end.
           return no-apply.   
     end.

     WHEN "job-no" THEN DO:
       RUN fgbin-help.
     END.

     WHEN "job-no2" THEN DO:
       RUN fgbin-help.
     END.

     WHEN "loc" THEN DO:
       RUN fgbin-help.
     END.

     WHEN "loc-bin" THEN DO:
       RUN fgbin-help.
     END.

     WHEN "tag" THEN DO:
       RUN fgbin-help.
     END.

     WHEN "cust-no" THEN DO:
       RUN fgbin-help.
     END.

     when "loc2" then do:
           run windows/l-loc.w (fg-rctd.company,focus:screen-value, output char-val).
           if char-val <> "" then do :
              assign focus:screen-value in  browse {&browse-name}  = entry(1,char-val).             
           end.
           return no-apply.   
     end.
     when "loc-bin2" then do:
           run windows/l-fgbin.w (fg-rctd.company,fg-rctd.loc2:screen-value,focus:screen-value, output char-val).
           if char-val <> "" then do :
              assign focus:screen-value  = entry(1,char-val).
           end.
           return no-apply.   
     end.

   end case.

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
ON LEAVE OF fg-rctd.i-no IN BROWSE Browser-Table /* Item */
DO:
  IF LASTKEY NE -1 THEN DO:      
    lv-new-tag-number-chosen = ?.

    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF fg-rctd.i-no IN BROWSE Browser-Table /* Item */
DO:
  FIND FIRST itemfg NO-LOCK
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    BEGINS {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}
       NO-ERROR.
  IF AVAIL itemfg THEN
    fg-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.i-name.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.i-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.i-name Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.i-name IN BROWSE Browser-Table /* Name/Desc */
DO:
  APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.job-no IN BROWSE Browser-Table /* Job# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-loc-bin-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.job-no2 IN BROWSE Browser-Table
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-loc-bin-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.loc IN BROWSE Browser-Table /* From!Whse */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-loc-bin-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc-bin Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.loc-bin IN BROWSE Browser-Table /* From!Bin */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-loc-bin-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.tag IN BROWSE Browser-Table /* From!Tag */
DO:
  IF LASTKEY NE -1 THEN DO:
     RUN valid-tag NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-job-loc-bin-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF fg-rctd.tag IN BROWSE Browser-Table /* Tag */
DO:
  RUN new-tag.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.cust-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.cust-no IN BROWSE Browser-Table /* Customer# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-loc-bin-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.cases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.cases Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.cases IN BROWSE Browser-Table /* Units */
DO:
  DEF BUFFER b-loadtag FOR loadtag.

  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-loc-bin-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     FIND FIRST b-loadtag WHERE
          b-loadtag.company = gcompany AND
          b-loadtag.item-type = NO AND
          b-loadtag.tag-no = fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
          NO-LOCK NO-ERROR.

     IF AVAIL b-loadtag THEN
     DO:
/* Task 10081201 - This is a transfer, so units may be less */        
/*         IF INT(fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}) < b-loadtag.tot-cases THEN */
/*         DO:                                                                                    */
/*            MESSAGE "Units is Less Than Loadtag O/H Cases."                                     */
/*                VIEW-AS ALERT-BOX ERROR BUTTONS OK.                                             */
/*            RETURN NO-APPLY.                                                                    */
/*         END.                                                                                   */

     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.qty-case
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.qty-case Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.qty-case IN BROWSE Browser-Table /* Qty/Unit */
DO:
  APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.partial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.partial Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.partial IN BROWSE Browser-Table /* Partial */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-loc-bin-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.loc2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc2 Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.loc2 IN BROWSE Browser-Table /* To!Whse */
DO:
  IF LASTKEY NE -1 THEN DO:
      IF fg-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name} EQ ""  THEN
               fg-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name} = fg-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name} .
    RUN valid-loc2 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.loc-bin2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc-bin2 Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.loc-bin2 IN BROWSE Browser-Table /* To!Bin */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-loc-bin2 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.tag2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.tag2 Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.tag2 IN BROWSE Browser-Table /* To!Tag */
DO:
  APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{custom/getloc.i}
{sys/inc/f3help.i}    
ASSIGN
 cocode = gcompany
 locode = gloc.

DO TRANSACTION:
  {sys/inc/fgrecpt.i}
  lv-fgrecpt-val = sys-ctrl.int-fld.
END.


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancel-item B-table-Win 
PROCEDURE cancel-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF AVAIL fg-rctd AND fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = "" THEN
      RUN dispatch IN THIS-PROCEDURE (INPUT 'cancel-record':U).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fgbin-help B-table-Win 
PROCEDURE fgbin-help :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lv-rowid AS ROWID NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        FILL(" ",6 - LENGTH(TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
        TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    RUN windows/l-fgibn4.w (fg-rctd.company, fg-rctd.i-no:screen-value in browse {&browse-name}, fg-rctd.job-no:screen-value in browse {&browse-name}, INT(fg-rctd.job-no2:screen-value in browse {&browse-name}), fg-rctd.loc:screen-value in browse {&browse-name}, fg-rctd.loc-bin:screen-value in browse {&browse-name}, fg-rctd.tag:screen-value in browse {&browse-name}, output lv-rowid).

    FIND fg-bin WHERE ROWID(fg-bin) EQ lv-rowid NO-LOCK NO-ERROR.

    IF AVAIL fg-bin AND (fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}       NE fg-bin.job-no  OR
                         INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) NE fg-bin.job-no2 OR
                         fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}          NE fg-bin.loc     OR
                         fg-rctd.loc-bin:SCREEN-VALUE IN browse {&browse-name}      NE fg-bin.loc-bin OR
                         fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}          NE fg-bin.tag     OR
                         fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}      NE fg-bin.cust-no)
    THEN DO:
      ASSIGN
       fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}  = fg-bin.job-no
       fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.job-no2)
       fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = fg-bin.loc
       fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = fg-bin.loc-bin
       fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}     = fg-bin.tag
       fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} = fg-bin.cust-no.
       
      IF fg-bin.loc  = "MAIN" THEN
           IF fg-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name} EQ ""  THEN
               fg-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name}      = CAPS(fg-bin.loc) .
      
      RUN new-bin.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadtag B-table-Win 
PROCEDURE loadtag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT AVAILABLE fg-rctd THEN RETURN.
  RUN custom/setUserPrint.p (INPUT fg-rctd.company,
                           INPUT 'r-loadtg.',
                           INPUT 'fi_cas-lab',
                           INPUT STRING(fg-rctd.tag2)).

  RUN Get_Procedure IN Persistent-Handle ('r-loadtg.',OUTPUT run-proc,yes).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-tag2 LIKE fg-rctd.tag2 NO-UNDO.
  DEF VAR ld AS DEC NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Buttons were made sensitive = no during add, so reverse that here */
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"Container-source",OUTPUT char-hdl).
    RUN make-buttons-sensitive IN WIDGET-HANDLE(char-hdl).

  DO WITH FRAME {&FRAME-NAME}:
    lv-tag2 = fg-rctd.tag2:SCREEN-VALUE IN BROWSE {&browse-name}.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   fg-rctd.t-qty = (fg-rctd.cases * fg-rctd.qty-case) + fg-rctd.partial
   fg-rctd.tag2  = lv-tag2
   fg-rctd.trans-time   = TIME
   fg-rctd.enteredBy = USERID("asi")
   fg-rctd.enteredDT = DATETIME(TODAY, MTIME)    
   .

  FIND FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ fg-rctd.i-no
      NO-LOCK NO-ERROR.

  IF AVAIL itemfg THEN
    ASSIGN
     fg-rctd.pur-uom  = itemfg.prod-uom
     fg-rctd.cost-uom = itemfg.prod-uom
     fg-rctd.std-cost = itemfg.std-tot-cost.

  FIND FIRST fg-bin 
      WHERE fg-bin.company EQ cocode
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
     fg-rctd.pur-uom      = fg-bin.pur-uom
     fg-rctd.cost-uom     = fg-bin.pur-uom
     fg-rctd.std-cost     = fg-bin.std-tot-cost
     fg-rctd.units-pallet = fg-bin.units-pallet
     fg-rctd.cases-unit   = fg-bin.cases-unit.

  ld = fg-rctd.std-cost.

  IF fg-rctd.pur-uom NE "EA" THEN
    RUN sys/ref/convcuom.p(fg-rctd.pur-uom, "EA", 0, 0, 0, 0,
                           ld, OUTPUT ld).

  fg-rctd.ext-cost = fg-rctd.t-qty * ld.

  IF AVAIL fg-bin                AND
     fg-bin.qty GT fg-rctd.t-qty AND
     fg-bin.tag NE ""            AND
     fg-bin.tag EQ fg-rctd.tag2  AND lv-new-tag-number-chosen THEN      
    RUN fg/mkloadtg.p (ROWID(fg-rctd), 0 /*fg-rctd.cases*/, INPUT-OUTPUT fg-rctd.tag2).
  
  lv-new-tag-number-chosen = ?.

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

    /* Buttons were made sensitive = no during add, so reverse that here */
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"Container-source",OUTPUT char-hdl).
    RUN make-buttons-sensitive IN WIDGET-HANDLE(char-hdl).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */  
  lv-new-tag-number-chosen = ?.

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
  
  /* Code placed here will execute PRIOR to standard behavior. */
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"Container-source",OUTPUT char-hdl).
  RUN make-buttons-insensitive IN WIDGET-HANDLE(char-hdl).

  lv-rno = 0.
  FIND LAST b-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
  IF AVAIL b-fg-rctd AND b-fg-rctd.r-no GT lv-rno THEN lv-rno = b-fg-rctd.r-no.

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

  assign fg-rctd.company = gcompany
         fg-rctd.r-no    = lv-rno
         fg-rctd.rita-code = "T".

  IF adm-adding-record THEN DO:
    ASSIGN
     fg-rctd.rct-date     = TODAY
     fg-rctd.trans-time   = TIME
     fg-rctd.s-num        = 0
     fg-rctd.units-pallet = 1
     fg-rctd.cases-unit   = 1.
    DISPLAY fg-rctd.rct-date WITH BROWSE {&browse-name}.
  END. 

  ASSIGN lv-recid = recid(fg-rctd).
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

  /*  progress bug - no rfqitem record available 
      if add is canceled when new line is appended to last line */
  if not avail fg-rctd then find fg-rctd where recid(fg-rctd) = lv-recid NO-LOCK no-error.

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
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  apply "entry" to fg-rctd.rct-date in browse {&browse-name}.
 
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
  
  /* when new record created from last row, get error "No fg-rctd" record ava */
  IF NOT AVAIL fg-rctd THEN
  FIND fg-rctd WHERE RECID(fg-rctd) EQ lv-recid NO-LOCK NO-ERROR.

  RUN valid-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-job-loc-bin-tag NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-loc2 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-loc-bin2 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-tag NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN repo-query (ROWID(fg-rctd)).

  DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY 'cursor-left' TO {&BROWSE-NAME}.
    END.
  END.
  
  lv-new-tag-number-chosen = ?.

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
    
    IF AVAIL fg-bin THEN
      ASSIGN
       fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.case-count)
       fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}   = fg-bin.job-no
       fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(fg-bin.job-no2)
       fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}      = CAPS(fg-bin.loc)
       fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}  = CAPS(fg-bin.loc-bin)
       fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}      = CAPS(fg-bin.tag)
       fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}  = CAPS(fg-bin.cust-no).
     IF AVAIL fg-bin THEN
         fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name} = STRING( TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)).

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-tag B-table-Win 
PROCEDURE new-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:

    IF fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
      FIND FIRST fg-bin
          WHERE fg-bin.company  EQ cocode
            AND fg-bin.tag      EQ trim(fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name})
            AND fg-bin.i-no     EQ trim(fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name})
        USE-INDEX tag NO-LOCK NO-ERROR.
        
      IF AVAIL fg-bin THEN DO:
      ASSIGN
         fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}  = fg-bin.job-no
         fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.job-no2)
         fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = fg-bin.loc
         fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = fg-bin.loc-bin
         fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}     = fg-bin.tag
         fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} = fg-bin.cust-no .
        
         IF fg-bin.loc  = "MAIN" THEN
           IF fg-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name} EQ ""  THEN
               fg-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name}      = CAPS(fg-bin.loc) .

        RUN new-bin.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-i-no B-table-Win 
PROCEDURE valid-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF NOT AVAIL itemfg THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX.
      RETURN ERROR.
    END.
  
    IF AVAILABLE itemfg AND itemfg.pur-uom EQ "" THEN 
    DO:
        MESSAGE "The finished goods item must have a valid Puchase Quantity UOM..." VIEW-AS ALERT-BOX.
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
  DEF VAR lv-fields AS CHAR INIT "job-no,job-no2,loc,loc-bin,tag,cust-no" NO-UNDO.
  DEF VAR li-field# AS INT NO-UNDO.
  DEF VAR li-fieldc AS CHAR NO-UNDO.
  

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     li-fieldc = TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name})
     li-fieldc = FILL(" ",6 - LENGTH(li-fieldc)) + li-fieldc
     fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = li-fieldc

     li-field# = LOOKUP(FOCUS:NAME IN BROWSE {&browse-name},lv-fields).

    IF li-field# EQ 0 THEN li-field# = 9999.

    IF li-field# LT 3                                          AND
       fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN li-field# = 3.

    IF li-field# LT 4                                              AND
       fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN li-field# = 4.
        
    IF li-field# LT 5                                          AND
       fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN li-field# = 5.
        
    IF li-field# LT 6                                              AND
       fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN li-field# = 6.

    FOR EACH fg-bin NO-LOCK
        WHERE fg-bin.company  EQ cocode
          AND fg-bin.i-no     EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.job-no   EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND (fg-bin.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) OR
               li-field#      LT 2)
          AND (fg-bin.loc     EQ fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}          OR
               li-field#      LT 3)
          AND (fg-bin.loc-bin EQ fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}      OR
               li-field#      LT 4)
          AND (fg-bin.tag     EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}          OR
               li-field#      LT 5)
          AND (fg-bin.cust-no EQ fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}          OR
               li-field#      LT 6)
        USE-INDEX co-ino
        BY fg-bin.qty DESC:
      LEAVE.
    END.

    IF AVAIL fg-bin AND
       fg-bin.qty GE (DEC(fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}) *
                      DEC(fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name})) +
                     DEC(fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name})
    THEN DO:
      ASSIGN
       fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.case-count)
       fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}      = CAPS(fg-bin.loc)
       fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}  = CAPS(fg-bin.loc-bin)
       fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}      = CAPS(fg-bin.tag)
       fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}  = CAPS(fg-bin.cust-no)
       fg-rctd.tag2:SCREEN-VALUE IN BROWSE {&browse-name}     = IF adm-new-record THEN CAPS(fg-bin.tag) ELSE CAPS(fg-rctd.tag2:SCREEN-VALUE IN BROWSE {&browse-name})
       fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}  = CAPS(fg-bin.cust-no).
       
       IF fg-bin.loc  = "MAIN" THEN 
           IF fg-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name} EQ ""  THEN
               fg-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name}      = CAPS(fg-bin.loc) .
       
       IF fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} NE "" AND lv-new-tag-number-chosen = ? 
           AND fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} EQ fg-rctd.tag2:SCREEN-VALUE IN BROWSE {&browse-name}  THEN DO:
           IF fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name} NE  STRING(TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count, 0)) 
               AND int(fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN DO:
                 MESSAGE "Units not matching units in Tag will need to create a new tag #"
                         SKIP(1)
                         "Click OK to continue and create a new tag number on save or cancel"
                         VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL
                         TITLE "" UPDATE choice AS LOGICAL.
                 IF choice THEN
                    lv-new-tag-number-chosen = TRUE.
                 ELSE
                    lv-new-tag-number-chosen = FALSE.
                 IF NOT lv-new-tag-number-chosen THEN
                    fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}    = STRING(TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count, 0)).
           END. /* ask if creating a new tag number */

       END.

    END.
    ELSE DO:
      IF AVAIL fg-bin THEN DO:
        MESSAGE "Insufficient qty in bin..." VIEW-AS ALERT-BOX.
        APPLY "entry" TO fg-rctd.cases IN BROWSE {&browse-name}.
      END.

      ELSE DO:
        MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX.
        IF li-field# LE 5 THEN
          APPLY "entry" TO FOCUS IN BROWSE {&browse-name}.
        ELSE
          APPLY "entry" TO fg-rctd.loc-bin IN BROWSE {&browse-name}.
      END.

      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc-bin2 B-table-Win 
PROCEDURE valid-loc-bin2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-msg AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    IF lv-msg EQ "" THEN
      IF fg-rctd.loc-bin2:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN
        lv-msg = "To Bin may not be spaces".

    IF lv-msg EQ "" THEN
      IF fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}      EQ
         fg-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name}     AND
         fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}  EQ
         fg-rctd.loc-bin2:SCREEN-VALUE IN BROWSE {&browse-name} THEN
        lv-msg = "To Whse/Bin may not be the same as From Whse/Bin".

    IF lv-msg EQ "" THEN DO:
      FIND FIRST fg-bin
          WHERE fg-bin.company EQ cocode
            AND fg-bin.i-no    EQ ""
            AND fg-bin.loc     EQ fg-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name}
            AND fg-bin.loc-bin EQ fg-rctd.loc-bin2:SCREEN-VALUE IN BROWSE {&browse-name}
        USE-INDEX co-ino NO-LOCK NO-ERROR.

      IF NOT AVAIL fg-bin THEN lv-msg = "Invalid entry, try help...".
    END.

    IF lv-msg NE "" THEN DO:
      MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX.
      APPLY "entry" TO fg-rctd.loc-bin2 IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc2 B-table-Win 
PROCEDURE valid-loc2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-msg AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    IF lv-msg EQ "" THEN
      IF fg-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN
        lv-msg = "To Bin may not be spaces".

    IF lv-msg EQ "" THEN DO:
      FIND FIRST loc
          WHERE loc.company EQ cocode
            AND loc.loc     EQ fg-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name}
          NO-LOCK NO-ERROR.
      IF NOT AVAIL loc THEN lv-msg = "Invalid entry, try help".
    END.

    IF lv-msg NE "" THEN DO:
      MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX.
      APPLY "entry" TO fg-rctd.loc2 IN BROWSE {&browse-name}.
      RETURN ERROR.
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
  
 /* (fg-rctd.cases * fg-rctd.qty-case) + fg-rctd.partial. */
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

  IF lv-fgrecpt-val = 1 THEN DO:
     FIND FIRST loadtag WHERE loadtag.company = gcompany
                          AND loadtag.item-type = NO
                          AND loadtag.tag-no = fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
     IF NOT AVAIL loadtag /*or fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} = ""*/ THEN DO:
        MESSAGE "Invalid Tag#. Try help or Scan valid tag#..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO fg-rctd.tag IN BROWSE {&browse-name}.
        RETURN ERROR.
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

