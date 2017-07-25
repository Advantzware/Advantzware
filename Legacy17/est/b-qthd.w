&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admBrowserUsing.i} /* added by script _admBrowsers.p */

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

&SCOPED-DEFINE dataGridInclude dataGrid\est\b-qthd.i
&SCOPED-DEFINE yellowColumnsName b-qthd
&SCOPED-DEFINE localOpenQuery
&SCOPED-DEFINE SORTBY-PHRASE BY STRING(quotehd.q-no,">>>>>9") DESCENDING
&SCOPED-DEFINE winReSize
&SCOPED-DEFINE browseOnly
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/var.i new shared}
{sys/inc/varasgn.i}

DEF VAR auto_date AS DATE NO-UNDO.
DEF VAR qRowID AS ROWID NO-UNDO.
DEF VAR changeValue AS INTEGER NO-UNDO.

/* &SCOPED-DEFINE browse2 est/j-qthd.i */
&SCOPED-DEFINE brwsdefs b-qthd

DEFINE VARIABLE q-noValue AS INTEGER NO-UNDO EXTENT 2 INITIAL 999999.
DEFINE VARIABLE recCount AS INTEGER NO-UNDO.
DEFINE BUFFER bQuotehd FOR quotehd.
DEFINE BUFFER bQuoteitm FOR quoteitm.
DEF VAR lActive AS LOG NO-UNDO.

DO TRANSACTION:
  {sys/inc/custlistform.i ""EQ"" }
  {sys/ref/CustList.i NEW}
  {sys/inc/browser.i "CEBROWSE"}
END.
recCount = sys-ctrl.int-fld.

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
&Scoped-define INTERNAL-TABLES quotehd quoteitm

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table quotehd.q-no quotehd.quo-date ~
quotehd.cust-no quotehd.contact quotehd.est-no quotehd.rfq quoteitm.part-no ~
quoteitm.part-dscr1 quotehd.upd-date ~
STRING (quotehd.upd-time,'HH:MM:SS') @ quotehd.upd-time quotehd.upd-user 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH quotehd ~
      WHERE quotehd.company EQ g_company ~
AND quotehd.loc EQ g_loc ~
AND quotehd.q-no GE q-noValue[1] ~
AND quotehd.q-no LE q-noValue[2] ~
AND quotehd.cust-no BEGINS fi_cust-no ~
AND ( (lookup(quotehd.cust-no,custcount) <> 0 AND quotehd.cust-no <> "" ) OR custcount = "") ~
AND (IF fi_contact BEGINS "*" THEN quotehd.contact MATCHES fi_contact ~
    ELSE quotehd.contact BEGINS fi_contact) ~
AND (quotehd.quo-date GE fi_quo-date ~
OR fi_quo-date EQ ?) ~
AND (quotehd.est-no EQ fi_est-no ~
OR fi_est-no EQ "") ~
AND quotehd.rfq BEGINS fi_rfq ~
~{&useIndexPhrase} NO-LOCK, ~
      EACH quoteitm OF quotehd OUTER-JOIN  ~
      WHERE (IF fi_part-no BEGINS "*" THEN quoteitm.part-no MATCHES fi_part-no  ~
          ELSE quoteitm.part-no BEGINS fi_part-no) ~
and (IF fi_item-decr BEGINS "*" THEN quoteitm.part-dscr1 MATCHES fi_item-decr  ~
    ELSE quoteitm.part-dscr1 BEGINS fi_item-decr)  NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH quotehd ~
      WHERE quotehd.company EQ g_company ~
AND quotehd.loc EQ g_loc ~
AND quotehd.q-no GE q-noValue[1] ~
AND quotehd.q-no LE q-noValue[2] ~
AND quotehd.cust-no BEGINS fi_cust-no ~
AND ( (lookup(quotehd.cust-no,custcount) <> 0 AND quotehd.cust-no <> "" ) OR custcount = "") ~
AND (IF fi_contact BEGINS "*" THEN quotehd.contact MATCHES fi_contact ~
    ELSE quotehd.contact BEGINS fi_contact) ~
AND (quotehd.quo-date GE fi_quo-date ~
OR fi_quo-date EQ ?) ~
AND (quotehd.est-no EQ fi_est-no ~
OR fi_est-no EQ "") ~
AND quotehd.rfq BEGINS fi_rfq ~
~{&useIndexPhrase} NO-LOCK, ~
      EACH quoteitm OF quotehd OUTER-JOIN ~
      WHERE (IF fi_part-no BEGINS "*" THEN quoteitm.part-no MATCHES fi_part-no  ~
          ELSE quoteitm.part-no BEGINS fi_part-no) ~
and (IF fi_item-decr BEGINS "*" THEN quoteitm.part-dscr1 MATCHES fi_item-decr  ~
    ELSE quoteitm.part-dscr1 BEGINS fi_item-decr)  NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table quotehd quoteitm
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table quotehd
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table quoteitm


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi_q-no fi_quo-date fi_cust-no fi_contact ~
fi_est-no fi_rfq fi_part-no fi_item-decr btnGO btnShowPrevious btnShowNext ~
Browser-Table 
&Scoped-Define DISPLAYED-OBJECTS fi_q-no fi_quo-date fi_cust-no fi_contact ~
fi_est-no fi_rfq fi_part-no fi_item-decr fi_sortby browse-order auto_find 

/* Custom List Definitions                                              */
/* filterFields,List-2,List-3,List-4,List-5,List-6                      */
&Scoped-define filterFields fi_q-no fi_quo-date fi_cust-no fi_contact ~
fi_est-no fi_rfq fi_part-no fi_item-decr 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnGO 
     LABEL "&Go" 
     SIZE 11 BY 1
     FONT 6.

DEFINE BUTTON btnShowNext 
     LABEL "Show &Next" 
     SIZE 20 BY 1
     FONT 6.

DEFINE BUTTON btnShowPrevious 
     LABEL "Show &Previous" 
     SIZE 20 BY 1
     FONT 6.

DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     LABEL "Auto Find" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE fi_contact AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 25.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_cust-no AS CHARACTER FORMAT "X(8)" 
     VIEW-AS FILL-IN 
     SIZE 11.8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_est-no AS CHARACTER FORMAT "X(8)" 
     VIEW-AS FILL-IN 
     SIZE 11.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_item-decr AS CHARACTER FORMAT "X(20)" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_part-no AS CHARACTER FORMAT "X(20)" 
     VIEW-AS FILL-IN 
     SIZE 21.8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_q-no AS INTEGER FORMAT ">>>>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_quo-date AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 12.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_rfq AS CHARACTER FORMAT "X(10)" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sorted By" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 73 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      quotehd
    FIELDS(quotehd.q-no
      quotehd.quo-date
      quotehd.cust-no
      quotehd.contact
      quotehd.est-no
      quotehd.rfq
      quotehd.upd-date
      quotehd.upd-time
      quotehd.upd-time
      quotehd.upd-user), 
      quoteitm
    FIELDS(quoteitm.part-no
      quoteitm.part-dscr1) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      quotehd.q-no COLUMN-LABEL "Quote" FORMAT ">>>>>9":U WIDTH 9
            LABEL-BGCOLOR 14
      quotehd.quo-date COLUMN-LABEL "Date" FORMAT "99/99/99":U
            LABEL-BGCOLOR 14
      quotehd.cust-no COLUMN-LABEL "Cust" FORMAT "x(8)":U LABEL-BGCOLOR 14
      quotehd.contact FORMAT "X(15)":U LABEL-BGCOLOR 14
      quotehd.est-no COLUMN-LABEL "Estimate" FORMAT "x(8)":U WIDTH 12
            LABEL-BGCOLOR 14
      quotehd.rfq FORMAT "x(10)":U LABEL-BGCOLOR 14
      quoteitm.part-no COLUMN-LABEL "Cust Part" FORMAT "x(20)":U
            LABEL-BGCOLOR 14
      quoteitm.part-dscr1 FORMAT "x(30)":U LABEL-BGCOLOR 14
      quotehd.upd-date FORMAT "99/99/9999":U LABEL-BGCOLOR 14
      STRING (quotehd.upd-time,'HH:MM:SS') @ quotehd.upd-time COLUMN-LABEL "Updated Time"
            WIDTH 17.4
      quotehd.upd-user FORMAT "X(8)":U LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 138 BY 16.43
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi_q-no AT ROW 1.71 COL 1.4 NO-LABEL
     fi_quo-date AT ROW 1.71 COL 11.8 NO-LABEL
     fi_cust-no AT ROW 1.71 COL 24.4 NO-LABEL
     fi_contact AT ROW 1.71 COL 36 NO-LABEL
     fi_est-no AT ROW 1.71 COL 61.4 NO-LABEL
     fi_rfq AT ROW 1.71 COL 72.6 NO-LABEL
     fi_part-no AT ROW 1.71 COL 85.4 NO-LABEL
     fi_item-decr AT ROW 1.71 COL 107 NO-LABEL
     btnGO AT ROW 2.91 COL 2
     btnShowPrevious AT ROW 2.91 COL 14
     btnShowNext AT ROW 2.91 COL 35
     fi_sortby AT ROW 2.91 COL 89 COLON-ALIGNED
     Browser-Table AT ROW 4.1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 19.33 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 19.33 COL 89 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 19.33 COL 126 HELP
          "CLEAR AUTO FIND Value"
     "Estimate" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 1 COL 61.8
          FGCOLOR 9 FONT 6
     "Date" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1 COL 12.2
          FGCOLOR 9 FONT 6
     "Contact" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 1 COL 36.4
          FGCOLOR 9 FONT 6
     "Customer Part No" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 1 COL 85.8
          FGCOLOR 9 FONT 6
     "Customer" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1 COL 24.4
          FGCOLOR 9 FONT 6
     "Item Description" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 1 COL 108
          FGCOLOR 9 FONT 6
     "Quote" VIEW-AS TEXT
          SIZE 7.6 BY .62 AT ROW 1 COL 1.4
          FGCOLOR 9 FONT 6
     "RFQ" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 1 COL 73
          FGCOLOR 9 FONT 6
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
         HEIGHT             = 19.52
         WIDTH              = 138.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
{methods/template/browser.i}
{custom/yellowColumns.i}

{Advantzware/WinKit/dataGridProc.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB Browser-Table fi_sortby F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN auto_find IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       auto_find:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR RADIO-SET browse-order IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       browse-order:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "2"
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR BUTTON Btn_Clear_Find IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Btn_Clear_Find:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fi_contact IN FRAME F-Main
   ALIGN-L 1                                                            */
/* SETTINGS FOR FILL-IN fi_cust-no IN FRAME F-Main
   ALIGN-L 1                                                            */
/* SETTINGS FOR FILL-IN fi_est-no IN FRAME F-Main
   ALIGN-L 1                                                            */
/* SETTINGS FOR FILL-IN fi_item-decr IN FRAME F-Main
   ALIGN-L 1                                                            */
/* SETTINGS FOR FILL-IN fi_part-no IN FRAME F-Main
   ALIGN-L 1                                                            */
/* SETTINGS FOR FILL-IN fi_q-no IN FRAME F-Main
   ALIGN-L 1                                                            */
/* SETTINGS FOR FILL-IN fi_quo-date IN FRAME F-Main
   ALIGN-L 1                                                            */
/* SETTINGS FOR FILL-IN fi_rfq IN FRAME F-Main
   ALIGN-L 1                                                            */
/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.quotehd,ASI.quoteitm OF ASI.quotehd "
     _Options          = "NO-LOCK SORTBY-PHRASE"
     _TblOptList       = "USED, OUTER USED"
     _OrdList          = "ASI.quotehd.q-no|no"
     _Where[1]         = "quotehd.company EQ g_company
AND quotehd.loc EQ g_loc
AND quotehd.q-no GE q-noValue[1]
AND quotehd.q-no LE q-noValue[2]
AND quotehd.cust-no BEGINS fi_cust-no
AND ( (lookup(quotehd.cust-no,custcount) <> 0 AND quotehd.cust-no <> "") OR custcount = "") 
AND (IF fi_contact BEGINS "*" THEN quotehd.contact MATCHES fi_contact
    ELSE quotehd.contact BEGINS fi_contact)
AND (quotehd.quo-date GE fi_quo-date
OR fi_quo-date EQ ?)
AND (quotehd.est-no EQ fi_est-no
OR fi_est-no EQ "")
AND quotehd.rfq BEGINS fi_rfq
~{&useIndexPhrase}"
     _Where[2]         = "(IF fi_part-no BEGINS "*" THEN quoteitm.part-no MATCHES fi_part-no
ELSE quoteitm.part-no BEGINS fi_part-no)
AND (IF fi_item-decr BEGINS "*" THEN quoteitm.part-dscr1 MATCHES fi_item-decr
ELSE quoteitm.part-dscr1 BEGINS fi_item-decr)"
     _FldNameList[1]   > ASI.quotehd.q-no
"quotehd.q-no" "Quote" ">>>>>9" "integer" ? ? ? 14 ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.quotehd.quo-date
"quotehd.quo-date" "Date" "99/99/99" "date" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.quotehd.cust-no
"quotehd.cust-no" "Cust" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.quotehd.contact
"quotehd.contact" ? "X(15)" "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.quotehd.est-no
"quotehd.est-no" "Estimate" "x(8)" "character" ? ? ? 14 ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.quotehd.rfq
"quotehd.rfq" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.quoteitm.part-no
"quoteitm.part-no" "Cust Part" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.quoteitm.part-dscr1
"quoteitm.part-dscr1" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.quotehd.upd-date
"quotehd.upd-date" ? ? "date" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"STRING (quotehd.upd-time,'HH:MM:SS') @ quotehd.upd-time" "Updated Time" ? ? ? ? ? ? ? ? no ? no no "17.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.quotehd.upd-user
"quotehd.upd-user" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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

  changeValue = IF AVAILABLE quotehd THEN quotehd.q-no ELSE 0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGO B-table-Win
ON CHOOSE OF btnGO IN FRAME F-Main /* Go */
DO:
  APPLY 'RETURN':U TO fi_q-no.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnShowNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnShowNext B-table-Win
ON CHOOSE OF btnShowNext IN FRAME F-Main /* Show Next */
DO:
  RUN getValueFields ('Next').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnShowPrevious
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnShowPrevious B-table-Win
ON CHOOSE OF btnShowPrevious IN FRAME F-Main /* Show Previous */
DO:
  RUN getValueFields ('Previous').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cust-no B-table-Win
ON HELP OF fi_cust-no IN FRAME F-Main
DO:
   DEF VAR char-val AS cha NO-UNDO.
   RUN windows/l-cust2.w (INPUT g_company, INPUT focus:screen-value,"", OUTPUT char-val).
          if char-val <> "" then FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
          return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME fi_part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_part-no B-table-Win
ON HELP OF fi_part-no IN FRAME F-Main
DO: 
   DEF VAR char-val AS cha NO-UNDO.
   DEF VAR char-val-rec AS RECID NO-UNDO.
   RUN windows/l-cstprt.w (INPUT g_company,"", INPUT focus:screen-value,"", OUTPUT char-val, OUTPUT char-val-rec).
        if char-val <> "" then FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
          return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_est-no B-table-Win
ON HELP OF fi_est-no IN FRAME F-Main
DO:
   DEF VAR char-val AS cha NO-UNDO.
   DEF VAR char-val-rec AS RECID NO-UNDO.
   RUN windows/l-est2.w (INPUT g_company,"", INPUT focus:screen-value, OUTPUT char-val).
        if char-val ne "" then 
            find eb where recid(eb) eq int(entry(1,char-val)) no-lock no-error.

           if avail eb then do:
            assign
             FOCUS:SCREEN-VALUE          = eb.est-no   .
           END.
          return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_q-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_q-no B-table-Win
ON RETURN OF fi_q-no IN FRAME F-Main
,fi_quo-date,fi_cust-no,fi_contact,fi_est-no,fi_rfq,fi_part-no,fi_item-decr
DO:
  ASSIGN {&filterFields}
    q-noValue[1] = IF fi_q-no NE 0 THEN fi_q-no ELSE 0
    q-noValue[2] = IF fi_q-no NE 0 THEN fi_q-no ELSE 999999.
  IF fi_est-no NE '' THEN
  fi_est-no = FILL(' ',8 - LENGTH(TRIM(fi_est-no))) + TRIM(fi_est-no).
  RUN openQuery.
  APPLY 'VALUE-CHANGED':U TO BROWSE {&BROWSE-NAME}.
  APPLY 'ENTRY':U TO BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

sortColumn = 'Quote':U.

RUN sys/ref/CustList.p (INPUT cocode,
                            INPUT 'EQ',
                            INPUT YES,
                            OUTPUT lActive).
{sys/inc/chblankcust.i ""EQ""}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changeValue B-table-Win 
PROCEDURE changeValue :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {smartobj/changeValue.i quotehd.q-no}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-cust-rowid B-table-Win 
PROCEDURE get-cust-rowid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF OUTPUT PARAM op-cust-rowid AS ROWID .

 FIND FIRST cust WHERE cust.company = quotehd.company
                   AND cust.cust-no = quotehd.cust-no NO-LOCK NO-ERROR.
 op-cust-rowid = IF AVAIL cust THEN ROWID(cust) ELSE ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getValueFields B-table-Win 
PROCEDURE getValueFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipShow AS CHARACTER NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  IF q-noValue[1] EQ 0 AND q-noValue[2] EQ 999999 THEN
  ASSIGN
    q-noValue[1] = 999999
    q-noValue[2] = 0.

  IF ipShow EQ 'Next':U THEN DO:
    q-noValue[1] = q-noValue[2].
    FOR EACH bQuotehd NO-LOCK WHERE bQuotehd.company EQ g_company
                                AND bQuotehd.loc EQ g_loc
                                AND ((lookup(quotehd.cust-no,custcount) <> 0 AND quotehd.cust-no <> "" ) OR custcount = "") 
                                AND bQuotehd.q-no GE q-noValue[1]
                              BY bQuotehd.q-no:
      ASSIGN
        i = i + 1
        q-noValue[2] = bQuotehd.q-no.
      IF i GE recCount THEN LEAVE.
    END. /* each bquotehd */
  END. /* next */
  ELSE DO:
    q-noValue[2] = q-noValue[1].
    FIND LAST bQuotehd NO-LOCK WHERE bQuotehd.company EQ g_company
                                 AND bQuotehd.loc EQ g_loc
                                 AND bQuotehd.cust-no <> ""
                                 AND ( (lookup(bQuotehd.cust-no,custcount) <> 0 ) OR custcount = "") 
                                 AND bQuotehd.q-no LE q-noValue[2] NO-ERROR.

    DO WHILE AVAILABLE(bQuotehd):
      ASSIGN
        i = i + 1
        q-noValue[1] = bQuotehd.q-no.
      IF i GE recCount THEN LEAVE.
      FIND PREV bQuotehd NO-LOCK WHERE bQuotehd.company EQ g_company
                                   AND ( (lookup(bQuotehd.cust-no,custcount) <> 0 AND bQuotehd.cust-no <> "" ) OR custcount = "")
                                   AND bQuotehd.loc EQ g_loc
                                   AND bQuotehd.q-no LE q-noValue[2] NO-ERROR.
    END. /* do while */
  END. /* else (previous) */
  ASSIGN
    fi_q-no = 0
    fi_quo-date = ?
    fi_cust-no = ''
    fi_contact = ''
    fi_est-no = ''
    fi_rfq = ''
    fi_part-no = ''
    fi_item-decr = ''.
  DISPLAY {&filterFields} WITH FRAME {&FRAME-NAME}.

  RUN openQuery.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initValueFields B-table-Win 
PROCEDURE initValueFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND LAST bQuotehd NO-LOCK WHERE bQuotehd.company EQ g_company
                               AND bQuotehd.loc EQ g_loc NO-ERROR.
  IF NOT AVAILABLE bQuotehd THEN RETURN.
  /*q-noValue[1] = bQuotehd.q-no.*/   /* Task 12051304  */
  RUN getValueFields ('Previous':U).

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
 APPLY "value-changed" TO {&browse-name} IN FRAME {&FRAME-NAME} .


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
  IF colLabels EQ '' THEN DO WITH FRAME {&FRAME-NAME}:
    RUN initValueFields.
    RUN getColLabels.
    RUN openQuery.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openQuery B-table-Win 
PROCEDURE openQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-cust-no AS CHAR NO-UNDO .
DEF BUFFER bf-quotehd  FOR quotehd .
DEF BUFFER bf-quoteitm FOR quoteitm .

&SCOPED-DEFINE SORTBY-PHRASE BY ~
IF sortColumn EQ 'Date' THEN STRING(YEAR(quotehd.quo-date),"9999") + ~
                             STRING(MONTH(quotehd.quo-date),"99") + ~
                             STRING(DAY(quotehd.quo-date),"99")  ELSE ~
IF sortColumn EQ 'Cust'             THEN quotehd.cust-no    ELSE ~
IF sortColumn EQ 'Contact'          THEN quotehd.contact    ELSE ~
IF sortColumn EQ 'Estimate'         THEN quotehd.est-no     ELSE ~
IF sortColumn EQ 'RFQ'              THEN quotehd.rfq        ELSE ~
IF sortColumn EQ 'Cust Part'        THEN quoteitm.part-no   ELSE ~
IF sortColumn EQ 'Item Description' THEN quoteitm.part-dscr1 ELSE ~
IF sortColumn EQ 'Updated Date'     THEN STRING(YEAR(quotehd.upd-date),"9999") + STRING(MONTH(quotehd.upd-date),"99") + STRING(DAY(quotehd.upd-date),"99")   ELSE ~
IF sortColumn EQ 'Updated User'     THEN quotehd.upd-user   ELSE ~
STRING(quotehd.q-no,'>>>>>9') ~{&SORTED}

&SCOPED-DEFINE openQuery ~
  OPEN QUERY {&BROWSE-NAME} ~
    FOR EACH quotehd NO-LOCK ~
        WHERE quotehd.company EQ g_company ~
          AND quotehd.loc EQ g_loc ~
          AND quotehd.q-no GE q-noValue[1] ~
          AND quotehd.q-no LE q-noValue[2] ~
          AND quotehd.cust-no BEGINS fi_cust-no ~
          AND ( (lookup(quotehd.cust-no,custcount) <> 0 AND quotehd.cust-no <> "" ) OR custcount = "") ~
          AND (IF fi_contact BEGINS '*' THEN quotehd.contact MATCHES fi_contact ~
              ELSE quotehd.contact MATCHES fi_contact ) ~
          AND (quotehd.quo-date GE fi_quo-date OR fi_quo-date EQ ?) ~
          AND (quotehd.est-no EQ fi_est-no OR fi_est-no EQ '') ~
          AND quotehd.rfq BEGINS fi_rfq ~{&useIndexPhrase}, ~
        EACH quoteitm OF quotehd NO-LOCK ~
        WHERE (IF fi_part-no BEGINS '*' THEN quoteitm.part-no MATCHES fi_part-no ~
            ELSE quoteitm.part-no BEGINS fi_part-no) ~
            AND (IF fi_item-decr BEGINS '*' THEN quoteitm.part-dscr1 MATCHES fi_item-decr ~
                ELSE quoteitm.part-dscr1 BEGINS fi_item-decr)  {&SORTBY-PHRASE}.

  IF sortBy THEN DO:
    /*IF fi_part-no EQ '' AND
       fi_item-decr EQ '' THEN*/  /*task 12051306 */
    {&OPEN-QUERY-{&BROWSE-NAME}}
    /*ELSE
    {&openQuery}*/
  END. /* if sortby */
  ELSE DO:
    &SCOPED-DEFINE SORTED DESCENDING
    /*IF fi_part-no EQ '' AND
       fi_item-decr EQ '' THEN*/        /*task 12051306 */
    {&OPEN-QUERY-{&BROWSE-NAME}}
    /*ELSE
    {&openQuery}*/
  END. /* else */
  APPLY 'VALUE-CHANGED':U TO BROWSE {&BROWSE-NAME}.

  GET FIRST Browser-Table .
  IF NOT AVAIL quotehd THEN do:

        IF fi_cust-no <> "" THEN DO:
            v-cust-no = fi_cust-no .
        END.
        ELSE IF (fi_part-no <> "" OR fi_item-decr <> "") AND fi_q-no = 0 THEN DO:
             FIND FIRST bf-quoteitm WHERE bf-quoteitm.company = cocode
                 AND (bf-quoteitm.part-no BEGINS fi_part-no OR fi_part-no = "")
                 AND (bf-quoteitm.part-dscr1 BEGINS fi_item-decr OR fi_item-decr = "") NO-LOCK NO-ERROR.
             IF AVAIL bf-quoteitm THEN
             FIND FIRST bf-quotehd WHERE bf-quotehd.company = cocode 
                 AND bf-quotehd.q-no = bf-quoteitm.q-no NO-LOCK NO-ERROR. 
             IF AVAIL bf-quotehd THEN
             v-cust-no = bf-quotehd.cust-no  .
         END.
         ELSE do:
            FIND FIRST bf-quotehd WHERE bf-quotehd.company = cocode 
                 AND (bf-quotehd.cust-no BEGINS fi_cust-no OR fi_cust-no = "")
                 AND (bf-quotehd.q-no = fi_q-no OR fi_q-no = 0)
                 AND (bf-quotehd.contact BEGINS fi_contact OR fi_contact = "")
                 AND (bf-quotehd.est-no BEGINS fi_est-no OR fi_est-no = "") 
                 AND (bf-quotehd.rfq BEGINS fi_rfq OR fi_rfq = "") NO-LOCK NO-ERROR. 
            IF AVAIL bf-quotehd THEN
                v-cust-no = bf-quotehd.cust-no .
            ELSE v-cust-no = "".
         END.                   

         FIND FIRST cust WHERE cust.company = cocode 
             AND cust.cust-no = v-cust-no NO-LOCK NO-ERROR.
         IF AVAIL cust AND ou-log AND LOOKUP(cust.cust-no,custcount) = 0 THEN
             MESSAGE "Customer is not on Users Customer List.  "  SKIP
              "Please add customer to Network Admin - Users Customer List."  VIEW-AS ALERT-BOX WARNING BUTTONS OK.
         ELSE
         MESSAGE "No Quote Found, please update your Search Criteria."
                VIEW-AS ALERT-BOX ERROR.
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
    RUN dispatch ("open-query").
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
    APPLY "value-changed" TO {&browse-name}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resetQuery B-table-Win 
PROCEDURE resetQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipQNo AS INTEGER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    RUN dispatch ('open-query':U).
    ASSIGN
      sortBy = NO
      sortColumn = 'Quote':U
      q-noValue[1] = ipQNo.
    RUN setEstNo ('Previous':U).
  END. /* do with */

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
  {src/adm/template/snd-list.i "quotehd"}
  {src/adm/template/snd-list.i "quoteitm"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setEstNo B-table-Win 
PROCEDURE setEstNo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipShow AS CHARACTER NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DEFINE BUFFER bQuotehd FOR quotehd.

  CASE ipShow:
    WHEN 'Next' THEN DO:
      q-noValue[1] = q-noValue[2].
      FOR EACH bQuotehd NO-LOCK WHERE bQuotehd.company EQ g_company
                                  AND bQuotehd.loc EQ g_loc
                                  AND ((lookup(bQuotehd.cust-no,custcount)  <> 0 AND bQuotehd.cust-no <> "") OR custcount = "")
                                  AND bQuotehd.q-no GE q-noValue[1]
                                BY bQuotehd.q-no:
        i = i + 1.
        IF i GE recCount THEN LEAVE.
      END. /* each bQuotehd */
      IF AVAIL bQuotehd THEN q-noValue[2] = bQuotehd.q-no.
    END. /* next */
    WHEN 'Previous' THEN DO:
      q-noValue[2] = q-noValue[1].
      FOR EACH bQuotehd NO-LOCK WHERE bQuotehd.company EQ g_company
                                  AND bQuotehd.loc EQ g_loc
                                  AND ((lookup(bQuotehd.cust-no,custcount) <> 0 AND bQuotehd.cust-no <> "") OR custcount = "")
                                  AND bQuotehd.q-no LE q-noValue[2]
                                BY bQuotehd.q-no DESCENDING:
        i = i + 1.
        IF i GE recCount THEN LEAVE.
      END. /* each bQuotehd */
      IF AVAIL bQuotehd THEN q-noValue[1] = bQuotehd.q-no.
    END. /* previous */
  END CASE.
  RUN openQuery.

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

