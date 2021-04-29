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

&SCOPED-DEFINE yellowColumnsName oe-prmtx
&SCOPED-DEFINE winReSize
&SCOPED-DEFINE browseOnly
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{custom/globdefs.i}
{sys/inc/var.i new shared}
ASSIGN
 cocode = g_company
 locode = g_loc.
{sys/inc/varasgn.i}

DEF TEMP-TABLE tt-oe-prmtxx LIKE oe-prmtx
   FIELD valid    AS LOGICAL INIT TRUE
   FIELD row-no   AS INTEGER
   FIELD refcode  AS CHAR.
DEFINE VARIABLE fi_eff-date AS DATE FORMAT "99/99/9999":U INITIAL TODAY .
DEFINE VARIABLE cSortBy             AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSortAsc            AS LOGICAL   NO-UNDO INIT YES.
DEFINE VARIABLE lIsGoButtonPressed  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cScreenType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iRecordLimit        AS INTEGER   NO-UNDO.
DEFINE VARIABLE dQueryTimeLimit     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lEnableShowAll      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lShowAll            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE char-hdl            AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle             AS HANDLE    NO-UNDO.

RUN Browser_GetRecordAndTimeLimit(
  INPUT  cocode,
  INPUT  "OF3",
  OUTPUT iRecordLimit,
  OUTPUT dQueryTimeLimit,
  OUTPUT lEnableShowAll
  ).

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
&Scoped-define INTERNAL-TABLES oe-prmtx

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table oe-prmtx.cust-no ~
oe-prmtx.custype oe-prmtx.custShipID oe-prmtx.i-no oe-prmtx.procat ~
oe-prmtx.eff-date oe-prmtx.exp-date oe-prmtx.price[1] oe-prmtx.price[2] ~
oe-prmtx.price[3] oe-prmtx.price[4] oe-prmtx.price[5] oe-prmtx.price[6] ~
oe-prmtx.price[7] oe-prmtx.price[8] oe-prmtx.price[9] oe-prmtx.price[10] ~
oe-prmtx.online 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH oe-prmtx WHERE ~{&KEY-PHRASE} ~
      AND oe-prmtx.company = cocode ~
AND oe-prmtx.cust-no EQ "zzzzzzzz" NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH oe-prmtx WHERE ~{&KEY-PHRASE} ~
      AND oe-prmtx.company = cocode ~
AND oe-prmtx.cust-no EQ "zzzzzzzz" NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table oe-prmtx
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table oe-prmtx


/* Definitions for FRAME F-Main                                         */
&Scoped-define QUERY-STRING-F-Main FOR EACH oe-prmtx NO-LOCK
&Scoped-define OPEN-QUERY-F-Main OPEN QUERY F-Main FOR EACH oe-prmtx NO-LOCK.
&Scoped-define TABLES-IN-QUERY-F-Main oe-prmtx
&Scoped-define FIRST-TABLE-IN-QUERY-F-Main oe-prmtx


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-4 RECT-5 btGo fiCustomer fiType fiItem ~
fiCategory fiEffDate cbStatus Browser-Table browse-order auto_find ~
Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS fiCustomer fiType fiItem fiCategory ~
fiEffDate cbStatus browse-order fi_sortby auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pfGetWhereConditions B-table-Win 
FUNCTION pfGetWhereConditions RETURNS CHARACTER PRIVATE
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btGo 
     LABEL "Go" 
     SIZE 12 BY 1.14.

DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE BUTTON btSHowAll 
     LABEL "Show All" 
     SIZE 13.6 BY 1.14.

DEFINE VARIABLE cbStatus AS CHARACTER FORMAT "X(256)":U INITIAL "Active" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "All","Inactive","Active" 
     DROP-DOWN-LIST
     SIZE 12.4 BY 1 NO-UNDO.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     LABEL "Auto Find" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE fiCategory AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 22 NO-UNDO.

DEFINE VARIABLE fiCustomer AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 22 NO-UNDO.

DEFINE VARIABLE fiEffDate AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiItem AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 16.8 BY 1
     BGCOLOR 15 FONT 22 NO-UNDO.

DEFINE VARIABLE fiType AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 22 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 58 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 143 BY 2.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      oe-prmtx
    FIELDS(oe-prmtx.cust-no
      oe-prmtx.custype
      oe-prmtx.custShipID
      oe-prmtx.i-no
      oe-prmtx.procat
      oe-prmtx.eff-date
      oe-prmtx.exp-date
      oe-prmtx.price[1]
      oe-prmtx.price[2]
      oe-prmtx.price[3]
      oe-prmtx.price[4]
      oe-prmtx.price[5]
      oe-prmtx.price[6]
      oe-prmtx.price[7]
      oe-prmtx.price[8]
      oe-prmtx.price[9]
      oe-prmtx.price[10]
      oe-prmtx.online) SCROLLING.

DEFINE QUERY F-Main FOR 
      oe-prmtx SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      oe-prmtx.cust-no FORMAT "x(8)":U LABEL-BGCOLOR 14
      oe-prmtx.custype FORMAT "x(8)":U LABEL-BGCOLOR 14
      oe-prmtx.custShipID FORMAT "x(8)":U LABEL-BGCOLOR 14
      oe-prmtx.i-no FORMAT "x(15)":U LABEL-BGCOLOR 14
      oe-prmtx.procat COLUMN-LABEL "Cat" FORMAT "x(5)":U LABEL-BGCOLOR 14
      oe-prmtx.eff-date FORMAT "99/99/9999":U LABEL-BGCOLOR 14
      oe-prmtx.exp-date FORMAT "99/99/9999":U LABEL-BGCOLOR 14
      oe-prmtx.price[1] COLUMN-LABEL "Price01" FORMAT ">>>,>>9.99<<":U
      oe-prmtx.price[2] COLUMN-LABEL "Price02" FORMAT ">>>,>>9.99<<":U
      oe-prmtx.price[3] COLUMN-LABEL "Price03" FORMAT ">>>,>>9.99<<":U
      oe-prmtx.price[4] COLUMN-LABEL "Price04" FORMAT ">>>,>>9.99<<":U
      oe-prmtx.price[5] COLUMN-LABEL "Price05" FORMAT ">>>,>>9.99<<":U
      oe-prmtx.price[6] COLUMN-LABEL "Price06" FORMAT ">>>,>>9.99<<":U
      oe-prmtx.price[7] COLUMN-LABEL "Price07" FORMAT ">>>,>>9.99<<":U
      oe-prmtx.price[8] COLUMN-LABEL "Price08" FORMAT ">>>,>>9.99<<":U
      oe-prmtx.price[9] COLUMN-LABEL "Price09" FORMAT ">>>,>>9.99<<":U
      oe-prmtx.price[10] COLUMN-LABEL "Price10" FORMAT ">>>,>>9.99<<":U
      oe-prmtx.online FORMAT "yes/no":U LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 144 BY 17.67
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btGo AT ROW 1.67 COL 117.4 WIDGET-ID 10
     btSHowAll AT ROW 1.67 COL 129.8 WIDGET-ID 12
     fiCustomer AT ROW 1.81 COL 2.8 NO-LABEL WIDGET-ID 2
     fiType AT ROW 1.81 COL 19 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     fiItem AT ROW 1.81 COL 37.6 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     fiCategory AT ROW 1.81 COL 58.8 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     fiEffDate AT ROW 1.81 COL 78.4 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     cbStatus AT ROW 1.81 COL 99.8 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     Browser-Table AT ROW 3.14 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 19.33 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     fi_sortby AT ROW 19.33 COL 63 COLON-ALIGNED NO-LABEL
     auto_find AT ROW 19.33 COL 100 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 19.33 COL 132 HELP
          "CLEAR AUTO FIND Value"
     "Status" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.19 COL 103 WIDGET-ID 32
          BGCOLOR 23 FGCOLOR 24 FONT 22
     "Customer#" VIEW-AS TEXT
          SIZE 12.2 BY .62 AT ROW 1.19 COL 3.4 WIDGET-ID 16
          BGCOLOR 23 FGCOLOR 24 FONT 22
     "Type" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1.19 COL 24.8 WIDGET-ID 18
          BGCOLOR 23 FGCOLOR 24 FONT 22
     "Item No#" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 1.19 COL 41.8 WIDGET-ID 20
          BGCOLOR 23 FGCOLOR 24 FONT 22
     "Category" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1.19 COL 62 WIDGET-ID 22
          BGCOLOR 23 FGCOLOR 24 FONT 22
     "From Eff. Date" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 1.19 COL 81.2 WIDGET-ID 26
          BGCOLOR 23 FGCOLOR 24 FONT 22
     RECT-4 AT ROW 19.1 COL 1
     RECT-5 AT ROW 1.05 COL 1 WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 
         DEFAULT-BUTTON btGo.


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
         HEIGHT             = 19.91
         WIDTH              = 150.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB Browser-Table cbStatus F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       auto_find:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       browse-order:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       Browser-Table:MAX-DATA-GUESS IN FRAME F-Main         = 100000
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "2"
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

ASSIGN 
       Btn_Clear_Find:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON btSHowAll IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiCustomer IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

ASSIGN 
       RECT-4:HIDDEN IN FRAME F-Main           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.oe-prmtx"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED"
     _Where[1]         = "ASI.oe-prmtx.company = cocode
AND ASI.oe-prmtx.cust-no EQ ""zzzzzzzz"""
     _FldNameList[1]   > ASI.oe-prmtx.cust-no
"oe-prmtx.cust-no" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.oe-prmtx.custype
"oe-prmtx.custype" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.oe-prmtx.custShipID
"oe-prmtx.custShipID" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.oe-prmtx.i-no
"oe-prmtx.i-no" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.oe-prmtx.procat
"oe-prmtx.procat" "Cat" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.oe-prmtx.eff-date
"oe-prmtx.eff-date" ? ? "date" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.oe-prmtx.exp-date
"oe-prmtx.exp-date" ? ? "date" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.oe-prmtx.price[1]
"oe-prmtx.price[1]" "Price01" ">>>,>>9.99<<" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.oe-prmtx.price[2]
"oe-prmtx.price[2]" "Price02" ">>>,>>9.99<<" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.oe-prmtx.price[3]
"oe-prmtx.price[3]" "Price03" ">>>,>>9.99<<" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.oe-prmtx.price[4]
"oe-prmtx.price[4]" "Price04" ">>>,>>9.99<<" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.oe-prmtx.price[5]
"oe-prmtx.price[5]" "Price05" ">>>,>>9.99<<" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.oe-prmtx.price[6]
"oe-prmtx.price[6]" "Price06" ">>>,>>9.99<<" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.oe-prmtx.price[7]
"oe-prmtx.price[7]" "Price07" ">>>,>>9.99<<" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.oe-prmtx.price[8]
"oe-prmtx.price[8]" "Price08" ">>>,>>9.99<<" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.oe-prmtx.price[9]
"oe-prmtx.price[9]" "Price09" ">>>,>>9.99<<" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > ASI.oe-prmtx.price[10]
"oe-prmtx.price[10]" "Price10" ">>>,>>9.99<<" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ASI.oe-prmtx.online
"oe-prmtx.online" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE Browser-Table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _TblList          = "ASI.oe-prmtx"
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
   {methods/template/sortindicator.i}  
    DEFINE VARIABLE hdColumn    AS HANDLE     NO-UNDO.
    DEFINE VARIABLE cColumnName AS CHARACTER  NO-UNDO.

    ASSIGN
        hdColumn     = {&BROWSE-NAME}:CURRENT-COLUMN 
        cColumnName  = hdColumn:NAME
        .
    IF hdColumn:LABEL-BGCOLOR NE 14 THEN 
        RETURN NO-APPLY.
    
    IF cSortBy EQ cColumnName THEN 
        lSortAsc = NOT lSortAsc.
    ELSE
        cSortBy = cColumnName.
        
    APPLY 'END-SEARCH' TO {&BROWSE-NAME}.

    RUN dispatch ("open-query").
    {methods/template/sortindicatorend.i}
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


&Scoped-define SELF-NAME btGo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btGo B-table-Win
ON CHOOSE OF btGo IN FRAME F-Main /* Go */
DO:
    
    ASSIGN 
        fiCustomer
        fiItem
        fiCategory
        fiType
        fiEffDate
        cbStatus
        lIsGoButtonPressed = YES.
   RUN dispatch ("open-query").
   lIsGoButtonPressed = NO.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSHowAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSHowAll B-table-Win
ON CHOOSE OF btSHowAll IN FRAME F-Main /* Show All */
DO:
    lShowAll = YES.
    APPLY "CHOOSE":U TO btGo.
    lShowAll = NO.
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
/* Ticket# : 92946
   Hiding this widget for now, as browser's column label should be indicating the column which is sorted by */
fi_sortby:HIDDEN  = TRUE.
fi_sortby:VISIBLE = FALSE.

IF lEnableShowAll THEN 
    btSHowAll:SENSITIVE = lEnableShowAll.

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
DEFINE VAR CustFrom  AS CHAR NO-UNDO.
DEFINE VAR CustTo   AS CHAR NO-UNDO.
DEFINE VAR ItemFrom AS CHAR NO-UNDO.
DEFINE VAR ItemTo   AS CHAR NO-UNDO.
DEFINE VAR typeFrom AS CHAR NO-UNDO.
DEFINE VAR typeTo   AS CHAR NO-UNDO.
DEFINE VAR catfrom AS CHAR NO-UNDO.
DEFINE VAR catto   AS CHAR NO-UNDO.

    GET FIRST Browser-Table .
    IF AVAIL oe-prmtx THEN
      ASSIGN
        CustFrom     = oe-prmtx.cust-no
        ItemFrom     = oe-prmtx.i-no   
        typeFrom     = oe-prmtx.custype
        catfrom      = oe-prmtx.procat .
    
    GET LAST Browser-Table .
    IF AVAIL oe-prmtx THEN
      ASSIGN
        CustTo       = oe-prmtx.cust-no
        ItemTo       = oe-prmtx.i-no   
        typeTo       = oe-prmtx.custype
        catto        = oe-prmtx.procat .
        
    IF fiCustomer NE "" OR fiItem NE "" OR fiCategory NE "" OR fiType NE ""  THEN
        auto_find = "zzz".
    ELSE
        auto_find = "".
                 

RUN oerep/rd-prmtx.w (CustFrom,CustTo,ItemFrom,ItemTo,typeFrom,typeTo,catfrom,catto,auto_find).


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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .
  RUN pPrepareAndExecuteQuery(
      INPUT lShowAll
      ).
  IF AVAILABLE {&first-table-in-query-{&browse-name}} THEN DO:
        RUN dispatch ("display-fields").
        RUN dispatch ("row-changed"). 
  END.         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrepareAndExecuteQuery B-table-Win 
PROCEDURE pPrepareAndExecuteQuery PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplShowAll AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cBrowseQuery   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResponse      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLimitingQuery AS CHARACTER NO-UNDO.
    
    IF NOT iplShowAll THEN DO:
        cLimitingQuery = " FOR EACH oe-prmtx NO-LOCK" 
                         + " WHERE oe-prmtx.company EQ " + QUOTER(cocode)
                         + pfGetWhereConditions()
                         + " BY rec_key DESC"
                         .
                   
        RUN Browse_PrepareAndExecuteLimitingQuery(
            INPUT  cLimitingQuery,         /* Query */
            INPUT  "oe-prmtx",             /* Buffers Name */
            INPUT  iRecordLimit,           /* Record Limit */
            INPUT  dQueryTimeLimit,        /* Time Limit*/
            INPUT  lEnableShowAll,         /* Enable ShowAll Button? */
            INPUT  "oe-prmtx",             /* Buffer name to fetch the field's value*/
            INPUT  "rec_key",              /* Field Name*/
            INPUT  NOT lIsGoButtonPressed, /* Initial Query*/
            INPUT  NO,                     /* Is breakby used */
            OUTPUT cResponse           
            ). 
        IF cResponse EQ "" THEN DO:
            IF lIsGoButtonPressed THEN
                MESSAGE "No Records Found"
                    VIEW-AS ALERT-BOX ERROR.
            RETURN.                            
        END.           
     END.       
     cBrowseQuery = " FOR EACH oe-prmtx NO-LOCK" 
                   + " WHERE oe-prmtx.company EQ " + QUOTER(cocode)
                   + " AND oe-prmtx.rec_key GE "   + QUOTER(cResponse)   
                   + pfGetWhereConditions() 
                   + (IF cSortBY NE "" THEN " BY oe-prmtx." + cSortBY + (IF NOT lSortAsc THEN " DESC" ELSE "")  ELSE "")
                   .   
               
        RUN Browse_PrepareAndExecuteBrowseQuery(
            INPUT  BROWSE {&BROWSE-NAME}:QUERY, /* Browse Query Handle */
            INPUT  cBrowseQuery,                /* BRowse Query */
            INPUT  NO,                          /* Show limit alert? */
            INPUT  0,                           /* Record limit */
            INPUT  0,                           /* Time Limit */
            INPUT  lEnableShowAll,              /* Enable ShowAll Button */
            OUTPUT cResponse
            ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE one-row-query B-table-Win 
PROCEDURE one-row-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

    DEFINE VARIABLE cQuery    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResponse AS CHARACTER NO-UNDO.        
    
    cQuery = "FOR EACH oe-prmtx NO-LOCK" 
             + " WHERE oe-prmtx.company EQ " + QUOTER(cocode)
             + " AND ROWID(oe-prmtx)    EQ " + "TO-ROWID(" + "'" + STRING(ip-rowid) + "')"                     
             + pfGetWhereConditions() 
             + (IF cSortBY NE "" THEN " BY oe-prmtx." + cSortBY + (IF NOT lSortAsc THEN " DESC" ELSE "")  ELSE "")
                   .             
                 
    RUN Browse_PrepareAndExecuteBrowseQuery(
        INPUT  BROWSE {&BROWSE-NAME}:QUERY, /* Browse Query Handle */      
        INPUT  cQuery,                      /* BRowse Query */             
        INPUT  NO,                          /* Show limit alert? */        
        INPUT  0,                           /* Record limit */             
        INPUT  0,                           /* Time Limit */               
        INPUT  lEnableShowAll,              /* Enable ShowAll Button */    
        OUTPUT cResponse
        ).               
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
  DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO. 
                               
  DO WITH FRAME {&FRAME-NAME}: 
      ASSIGN
          cbStatus:SCREEN-VALUE = "Active"
          cbStatus = "Active".
      RUN one-row-query (ip-rowid).
      REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.    
    IF AVAILABLE {&first-table-in-query-{&browse-name}} THEN DO:
        RUN dispatch ("display-fields").
        RUN dispatch ("row-changed"). 
    END. 
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
  {src/adm/template/snd-list.i "oe-prmtx"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus B-table-Win
PROCEDURE Set-Focus PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: To avoid error message when moving to new row in browse
 Notes:
------------------------------------------------------------------------------*/


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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pfGetWhereConditions B-table-Win 
FUNCTION pfGetWhereConditions RETURNS CHARACTER PRIVATE
  (  ):
/*------------------------------------------------------------------------------
 Purpose: Returns the where clause conditions
 Notes:
------------------------------------------------------------------------------*/
    RETURN (  IF fiCustomer NE "" THEN " AND oe-prmtx.cust-no  BEGINS " + QUOTER(fiCustomer) ELSE "")
           + (IF fiType     NE "" THEN " AND oe-prmtx.custype  BEGINS " + QUOTER(fiType)     ELSE "")
           + (IF fiItem     NE "" THEN " AND oe-prmtx.i-no     BEGINS " + QUOTER(fiItem)     ELSE "")
           + (IF fiCategory NE "" THEN " AND oe-prmtx.procat   BEGINS " + QUOTER(fiCategory) ELSE "")
           + (IF fiEffDate  NE ?  THEN " AND oe-prmtx.eff-date GE "     + STRING(fiEffDate)  ELSE "")
           + (IF cbStatus   EQ "Active"   THEN " AND (oe-prmtx.exp-date GT TODAY OR oe-prmtx.exp-date EQ ?)"
           ELSE IF cbStatus EQ "Inactive" THEN " AND oe-prmtx.exp-date  LE TODAY"
           ELSE "")
              .
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

