&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  browsers/account.w

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

&SCOPED-DEFINE winReSize
&SCOPED-DEFINE sizeOption HEIGHT
&SCOPED-DEFINE useMatches
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{custom/gcompany.i}
{custom/globdefs.i}
{sys/inc/var.i new shared}
{sys/inc/varasgn.i}
{methods/defines/sortByDefs.i}

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
&Scoped-define INTERNAL-TABLES account

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table account.actnum account.dscr ~
account.type account.inactive account.salesReport account.commReport 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH account WHERE ~{&KEY-PHRASE} ~
      AND account.company EQ gcompany AND ~
account.actnum MATCHES cAccount AND ~
account.dscr MATCHES cDescription AND ~
account.type MATCHES cType AND ~
(account.inactive EQ lInactive OR ~
lInactive EQ NO) AND ~
(account.salesReport EQ lSalesReport OR ~
lSalesReport EQ NO) AND ~
(account.commReport EQ lCommReport OR ~
lCommReport EQ NO) NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH account WHERE ~{&KEY-PHRASE} ~
      AND account.company EQ gcompany AND ~
account.actnum MATCHES cAccount AND ~
account.dscr MATCHES cDescription AND ~
account.type MATCHES cType AND ~
(account.inactive EQ lInactive OR ~
lInactive EQ NO) AND ~
(account.salesReport EQ lSalesReport OR ~
lSalesReport EQ NO) AND ~
(account.commReport EQ lCommReport OR ~
lCommReport EQ NO) NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table account
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table account


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cAccount cDescription cType lCommReport ~
lSalesReport lInactive btnGO btnShowAll Browser-Table browse-order 
&Scoped-Define DISPLAYED-OBJECTS cAccount cDescription cType lCommReport ~
lSalesReport lInactive browse-order auto_find 

/* Custom List Definitions                                              */
/* FilterFields,List-2,List-3,List-4,List-5,List-6                      */
&Scoped-define FilterFields cAccount cDescription cType lCommReport ~
lSalesReport lInactive 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnGO 
     LABEL "&GO" 
     SIZE 9 BY .81
     FONT 6.

DEFINE BUTTON btnShowAll 
     LABEL "&Show All" 
     SIZE 14 BY .81
     FONT 6.

DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     LABEL "Auto Find" 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cAccount AS CHARACTER FORMAT "X(256)":U 
     LABEL "Account" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cDescription AS CHARACTER FORMAT "X(256)":U 
     LABEL "Description" 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Type" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 51 BY .71
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 77 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 143 BY 1.43.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 143 BY 2.38.

DEFINE VARIABLE lCommReport AS LOGICAL INITIAL no 
     LABEL "Commission Report" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE lInactive AS LOGICAL INITIAL no 
     LABEL "Inactive" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE lSalesReport AS LOGICAL INITIAL no 
     LABEL "Sales Report" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      account
    FIELDS(account.actnum
      account.dscr
      account.type
      account.inactive
      account.salesReport
      account.commReport) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      account.actnum FORMAT "x(25)":U LABEL-BGCOLOR 14
      account.dscr COLUMN-LABEL "Description" FORMAT "x(45)":U
            LABEL-BGCOLOR 14
      account.type FORMAT "X":U LABEL-BGCOLOR 14
      account.inactive FORMAT "yes/no":U LABEL-BGCOLOR 14 VIEW-AS TOGGLE-BOX
      account.salesReport COLUMN-LABEL "Sales" FORMAT "yes/no":U
            LABEL-BGCOLOR 14 VIEW-AS TOGGLE-BOX
      account.commReport COLUMN-LABEL "Commission" FORMAT "yes/no":U
            LABEL-BGCOLOR 14 VIEW-AS TOGGLE-BOX
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 143 BY 18.33
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     cAccount AT ROW 1.24 COL 9 COLON-ALIGNED WIDGET-ID 6
     cDescription AT ROW 1.24 COL 48 COLON-ALIGNED WIDGET-ID 8
     cType AT ROW 1.24 COL 106 COLON-ALIGNED WIDGET-ID 10
     lCommReport AT ROW 1.24 COL 116 WIDGET-ID 16
     lSalesReport AT ROW 2.19 COL 116 WIDGET-ID 14
     lInactive AT ROW 2.19 COL 132 WIDGET-ID 12
     btnGO AT ROW 2.43 COL 11 WIDGET-ID 18
     btnShowAll AT ROW 2.43 COL 23 WIDGET-ID 20
     fi_sortby AT ROW 2.43 COL 48 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     Browser-Table AT ROW 3.38 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 20.52 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 20.52 COL 93 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 20.52 COL 130 HELP
          "CLEAR AUTO FIND Value"
     RECT-4 AT ROW 20.29 COL 1
     RECT-9 AT ROW 1 COL 1 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FGCOLOR 1 .


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
         HEIGHT             = 20.71
         WIDTH              = 143.
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
/* BROWSE-TAB Browser-Table fi_sortby F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN auto_find IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       auto_find:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       Browser-Table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 4
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "2"
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR BUTTON Btn_Clear_Find IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Btn_Clear_Find:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cAccount IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN cDescription IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN cType IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR TOGGLE-BOX lCommReport IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lInactive IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lSalesReport IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR RECTANGLE RECT-4 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-9 IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.account"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED"
     _Where[1]         = "account.company EQ gcompany AND
account.actnum MATCHES cAccount AND
account.dscr MATCHES cDescription AND
account.type MATCHES cType AND
(account.inactive EQ lInactive OR
lInactive EQ NO) AND
(account.salesReport EQ lSalesReport OR
lSalesReport EQ NO) AND
(account.commReport EQ lCommReport OR
lCommReport EQ NO)"
     _FldNameList[1]   > ASI.account.actnum
"account.actnum" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.account.dscr
"account.dscr" "Description" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.account.type
"account.type" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.account.inactive
"account.inactive" ? ? "logical" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "TOGGLE-BOX" "," ? ? 5 no 0 no no
     _FldNameList[5]   > ASI.account.salesReport
"account.salesReport" "Sales" ? "logical" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "TOGGLE-BOX" "," ? ? 5 no 0 no no
     _FldNameList[6]   > ASI.account.commReport
"account.commReport" "Commission" ? "logical" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "TOGGLE-BOX" "," ? ? 5 no 0 no no
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
ON START-SEARCH OF Browser-Table IN FRAME F-Main
DO:
	{methods/template/sortindicator.i}
    IF {&BROWSE-NAME}:CURRENT-COLUMN:NAME NE ? THEN DO:
        ASSIGN
            cColumnLabel = BROWSE {&BROWSE-NAME}:CURRENT-COLUMN:NAME
            cColLabel    = BROWSE {&BROWSE-NAME}:CURRENT-COLUMN:LABEL
            .
        IF cColumnLabel EQ cSaveLabel THEN
        lAscending = NOT lAscending.
        cSaveLabel = cColumnLabel.
        RUN pReopenBrowse.
    END.
	{methods/template/sortindicatorend.i}
    RETURN NO-APPLY.
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


&Scoped-define SELF-NAME btnGO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGO B-table-Win
ON CHOOSE OF btnGO IN FRAME F-Main /* GO */
DO:
    ASSIGN
        {&FilterFields}
        cAccount     = cAccount + "*"
        cDescription = cDescription + "*"
        cType        = cType + "*"
        .
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnShowAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnShowAll B-table-Win
ON CHOOSE OF btnShowAll IN FRAME F-Main /* Show All */
DO:
    ASSIGN
        cAccount:SCREEN-VALUE     = ""
        cDescription:SCREEN-VALUE = ""
        cType:SCREEN-VALUE        = ""
        lCommReport:SCREEN-VALUE  = "NO"
        lSalesReport:SCREEN-VALUE = "NO"
        lInactive:SCREEN-VALUE    = "NO"
        {&FilterFields}
        cAccount                  = "*"
        cDescription              = "*"
        cType                     = "*"
        .
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cAccount
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cAccount B-table-Win
ON RETURN OF cAccount IN FRAME F-Main /* Account */
DO:
    APPLY "CHOOSE":U TO btnGO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cDescription
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cDescription B-table-Win
ON RETURN OF cDescription IN FRAME F-Main /* Description */
DO:
    APPLY "CHOOSE":U TO btnGO.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cType B-table-Win
ON RETURN OF cType IN FRAME F-Main /* Type */
DO:
    APPLY "CHOOSE":U TO btnGO. 
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

ASSIGN
    browse-order:SENSITIVE = NO
    browse-order:HIDDEN    = YES
    .
{methods/winReSize.i}

{methods/sortByProc.i "pByAccount" "account.actnum"}
{methods/sortByProc.i "pByCommReport" "account.commReport"}
{methods/sortByProc.i "pByDescription" "account.dscr"}
{methods/sortByProc.i "pByInactive" "account.inactive"}
{methods/sortByProc.i "pBySalesReport" "account.salesReport"}
{methods/sortByProc.i "pByType" "account.type"}
/* Ticket# : 92946
   Hiding this widget for now, as browser's column label should be indicating the column which is sorted by */
fi_sortby:HIDDEN  = TRUE.
fi_sortby:VISIBLE = FALSE.
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
    DEFINE VARIABLE lcAccFrom AS CHAR NO-UNDO.
    DEFINE VARIABLE lcAccTo   AS CHAR NO-UNDO.
    
    IF AVAIL account AND account.actnum NE "" THEN
    ASSIGN
        lcAccFrom = account.actnum
        lcAccTo = account.actnum
        .
    RUN fg/acc-exp.w (lcAccFrom, lcAccTo).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
      cColumnLabel = "actnum"
      cColLabel = "Account No"
      .
      RUN pReopenBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReopenBrowse B-table-Win 
PROCEDURE pReopenBrowse :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    CASE cColumnLabel:
        WHEN "actnum" THEN
        RUN pByAccount.
        WHEN "commReport" THEN
        RUN pByCommReport.
        WHEN "dscr" THEN
        RUN pByDescription.
        WHEN "inactive" THEN
        RUN pByInactive.
        WHEN "salesReport" THEN
        RUN pBySalesReport.
        WHEN "type" THEN
        RUN pByType.
        OTHERWISE
        {&OPEN-QUERY-{&BROWSE-NAME}}
    END CASE.
    fi_SortBy:SCREEN-VALUE IN FRAME {&FRAME-NAME} = " " + cColLabel + " "
                                                  + TRIM(STRING(lAscending,"A/De"))
                                                  + "scending"
                                                  .
    IF AVAILABLE account THEN
    APPLY "VALUE-CHANGED":U TO {&BROWSE-NAME}.

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
  {src/adm/template/snd-list.i "account"}

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

