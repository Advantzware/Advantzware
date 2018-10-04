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

&SCOPED-DEFINE dataGridInclude dataGrid\browsers\b-itm-com.i
&SCOPED-DEFINE yellowColumnsName item-comm
&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{custom/globdefs.i}
{sys/inc/var.i new shared}
ASSIGN
 cocode = g_company
 locode = g_loc.
{sys/inc/varasgn.i}

DEF TEMP-TABLE tt-item-comm LIKE item-comm
   FIELD valid    AS LOGICAL INIT TRUE
   FIELD row-no   AS INTEGER.

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
&Scoped-define INTERNAL-TABLES item-comm

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table item-comm.cust-no ~
item-comm.i-no item-comm.part-no item-comm.set-sales-price ~
item-comm.base-cost item-comm.zz-dec[1] item-comm.fixed-gross-profit ~
item-comm.overhead-percent item-comm.warehouse-percent ~
item-comm.misc-percent item-comm.freight-percent ~
item-comm.industrial-percent item-comm.comm-rate-percent 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH item-comm WHERE ~{&KEY-PHRASE} ~
      AND item-comm.company = cocode NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH item-comm WHERE ~{&KEY-PHRASE} ~
      AND item-comm.company = cocode NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table item-comm
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table item-comm


/* Definitions for FRAME F-Main                                         */
&Scoped-define QUERY-STRING-F-Main FOR EACH item-comm ~
      WHERE item-comm.company = cocode NO-LOCK
&Scoped-define OPEN-QUERY-F-Main OPEN QUERY F-Main FOR EACH item-comm ~
      WHERE item-comm.company = cocode NO-LOCK.
&Scoped-define TABLES-IN-QUERY-F-Main item-comm
&Scoped-define FIRST-TABLE-IN-QUERY-F-Main item-comm


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 browse-order auto_find ~
Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order fi_sortby auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidDec B-table-Win 
FUNCTION ValidDec RETURNS DECIMAL
  ( ipchRange AS COM-HANDLE )  FORWARD.

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
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 58 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 146 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      item-comm
    FIELDS(item-comm.cust-no
      item-comm.i-no
      item-comm.part-no
      item-comm.set-sales-price
      item-comm.base-cost
      item-comm.zz-dec[1]
      item-comm.fixed-gross-profit
      item-comm.overhead-percent
      item-comm.warehouse-percent
      item-comm.misc-percent
      item-comm.freight-percent
      item-comm.industrial-percent
      item-comm.comm-rate-percent) SCROLLING.

DEFINE QUERY F-Main FOR 
      item-comm SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      item-comm.cust-no FORMAT "x(8)":U WIDTH 11.2 LABEL-BGCOLOR 14
      item-comm.i-no FORMAT "x(15)":U WIDTH 21.2 LABEL-BGCOLOR 14
      item-comm.part-no FORMAT "x(15)":U WIDTH 21.2 LABEL-BGCOLOR 14
      item-comm.set-sales-price COLUMN-LABEL "Set!Sales Price" FORMAT ">>,>>>,>>9.99<<<<":U
            WIDTH 16.2
      item-comm.base-cost FORMAT ">>,>>>,>>9.99<<<<":U WIDTH 13.2
      item-comm.zz-dec[1] COLUMN-LABEL "Rebate%" FORMAT ">>>9.99<":U
            WIDTH 11.2
      item-comm.fixed-gross-profit COLUMN-LABEL "Fixed!Gross Profit" FORMAT ">>>9.99<":U
            WIDTH 17.2
      item-comm.overhead-percent FORMAT ">>>9.99<":U WIDTH 13.2
      item-comm.warehouse-percent FORMAT ">>>9.99<":U WIDTH 14.2
      item-comm.misc-percent FORMAT ">>>9.99<":U
      item-comm.freight-percent FORMAT ">>>9.99<":U WIDTH 11.4
      item-comm.industrial-percent FORMAT ">>>9.99<":U WIDTH 15.8
      item-comm.comm-rate-percent FORMAT ">>>9.99<":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 146 BY 17.86
         FONT 2 ROW-HEIGHT-CHARS .52 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 19.14 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     fi_sortby AT ROW 19.14 COL 58 COLON-ALIGNED NO-LABEL
     auto_find AT ROW 19.14 COL 100.8 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 19.14 COL 132.8 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 19 COL 2
     RECT-4 AT ROW 18.91 COL 1
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
         HEIGHT             = 19.57
         WIDTH              = 146.8.
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
/* BROWSE-TAB Browser-Table TEXT-1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 3
       Browser-Table:MAX-DATA-GUESS IN FRAME F-Main         = 100000
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "2"
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE
       Browser-Table:COLUMN-RESIZABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.item-comm"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED"
     _Where[1]         = "item-comm.company = cocode"
     _FldNameList[1]   > ASI.item-comm.cust-no
"item-comm.cust-no" ? ? "character" ? ? ? 14 ? ? no ? no no "11.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.item-comm.i-no
"item-comm.i-no" ? ? "character" ? ? ? 14 ? ? no ? no no "21.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.item-comm.part-no
"item-comm.part-no" ? "x(15)" "character" ? ? ? 14 ? ? no ? no no "21.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.item-comm.set-sales-price
"item-comm.set-sales-price" "Set!Sales Price" ? "decimal" ? ? ? ? ? ? no ? no no "16.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.item-comm.base-cost
"item-comm.base-cost" ? ? "decimal" ? ? ? ? ? ? no ? no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.item-comm.zz-dec[1]
"item-comm.zz-dec[1]" "Rebate%" ">>>9.99<" "decimal" ? ? ? ? ? ? no ? no no "11.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.item-comm.fixed-gross-profit
"item-comm.fixed-gross-profit" "Fixed!Gross Profit" ? "decimal" ? ? ? ? ? ? no ? no no "17.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.item-comm.overhead-percent
"item-comm.overhead-percent" ? ? "decimal" ? ? ? ? ? ? no ? no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.item-comm.warehouse-percent
"item-comm.warehouse-percent" ? ? "decimal" ? ? ? ? ? ? no ? no no "14.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   = ASI.item-comm.misc-percent
     _FldNameList[11]   > ASI.item-comm.freight-percent
"item-comm.freight-percent" ? ? "decimal" ? ? ? ? ? ? no ? no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.item-comm.industrial-percent
"item-comm.industrial-percent" ? ? "decimal" ? ? ? ? ? ? no ? no no "15.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   = ASI.item-comm.comm-rate-percent
     _Query            is NOT OPENED
*/  /* BROWSE Browser-Table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _TblList          = "ASI.item-comm"
     _Options          = "NO-LOCK"
     _Where[1]         = "ASI.item-comm.company = cocode"
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
  RUN startsearch.
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
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
RUN oerep/rd-itcom.w ("","","","","","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE import-excel B-table-Win 
PROCEDURE import-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF BUFFER b-item-comm  FOR item-comm.
   DEF BUFFER b-itemfg     FOR itemfg.
   DEF BUFFER b-cust       FOR cust.

   DEF VAR v-answer     AS LOG         NO-UNDO.
   DEF VAR chFile       AS CHAR        NO-UNDO.
   DEF VAR v-ok         AS LOG         NO-UNDO.
   DEF VAR chExcelApplication AS COM-HANDLE NO-UNDO.
   DEF VAR chWorkBook   AS COM-HANDLE  NO-UNDO.
   DEF VAR chWorksheet  AS COM-HANDLE  NO-UNDO.
   DEF VAR v-RowCount   AS INT INIT 2  NO-UNDO.
   DEF VAR v-line       AS INT INIT 1  NO-UNDO.
   DEF VAR v-valid-flag AS LOG INIT YES NO-UNDO.
   DEF VAR char-hdl     AS CHAR        NO-UNDO.
   DEF VAR v-rowid      AS ROWID       NO-UNDO.
   DEF VAR v-id         AS CHAR        NO-UNDO.
   DEF VAR v-deci-at    AS INT         NO-UNDO.
   DEF VAR v-ok-cnt     AS INT         NO-UNDO.
   DEF VAR v-nok-cnt    AS INT         NO-UNDO.
   DEF VAR v-tot-cnt    AS INT         NO-UNDO.
   DEF VAR v-over-written AS INT       NO-UNDO.

   DEF VAR v-file       AS CHAR INIT "c:\tmp\comm-item-error.txt".

   IF SEARCH(v-file) <> ? THEN
      OS-DELETE VALUE(v-file).

   OUTPUT TO VALUE(v-file).

   FOR EACH tt-item-comm:
      DELETE tt-item-comm.
   END.

   DO WITH FRAME {&FRAME-NAME}:

      SYSTEM-DIALOG GET-FILE chFile 
                    TITLE "Select File to Import"
                    FILTERS "Excel File (*.xls,*.xlsx) " "*.xls,*.xlsx"
                    INITIAL-DIR "c:\"
                    MUST-EXIST
                    USE-FILENAME
                    UPDATE v-ok.

      IF v-ok THEN DO:
         IF LENGTH(chFile) LT 4 OR
            (SUBSTR(chFile,LENGTH(chFile) - 3) NE ".xls" AND 
            SUBSTR(chFile,LENGTH(chFile) - 4) NE ".xlsx") THEN DO:
            MESSAGE "Invalid File.  Must Choose Excel (.xls) File."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            LEAVE.
         END.

         SESSION:SET-WAIT-STATE ("general").

         /* Initialize Excel. */
         CREATE "Excel.Application" chExcelApplication NO-ERROR.

         /* Check if Excel got initialized. */
         IF NOT (VALID-HANDLE (chExcelApplication)) THEN DO:
            MESSAGE "Unable to Start Excel." VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR. 
         END.

         /* Open our Excel File. */  
         chExcelApplication:VISIBLE = FALSE.
         chWorkbook = chExcelApplication:Workbooks:OPEN(chfile) NO-ERROR.

         /* Do not display Excel error messages. */
         chExcelApplication:DisplayAlerts = FALSE NO-ERROR.

         /* Go to the Active Sheet. */
         chWorkbook:WorkSheets(1):Activate NO-ERROR.

         ASSIGN
            chWorkSheet = chExcelApplication:Sheets:ITEM(1).

         REPEAT:
            IF chWorkSheet:Range("A" + STRING(v-RowCount)):VALUE = ? THEN LEAVE.

            CREATE tt-item-comm.
            ASSIGN
               tt-item-comm.company            = cocode
               tt-item-comm.create-date        = TODAY
               tt-item-comm.create-time        = TIME
               tt-item-comm.create-user-id     = USERID("NOSWEAT")
               tt-item-comm.rec_key            = STRING(YEAR(TODAY), "9999") + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99") + STRING(TIME)
               tt-item-comm.upd-date           = TODAY
               tt-item-comm.upd-user-id        = USERID("NOSWEAT")
               tt-item-comm.cust-no            = chWorkSheet:Range("A" + STRING(v-RowCount)):VALUE NO-ERROR.
               tt-item-comm.i-no               = chWorkSheet:Range("B" + STRING(v-RowCount)):VALUE NO-ERROR.
               tt-item-comm.set-sales-price    = ValidDec(chWorkSheet:Range("C" + STRING(v-RowCount))).
               tt-item-comm.base-cost          = ValidDec(chWorkSheet:Range("D" + STRING(v-RowCount))).
               tt-item-comm.zz-char[2]         = chWorkSheet:Range("E" + STRING(v-RowCount)):VALUE NO-ERROR.
               tt-item-comm.zz-char[3]         = chWorkSheet:Range("F" + STRING(v-RowCount)):VALUE NO-ERROR.
               tt-item-comm.zz-dec[1]          = ValidDec(chWorkSheet:Range("G" + STRING(v-RowCount))).
               tt-item-comm.fixed-gross-profit = ValidDec(chWorkSheet:Range("H" + STRING(v-RowCount))).
               tt-item-comm.overhead-percent   = ValidDec(chWorkSheet:Range("I" + STRING(v-RowCount))).
               tt-item-comm.misc-percent       = ValidDec(chWorkSheet:Range("J" + STRING(v-RowCount))).
               tt-item-comm.freight-percent    = ValidDec(chWorkSheet:Range("K" + STRING(v-RowCount))).
               tt-item-comm.industrial-percent = ValidDec(chWorkSheet:Range("L" + STRING(v-RowCount))).
               tt-item-comm.warehouse-percent  = ValidDec(chWorkSheet:Range("M" + STRING(v-RowCount))).
               tt-item-comm.comm-rate-percent  = ValidDec(chWorkSheet:Range("N" + STRING(v-RowCount))).
               tt-item-comm.zz-char[4]         = chWorkSheet:Range("O" + STRING(v-RowCount)):VALUE NO-ERROR.

            ASSIGN
               v-tot-cnt = v-tot-cnt + 1
               tt-item-comm.row-no = v-RowCount
               v-RowCount = v-RowCount + 1.
         END.
      END.

      /*Free memory*/
      chWorkbook = chExcelApplication:Workbooks:CLOSE() NO-ERROR.
      RELEASE OBJECT chWorkbook NO-ERROR.
      RELEASE OBJECT chWorkSheet NO-ERROR.
      RELEASE OBJECT chExcelApplication NO-ERROR.

      FOR EACH tt-item-comm:

         FIND FIRST b-cust WHERE b-cust.company = tt-item-comm.company
                             AND b-cust.cust-no = tt-item-comm.cust-no NO-LOCK NO-ERROR.
         IF NOT AVAILABLE(b-cust) THEN DO:
            PUT UNFORMATTED "Invalid Customer Number " + '"' + tt-item-comm.cust-no + '"' + ", in row " + STRING(tt-item-comm.row-no) + "." SKIP.
            tt-item-comm.valid = FALSE.
         END.

         FIND FIRST b-itemfg WHERE b-itemfg.company = tt-item-comm.company
                               AND b-itemfg.cust-no = tt-item-comm.cust-no
                               AND b-itemfg.i-no    = tt-item-comm.i-no NO-LOCK NO-ERROR.
         IF NOT AVAILABLE(b-itemfg) THEN DO:
            FIND FIRST b-itemfg WHERE b-itemfg.company = tt-item-comm.company
                                  AND b-itemfg.i-no    = tt-item-comm.i-no NO-LOCK NO-ERROR.
            IF NOT AVAILABLE(b-itemfg) THEN DO: 
               PUT UNFORMATTED "Invalid FG Item Number " + '"' + tt-item-comm.i-no + '"' + ", in row " + STRING(tt-item-comm.row-no) + "." SKIP.
               tt-item-comm.valid = FALSE.
            END.
            ELSE
               ASSIGN
                  tt-item-comm.part-no    = b-itemfg.part-no
                  tt-item-comm.i-dscr     = b-itemfg.i-dscr
                  tt-item-comm.i-name     = b-itemfg.i-name
                  tt-item-comm.part-dscr1 = b-itemfg.part-dscr1
                  tt-item-comm.part-dscr2 = b-itemfg.part-dscr2.
         END.
         ELSE
            ASSIGN
               tt-item-comm.part-no    = b-itemfg.part-no
               tt-item-comm.i-dscr     = b-itemfg.i-dscr
               tt-item-comm.i-name     = b-itemfg.i-name
               tt-item-comm.part-dscr1 = b-itemfg.part-dscr1
               tt-item-comm.part-dscr2 = b-itemfg.part-dscr2.

/*          IF CAN-FIND(FIRST b-item-comm WHERE b-item-comm.company = tt-item-comm.company                                                                                                             */
/*                                          AND b-item-comm.cust-no = tt-item-comm.cust-no                                                                                                             */
/*                                          AND b-item-comm.i-no    = tt-item-comm.i-no) THEN DO:                                                                                                      */
/*             PUT UNFORMATTED "FG Item Number " + '"' + tt-item-comm.i-no + '"' + ", in row " + STRING(tt-item-comm.row-no) + " already exists in the Commission Cost Item file. Did not load." SKIP. */
/*             tt-item-comm.valid = FALSE.                                                                                                                                                             */
/*          END.                                                                                                                                                                                       */
         IF tt-item-comm.valid = TRUE THEN DO:
            FIND FIRST b-item-comm WHERE b-item-comm.company = tt-item-comm.company
                                     AND b-item-comm.cust-no = tt-item-comm.cust-no
                                     AND b-item-comm.i-no    = tt-item-comm.i-no NO-ERROR.
            IF NOT AVAILABLE(b-item-comm) THEN DO:
               CREATE item-comm.
               BUFFER-COPY tt-item-comm EXCEPT tt-item-comm.valid tt-item-comm.row-no TO item-comm NO-ERROR.
               ASSIGN
                  v-ok-cnt = v-ok-cnt + 1
                  v-rowid = ROWID(item-comm).
            END.
            ELSE DO:
               ASSIGN
                  b-item-comm.set-sales-price = tt-item-comm.set-sales-price               
                  b-item-comm.base-cost       = tt-item-comm.base-cost
                  v-rowid = ROWID(b-item-comm)
                  v-over-written = v-over-written + 1.
               IF tt-item-comm.zz-char[2] NE "" AND tt-item-comm.zz-char[2] NE ? THEN
                  b-item-comm.zz-char[2]      = tt-item-comm.zz-char[2].
               IF tt-item-comm.zz-char[3] NE "" AND tt-item-comm.zz-char[3] NE ? THEN
                  b-item-comm.zz-char[3]      = tt-item-comm.zz-char[3].
               IF tt-item-comm.zz-dec[1] NE 0 AND tt-item-comm.zz-dec[1] NE ? THEN 
                  b-item-comm.zz-dec[1]       = tt-item-comm.zz-dec[1].
               IF tt-item-comm.fixed-gross-profit NE 0 AND tt-item-comm.fixed-gross-profit NE ? THEN
                  b-item-comm.fixed-gross-profit = tt-item-comm.fixed-gross-profit.
               IF tt-item-comm.overhead-percent NE 0 AND tt-item-comm.overhead-percent NE ? THEN
                  b-item-comm.overhead-percent   = tt-item-comm.overhead-percent.
               IF tt-item-comm.misc-percent NE 0 AND tt-item-comm.misc-percent NE ? THEN
                  b-item-comm.misc-percent       = tt-item-comm.misc-percent.
               IF tt-item-comm.freight-percent NE 0 AND tt-item-comm.freight-percent NE ? THEN
                  b-item-comm.freight-percent    = tt-item-comm.freight-percent.
               IF tt-item-comm.industrial-percent NE 0 AND tt-item-comm.industrial-percent NE ? THEN
                  b-item-comm.industrial-percent = tt-item-comm.industrial-percent.
               IF tt-item-comm.comm-rate-percent NE 0 AND tt-item-comm.comm-rate-percent NE ? THEN
                  b-item-comm.comm-rate-percent  = tt-item-comm.comm-rate-percent.
               IF tt-item-comm.warehouse-percent NE 0 AND tt-item-comm.warehouse-percent NE ? THEN
                  b-item-comm.warehouse-percent  = tt-item-comm.warehouse-percent.
               IF tt-item-comm.zz-char[4] EQ "YES" OR tt-item-comm.zz-char[4] EQ "NO" THEN
                  b-item-comm.zz-char[4]  = tt-item-comm.zz-char[4].
            END.
         END.
         ELSE
            v-nok-cnt = v-nok-cnt + 1.
      END.

      PUT UNFORMATTED "Total number of records in Excel file " + STRING(v-tot-cnt)              SKIP
                      "Total number of records sucessfully created " + STRING(v-ok-cnt)         SKIP
                      "Total number of records over written " + STRING(v-over-written)          SKIP
                      "Total number of records in Excel file with errors " + STRING(v-nok-cnt)  SKIP.

      MESSAGE "Excel File Import Completed." SKIP
              "Please review error file: c:\tmp\comm-item-error.txt"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.

   OUTPUT CLOSE.

   RUN dispatch ('open-query').

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

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY 'ENTRY' TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.

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
  {src/adm/template/snd-list.i "item-comm"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-base-cost B-table-Win 
PROCEDURE update-base-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-itemfg FOR itemfg.
DEF BUFFER b-item-comm FOR item-comm.
DEF VAR vp-rowid AS ROWID NO-UNDO.

FOR EACH b-item-comm WHERE
    b-item-comm.company = cocode AND
    TRIM(b-item-comm.zz-char[4]) <> "YES",
    FIRST b-itemfg FIELDS(avg-cost prod-uom) WHERE
          b-itemfg.company = cocode AND
          b-itemfg.i-no    = b-item-comm.i-no
          NO-LOCK:

    ASSIGN 
       b-item-comm.base-cost = b-itemfg.avg-cost
       b-item-comm.zz-char[3] = b-itemfg.prod-uom.
END.

FIND CURRENT item-comm NO-LOCK NO-ERROR.
IF AVAIL item-comm THEN
       vp-rowid  = ROWID(item-comm) .

/* RUN local-open-query. */
RUN dispatch ('open-query').

 DO WITH FRAME {&FRAME-NAME}:
    REPOSITION {&browse-name} TO ROWID vp-rowid NO-ERROR.
    APPLY "VALUE-CHANGED" TO Browser-Table.
 END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValidDec B-table-Win 
FUNCTION ValidDec RETURNS DECIMAL
  ( ipchRange AS COM-HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cTemp AS CHAR NO-UNDO.

cTemp = ipchRange:VALUE NO-ERROR.

IF TRIM(cTemp) = "" OR cTemp = ? THEN
    RETURN 0.00.   
ELSE
    RETURN DEC(cTemp).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

