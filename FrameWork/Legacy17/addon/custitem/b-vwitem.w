&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          emptrack         PROGRESS
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
&SCOPED-DEFINE dataGridInclude dataGrid\addon\custitem\b-vwitem.i
&SCOPED-DEFINE yellowColumnsName vend-whse-item
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/gcompany.i}
{custom/gloc.i}

DEF VAR v-char-val         AS CHAR  NO-UNDO.
DEF VAR v-prev-val         AS CHAR  NO-UNDO.
DEF VAR v-recid            AS RECID NO-UNDO.  /* get assigned from local-create-record*/
DEF VAR v-last-cust-no     LIKE vend-whse-item.cust-no NO-UNDO.
DEF VAR v-last-plant-code  LIKE vend-whse-item.vendor-plant-code NO-UNDO.
DEF VAR v-copy-rec         AS LOG NO-UNDO.     
DEF VAR v-cust-no          LIKE vend-whse-item.cust-no NO-UNDO.                 /* for copy */
DEF VAR v-plant-code       LIKE vend-whse-item.vendor-plant-code NO-UNDO.       /* for copy */
DEF VAR v-prev-recid       AS RECID NO-UNDO.  
DEF VAR char-val           AS CHAR NO-UNDO.

&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF

DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}

ASSIGN cocode = g_company
       locode = g_loc.

 DEF TEMP-TABLE tt-vend-whse-trans LIKE vend-whse-trans
   FIELD valid             AS LOG INIT TRUE
   FIELD row-no            AS INT.

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
&Scoped-define INTERNAL-TABLES vend-whse-item

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table vend-whse-item.cust-no ~
vend-whse-item.revision vend-whse-item.fg-item-no ~
vend-whse-item.cust-part-no vend-whse-item.vendor-code ~
vend-whse-item.vendor-plant-code vend-whse-item.vendor-dept-code ~
vend-whse-item.obsolete-date vend-whse-item.est-annual-usage ~
vend-whse-item.plant-tot-oh-qty 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table vend-whse-item.cust-no ~
vend-whse-item.revision vend-whse-item.fg-item-no ~
vend-whse-item.cust-part-no vend-whse-item.vendor-plant-code ~
vend-whse-item.vendor-dept-code vend-whse-item.obsolete-date ~
vend-whse-item.est-annual-usage vend-whse-item.plant-tot-oh-qty 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table vend-whse-item
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table vend-whse-item
&Scoped-define QUERY-STRING-Browser-Table FOR EACH vend-whse-item WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH vend-whse-item WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table vend-whse-item
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table vend-whse-item


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 browse-order auto_find ~
Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
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
     SIZE 117 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 88 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 146 BY 2.57.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      vend-whse-item
    FIELDS(vend-whse-item.cust-no
      vend-whse-item.revision
      vend-whse-item.fg-item-no
      vend-whse-item.cust-part-no
      vend-whse-item.vendor-code
      vend-whse-item.vendor-plant-code
      vend-whse-item.vendor-dept-code
      vend-whse-item.obsolete-date
      vend-whse-item.est-annual-usage
      vend-whse-item.plant-tot-oh-qty) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      vend-whse-item.cust-no FORMAT "x(8)":U WIDTH 13.2 LABEL-BGCOLOR 14
      vend-whse-item.revision COLUMN-LABEL "REV" FORMAT "x(8)":U
            WIDTH 5.2 LABEL-BGCOLOR 14
      vend-whse-item.fg-item-no FORMAT "x(15)":U WIDTH 19.2 LABEL-BGCOLOR 14
      vend-whse-item.cust-part-no FORMAT "x(15)":U WIDTH 19.2 LABEL-BGCOLOR 14
      vend-whse-item.vendor-code FORMAT "x(8)":U WIDTH 13.2 LABEL-BGCOLOR 14
      vend-whse-item.vendor-plant-code FORMAT "x(8)":U WIDTH 13.2
            LABEL-BGCOLOR 14
      vend-whse-item.vendor-dept-code FORMAT "x(8)":U WIDTH 13.2
            LABEL-BGCOLOR 14
      vend-whse-item.obsolete-date COLUMN-LABEL "Obsolete!Date" FORMAT "99/99/99":U
            WIDTH 12.2 LABEL-BGCOLOR 14
      vend-whse-item.est-annual-usage COLUMN-LABEL "Customers Est.!Annual Usage" FORMAT "->>,>>>,>>9":U
            WIDTH 20.2
      vend-whse-item.plant-tot-oh-qty FORMAT "->>>,>>>,>>9":U WIDTH 21.2
  ENABLE
      vend-whse-item.cust-no
      vend-whse-item.revision
      vend-whse-item.fg-item-no
      vend-whse-item.cust-part-no
      vend-whse-item.vendor-plant-code
      vend-whse-item.vendor-dept-code
      vend-whse-item.obsolete-date
      vend-whse-item.est-annual-usage
      vend-whse-item.plant-tot-oh-qty
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 146 BY 14.52
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 15.81 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     fi_sortby AT ROW 15.81 COL 92 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     auto_find AT ROW 16.86 COL 13 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 16.86 COL 133.2 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 15.71 COL 2
     RECT-4 AT ROW 15.57 COL 1
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
         HEIGHT             = 17.14
         WIDTH              = 146.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
{methods/template/browser.i}

{Advantzware/WinKit/dataGridProc.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB Browser-Table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 3
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE
       Browser-Table:COLUMN-RESIZABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "vend-whse-item"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED"
     _FldNameList[1]   > vend-whse-item.cust-no
"cust-no" ? ? "character" ? ? ? 14 ? ? yes ? no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > vend-whse-item.revision
"revision" "REV" ? "character" ? ? ? 14 ? ? yes ? no no "5.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > vend-whse-item.fg-item-no
"fg-item-no" ? ? "character" ? ? ? 14 ? ? yes ? no no "19.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > vend-whse-item.cust-part-no
"cust-part-no" ? "x(15)" "character" ? ? ? 14 ? ? yes ? no no "19.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > vend-whse-item.vendor-code
"vendor-code" ? ? "character" ? ? ? 14 ? ? no ? no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > vend-whse-item.vendor-plant-code
"vendor-plant-code" ? ? "character" ? ? ? 14 ? ? yes ? no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > vend-whse-item.vendor-dept-code
"vendor-dept-code" ? ? "character" ? ? ? 14 ? ? yes ? no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > vend-whse-item.obsolete-date
"obsolete-date" "Obsolete!Date" ? "date" ? ? ? 14 ? ? yes ? no no "12.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > vend-whse-item.est-annual-usage
"est-annual-usage" "Customers Est.!Annual Usage" ? "decimal" ? ? ? ? ? ? yes ? no no "20.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > vend-whse-item.plant-tot-oh-qty
"plant-tot-oh-qty" ? ? "decimal" ? ? ? ? ? ? yes ? no no "21.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
   DEF VAR lv-handle AS WIDGET-HANDLE NO-UNDO.
   DEF BUFFER b-itemfg FOR itemfg.

   v-char-val = "".

   CASE FOCUS:NAME:
      WHEN "cust-no" THEN DO:
         RUN custitem/l-vwxref.w (cocode, FOCUS:SCREEN-VALUE, OUTPUT v-char-val).
         IF v-char-val <> "" THEN DO:
            ASSIGN 
               FOCUS:SCREEN-VALUE = ENTRY(1,v-char-val)
               vend-whse-item.vendor-code:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = ENTRY(2,v-char-val).
         END.
         RETURN NO-APPLY.
      END.
      WHEN "fg-item-no" THEN DO:
         RUN windows/l-itemfg.w (cocode, "", FOCUS:SCREEN-VALUE, OUTPUT v-char-val).
         IF v-char-val <> "" THEN DO:
            ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,v-char-val).
            FIND FIRST b-itemfg WHERE b-itemfg.company = cocode 
                                  AND b-itemfg.i-no = ENTRY(1,v-char-val) NO-LOCK NO-ERROR.
            IF AVAILABLE(b-itemfg) THEN DO:
               ASSIGN vend-whse-item.cust-part-no:SCREEN-VALUE IN BROWSE {&browse-name} = b-itemfg.part-no.
            END.
         END.
         RETURN NO-APPLY.
      END.
      WHEN "vendor-plant-code" THEN DO:
         RUN custitem/l-plntid.w (cocode, vend-whse-item.cust-no:SCREEN-VALUE, OUTPUT v-char-val).
         IF v-char-val <> "" THEN DO:
            ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,v-char-val).
            ASSIGN vend-whse-item.vendor-dept-code:SCREEN-VALUE = ENTRY(2,v-char-val). 
         END.
         RETURN NO-APPLY.
      END.
      WHEN "vendor-dept-code" THEN DO:
         RUN custitem/l-plntid.w (cocode, vend-whse-item.cust-no:SCREEN-VALUE, OUTPUT v-char-val).
         IF v-char-val <> "" THEN DO:
            ASSIGN FOCUS:SCREEN-VALUE = ENTRY(2,v-char-val).
         END.
         RETURN NO-APPLY.
      END.
   END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend-whse-item.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-whse-item.cust-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF vend-whse-item.cust-no IN BROWSE Browser-Table /* Suppliers!A/R Code */
DO:
   IF LASTKEY NE -1 THEN DO:
      RUN val-cust-no NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
         RETURN NO-APPLY.
      ELSE
         RUN fill-vendor-code.   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend-whse-item.fg-item-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-whse-item.fg-item-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF vend-whse-item.fg-item-no IN BROWSE Browser-Table /* Suppliers!FG Item */
DO:
   IF LASTKEY NE -1 THEN DO:
      RUN val-fg-item NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend-whse-item.cust-part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend-whse-item.cust-part-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF vend-whse-item.cust-part-no IN BROWSE Browser-Table /* Customers!Part# */
DO:
   IF LASTKEY NE -1 THEN DO:
      RUN val-cust-part-no NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{custom/getloc.i}
{sys/inc/f3help.i}
{custom/yellowColumns.i}

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).  


&ENDIF

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fill-vendor-code B-table-Win 
PROCEDURE fill-vendor-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   DEF BUFFER b-vend-code-cust-xref FOR vend-code-cust-xref.

   FIND FIRST b-vend-code-cust-xref WHERE b-vend-code-cust-xref.company = cocode
                                      AND b-vend-code-cust-xref.cust-no = vend-whse-item.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
   IF AVAILABLE(b-vend-code-cust-xref) THEN
      ASSIGN vend-whse-item.vendor-code:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = b-vend-code-cust-xref.vendor-code.

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
   DEF BUFFER b-vend-whse-trans FOR vend-whse-trans.
   DEF BUFFER b-vend-whse-item  FOR vend-whse-item.
   DEF BUFFER b-itemfg          FOR itemfg.
   DEF BUFFER b-vend-code-cust-xref FOR vend-code-cust-xref.

   DEF VAR v-r-no       LIKE vend-whse-trans.r-no NO-UNDO.
   DEF VAR v-answer     AS LOG         NO-UNDO.
   DEF VAR chFile       AS CHAR        NO-UNDO.
   DEF VAR v-ok         AS LOG         NO-UNDO.
   DEF VAR chExcelAppl  AS COM-HANDLE  NO-UNDO.
   DEF VAR chWorkBook   AS COM-HANDLE  NO-UNDO.
   DEF VAR chWorksheet  AS COM-HANDLE  NO-UNDO.
   DEF VAR v-RowCount   AS INT INIT 2  NO-UNDO.
   DEF VAR v-valid-flag AS LOG INIT YES NO-UNDO.
   DEF VAR char-hdl     AS CHAR        NO-UNDO.
   DEF VAR v-rowid      AS ROWID       NO-UNDO.
   DEF VAR v-id         AS CHAR        NO-UNDO.
   DEF VAR v-deci-at    AS INTEGER     NO-UNDO.

   DEF VAR v-file       AS CHAR INIT "c:\tmp\vwexcimp-error.txt".

   OUTPUT TO VALUE(v-file).

   FOR EACH tt-vend-whse-trans:
      DELETE tt-vend-whse-trans.
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
         CREATE "Excel.Application" chExcelAppl NO-ERROR.

         /* Check if Excel got initialized. */
         IF NOT (VALID-HANDLE (chExcelAppl)) THEN DO:
            MESSAGE "Unable to Start Excel." VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR. 
         END.

         /* Open our Excel File. */  
         chExcelAppl:VISIBLE = FALSE.
         chWorkbook = chExcelAppl:Workbooks:OPEN(chfile) NO-ERROR.

         /* Do not display Excel error messages. */
         chExcelAppl:DisplayAlerts = FALSE NO-ERROR.

         /* Go to the Active Sheet. */
         chWorkbook:WorkSheets(1):Activate NO-ERROR.

         ASSIGN
            chWorkSheet = chExcelAppl:Sheets:ITEM(1).

         REPEAT:
            IF chWorkSheet:Range("A" + STRING(v-RowCount)):VALUE EQ ? THEN LEAVE.

            CREATE tt-vend-whse-trans.
            ASSIGN
               tt-vend-whse-trans.company           = cocode
               tt-vend-whse-trans.vendor-code       = chWorkSheet:Range("A" + STRING(v-RowCount)):VALUE NO-ERROR.
               tt-vend-whse-trans.vendor-plant-code = chWorkSheet:Range("B" + STRING(v-RowCount)):VALUE NO-ERROR.
               tt-vend-whse-trans.vendor-dept-code  = chWorkSheet:Range("C" + STRING(v-RowCount)):VALUE NO-ERROR.
               tt-vend-whse-trans.cust-part-no      = chWorkSheet:Range("D" + STRING(v-RowCount)):VALUE NO-ERROR.
               tt-vend-whse-trans.revision          = chWorkSheet:Range("E" + STRING(v-RowCount)):VALUE NO-ERROR.
               tt-vend-whse-trans.est-annual-usage  = chWorkSheet:Range("G" + STRING(v-RowCount)):VALUE NO-ERROR.
               tt-vend-whse-trans.plant-tot-oh-qty  = chWorkSheet:Range("H" + STRING(v-RowCount)):VALUE NO-ERROR.
            ASSIGN
               tt-vend-whse-trans.row-no = v-RowCount
               v-RowCount = v-RowCount + 1.
         END.
      END.

      /*Free memory*/
      chWorkbook = chExcelAppl:Workbooks:CLOSE() NO-ERROR.
      RELEASE OBJECT chWorkbook NO-ERROR.
      RELEASE OBJECT chWorkSheet NO-ERROR.
      RELEASE OBJECT chExcelAppl NO-ERROR.

      FOR EACH tt-vend-whse-trans:

         v-deci-at = INDEX(tt-vend-whse-trans.vendor-code, ".0000000000").
         IF v-deci-at > 0 THEN
            tt-vend-whse-trans.vendor-code = SUBSTRING(tt-vend-whse-trans.vendor-code, 1, v-deci-at - 1). 

         v-deci-at = INDEX(tt-vend-whse-trans.vendor-plant-code, ".0000000000").
         IF v-deci-at > 0 THEN
            tt-vend-whse-trans.vendor-plant-code = SUBSTRING(tt-vend-whse-trans.vendor-plant-code, 1, v-deci-at - 1). 

         v-deci-at = INDEX(tt-vend-whse-trans.vendor-dept-code, ".0000000000").
         IF v-deci-at > 0 THEN
            tt-vend-whse-trans.vendor-dept-code = SUBSTRING(tt-vend-whse-trans.vendor-dept-code, 1, v-deci-at - 1). 

         v-deci-at = INDEX(tt-vend-whse-trans.fg-item-no, ".0000000000").
         IF v-deci-at > 0 THEN
            tt-vend-whse-trans.fg-item-no = SUBSTRING(tt-vend-whse-trans.fg-item-no, 1, v-deci-at - 1). 

         FIND FIRST b-vend-code-cust-xref WHERE b-vend-code-cust-xref.company     = tt-vend-whse-trans.company 
                                           AND b-vend-code-cust-xref.vendor-code  = tt-vend-whse-trans.vendor-code NO-LOCK NO-ERROR.

         IF NOT AVAILABLE(b-vend-code-cust-xref) THEN DO: 
            PUT UNFORMATTED "Invalid Customers A/P Code " + '"' + tt-vend-whse-trans.vendor-code + '"' + ", in row " + STRING(tt-vend-whse-trans.row-no) + "." SKIP.
            ASSIGN
               tt-vend-whse-trans.valid = FALSE
               v-valid-flag = FALSE.
         END.
         ELSE
            ASSIGN tt-vend-whse-trans.cust-no = b-vend-code-cust-xref.cust-no.

         IF NOT CAN-FIND(FIRST vend-plant WHERE vend-plant.company      = tt-vend-whse-trans.company
                                            AND vend-plant.vendor-code  = tt-vend-whse-trans.vendor-code
                                            AND vend-plant.plant-id     = tt-vend-whse-trans.vendor-plant-code) THEN DO:
            PUT UNFORMATTED "Invalid Customers Plant ID " + '"' + tt-vend-whse-trans.vendor-plant-code + '"' + ", in row " + STRING(tt-vend-whse-trans.row-no) + "." SKIP.
            ASSIGN
               tt-vend-whse-trans.valid = FALSE
               v-valid-flag = FALSE.
         END.

         IF NOT CAN-FIND(FIRST vend-plant WHERE vend-plant.company      = tt-vend-whse-trans.company
                                            AND vend-plant.vendor-code  = tt-vend-whse-trans.vendor-code
                                            AND vend-plant.plant-id     = tt-vend-whse-trans.vendor-plant-code
                                            AND vend-plant.vendor-dept-code  = tt-vend-whse-trans.vendor-dept-code) THEN DO:
            PUT UNFORMATTED "Invalid Customers Dept Code " + '"' + tt-vend-whse-trans.vendor-dept-code + '"' + ", in row " + STRING(tt-vend-whse-trans.row-no) + "." SKIP.
            ASSIGN
               tt-vend-whse-trans.valid = FALSE
               v-valid-flag = FALSE.
         END.

         IF NOT CAN-FIND(FIRST b-itemfg WHERE b-itemfg.company = tt-vend-whse-trans.company 
                                          AND b-itemfg.part-no = tt-vend-whse-trans.cust-part-no) THEN DO:

            PUT UNFORMATTED "Invalid Customers Part Number " + '"' + tt-vend-whse-trans.cust-part-no + '"' + ", in row " + STRING(tt-vend-whse-trans.row-no) + "." SKIP.
            ASSIGN
               tt-vend-whse-trans.valid = FALSE
               v-valid-flag = FALSE.
         END.

         IF NOT CAN-FIND(FIRST b-vend-whse-item WHERE b-vend-whse-item.company            = tt-vend-whse-trans.company
                                                  AND b-vend-whse-item.vendor-code        = tt-vend-whse-trans.vendor-code
                                                  AND b-vend-whse-item.vendor-plant-code  = tt-vend-whse-trans.vendor-plant-code
                                                  AND b-vend-whse-item.cust-part-no       = tt-vend-whse-trans.cust-part-no 
                                                  AND b-vend-whse-item.vendor-dept-code   = tt-vend-whse-trans.vendor-dept-code) THEN DO:

            PUT UNFORMATTED "Invalid Warehouse Management Customers Part Number " + '"' + tt-vend-whse-trans.cust-part-no + '"' + ", in row " + STRING(tt-vend-whse-trans.row-no) + "." SKIP.
            ASSIGN
               tt-vend-whse-trans.valid = FALSE
               v-valid-flag = FALSE.
         END.
      END.

      IF v-valid-flag = FALSE THEN DO:
         MESSAGE "The Excel file did not load, please review error file." SKIP
                 "c:\tmp\vw-xlsimp-error.txt"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

      END.
      ELSE DO:
         v-r-no = 0.
         FIND LAST b-vend-whse-trans USE-INDEX r-no NO-LOCK NO-ERROR.
         IF AVAIL b-vend-whse-trans AND b-vend-whse-trans.r-no > v-r-no THEN v-r-no = b-vend-whse-trans.r-no.

         FIND LAST vend-whse-trans-hist USE-INDEX r-no NO-LOCK NO-ERROR.
         IF AVAIL vend-whse-trans-hist AND vend-whse-trans-hist.r-no GT v-r-no THEN v-r-no = vend-whse-trans-hist.r-no.

         DO WHILE TRUE:
            v-r-no = v-r-no + 1.

            FIND FIRST vend-whse-trans-hist WHERE vend-whse-trans-hist.r-no = v-r-no USE-INDEX r-no NO-LOCK NO-ERROR.
            IF AVAIL vend-whse-trans-hist THEN NEXT.

            FIND FIRST b-vend-whse-trans WHERE b-vend-whse-trans.r-no = v-r-no USE-INDEX r-no NO-LOCK NO-ERROR.
            IF AVAIL b-vend-whse-trans THEN NEXT.

            LEAVE.
         END.

         FOR EACH tt-vend-whse-trans:

            FIND FIRST b-itemfg WHERE b-itemfg.company = tt-vend-whse-trans.company
                                  AND b-itemfg.cust-no = tt-vend-whse-trans.cust-no
                                  AND b-itemfg.part-no = tt-vend-whse-trans.cust-part-no NO-LOCK NO-ERROR.
            IF NOT AVAILABLE(b-itemfg) THEN
               FIND FIRST b-itemfg WHERE b-itemfg.company = tt-vend-whse-trans.company
                                     AND b-itemfg.part-no = tt-vend-whse-trans.cust-part-no NO-LOCK NO-ERROR.
            CREATE vend-whse-trans.
            ASSIGN
               vend-whse-trans.company           = tt-vend-whse-trans.company
               vend-whse-trans.r-no              = v-r-no
               vend-whse-trans.vendor-code       = tt-vend-whse-trans.vendor-code       
               vend-whse-trans.vendor-plant-code = tt-vend-whse-trans.vendor-plant-code
               vend-whse-trans.vendor-dept-code  = tt-vend-whse-trans.vendor-dept-code
               vend-whse-trans.fg-item-no        = b-itemfg.i-no
               vend-whse-trans.cust-part-no      = tt-vend-whse-trans.cust-part-no
               vend-whse-trans.est-annual-usage  = tt-vend-whse-trans.est-annual-usage               
               vend-whse-trans.plant-tot-oh-qty  = tt-vend-whse-trans.plant-tot-oh-qty                  
               vend-whse-trans.create-date       = TODAY
               vend-whse-trans.create-time       = TIME
               vend-whse-trans.create-userid     = USERID("nosweat")
               vend-whse-trans.cust-no           = tt-vend-whse-trans.cust-no
               vend-whse-trans.rec_key           = STRING(YEAR(TODAY)) + STRING(MONTH(TODAY)) + STRING(DAY(TODAY)) + STRING(TIME) 
               vend-whse-trans.trans-date        = TODAY
               vend-whse-trans.trans-type        = "X"                                  
               vend-whse-trans.upd-date          = TODAY 
               vend-whse-trans.upd-time          = TIME
               vend-whse-trans.upd-userid        = USERID("nosweat")
               vend-whse-trans.revision          = tt-vend-whse-trans.revision
               v-rowid = ROWID(vend-whse-trans)
               v-r-no = v-r-no + 1.

            FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company           = tt-vend-whse-trans.company
                                          AND b-vend-whse-item.vendor-code       = tt-vend-whse-trans.vendor-code
                                          AND b-vend-whse-item.vendor-plant-code = tt-vend-whse-trans.vendor-plant-code
                                          AND b-vend-whse-item.fg-item-no        = tt-vend-whse-trans.fg-item-no
                                          AND b-vend-whse-item.cust-part-no      = tt-vend-whse-trans.cust-part-no 
                                          AND b-vend-whse-item.vendor-dept-code  = tt-vend-whse-trans.vendor-dept-code NO-ERROR.
            ASSIGN
               b-vend-whse-item.est-annual-usage   = tt-vend-whse-trans.est-annual-usage 
               b-vend-whse-item.plant-tot-oh-qty   = tt-vend-whse-trans.plant-tot-oh-qty
               b-vend-whse-item.upd-date           = TODAY
               b-vend-whse-item.upd-time           = TIME 
               b-vend-whse-item.upd-userid         = USERID("noswweat").

            IF tt-vend-whse-trans.est-annual-usage = 0 AND b-vend-whse-item.obsolete-date <> ? THEN
               ASSIGN 
                  b-vend-whse-item.obsolete-date = TODAY
                  b-vend-whse-item.obsolete      = YES.
         END.

         MESSAGE "Excel File Import Completed." SKIP
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
      END.
   END.

   OUTPUT CLOSE.

   RUN dispatch ('open-query'). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record B-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  v-prev-recid = RECID(vend-whse-item).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR v-vendor-code AS CHAR NO-UNDO.

   /* Code placed here will execute PRIOR to standard behavior. */

   IF v-copy-rec THEN
      ASSIGN 
         v-cust-no    = CAPS(vend-whse-item.cust-no:SCREEN-VALUE IN BROWSE {&browse-name})
         v-plant-code = CAPS(vend-whse-item.vendor-plant-code:SCREEN-VALUE).
   DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
         v-vendor-code = vend-whse-item.vendor-code:SCREEN-VALUE IN BROWSE {&browse-name}.
   END.

   /* Dispatch standard ADM method.                             */
   RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ).

   ASSIGN 
      v-last-cust-no    = CAPS(vend-whse-item.cust-no)
      v-last-plant-code = CAPS(vend-whse-item.vendor-plant-code)
      vend-whse-item.vendor-code = CAPS(v-vendor-code)
      vend-whse-item.cust-no = CAPS(vend-whse-item.cust-no:SCREEN-VALUE).
   /* Code placed here will execute AFTER standard behavior.    */
   v-copy-rec = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record B-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

   /* Code placed here will execute PRIOR to standard behavior. */
   v-copy-rec = yes.

   /* Dispatch standard ADM method.                             */
   RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .

   /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

   /* Code placed here will execute PRIOR to standard behavior. */

   /* Dispatch standard ADM method.                             */
   RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

   /* Code placed here will execute AFTER standard behavior.    */
   v-recid = RECID(vend-whse-item).



   ASSIGN 
      vend-whse-item.company = gcompany
      vend-whse-item.cust-no = vend-whse-item.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}
      vend-whse-item.rec_key = STRING(YEAR(TODAY), "9999") + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99") + STRING(TIME).

   DISP vend-whse-item.cust-no WITH BROWSE {&browse-name}.


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

  MESSAGE "Delete Currently Selected Record?"
  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE response AS LOGICAL.

  IF NOT response THEN RETURN "ADM-ERROR":U.


  /*  progress bug - no rfqitem record available 
      if add is canceled when new line is appended to last line */

  IF NOT AVAIL vend-whse-item THEN 
     FIND vend-whse-item WHERE RECID(vend-whse-item) = v-recid NO-ERROR.


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
   APPLY 'entry' to vend-whse-item.cust-no IN BROWSE {&BROWSE-NAME}.

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

   IF NOT AVAIL vend-whse-item THEN 
      FIND vend-whse-item WHERE RECID(vend-whse-item) = v-recid NO-ERROR.

   RUN val-cust-no NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN val-fg-item NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN val-cust-part-no NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN val-plant-id NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN val-dept-code NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.


   /* Dispatch standard ADM method.                             */
   RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .


   /* Code placed here will execute AFTER standard behavior.    */


/*    RUN repo-query(INPUT v-recid). */

   DO WITH FRAME {&FRAME-NAME}:
      DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
         APPLY 'cursor-left' TO {&BROWSE-NAME}.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val-cust-no B-table-Win 
PROCEDURE val-cust-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
   IF NOT CAN-FIND(FIRST vend-code-cust-xref WHERE vend-code-cust-xref.company = cocode
                                               AND vend-code-cust-xref.cust-no = vend-whse-item.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}) THEN DO:
      MESSAGE "Invalid Suppliers A/R Code" 
         VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO vend-whse-item.cust-no IN BROWSE {&browse-name}.
      RETURN ERROR.
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val-cust-part-no B-table-Win 
PROCEDURE val-cust-part-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
   IF NOT CAN-FIND(FIRST itemfg
                         {sys/look/itemfgrlW.i}
                     AND itemfg.part-no EQ vend-whse-item.cust-part-no:SCREEN-VALUE IN BROWSE {&browse-name})
      THEN DO:
      MESSAGE "Invalid Customers Part Number    " 
         VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO vend-whse-item.cust-part-no IN BROWSE {&browse-name}.
      RETURN ERROR.
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val-dept-code B-table-Win 
PROCEDURE val-dept-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-vend-plant FOR vend-plant.

DO WITH FRAME {&FRAME-NAME}:
   IF NOT CAN-FIND(FIRST b-vend-plant WHERE b-vend-plant.company = cocode
                                        AND b-vend-plant.plant-id = vend-whse-item.vendor-plant-code:SCREEN-VALUE IN BROWSE {&browse-name}
                                        AND b-vend-plant.vendor-dept-code = vend-whse-item.vendor-dept-code:SCREEN-VALUE IN BROWSE {&browse-name}) THEN DO:
      MESSAGE "Invalid Customers Dept Code      " 
         VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO vend-whse-item.vendor-dept-code IN BROWSE {&browse-name}.
      RETURN ERROR.
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val-fg-item B-table-Win 
PROCEDURE val-fg-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-itemfg FOR itemfg.

DO WITH FRAME {&FRAME-NAME}:
      FIND FIRST b-itemfg WHERE b-itemfg.company = cocode
                            AND b-itemfg.i-no EQ vend-whse-item.fg-item-no:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(b-itemfg) THEN DO:
         MESSAGE "Invalid Suppliers FG Item     " 
            VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO vend-whse-item.fg-item-no IN BROWSE {&browse-name}.
         RETURN ERROR.
      END.
      ELSE DO:
         IF AVAILABLE(b-itemfg) THEN
            ASSIGN vend-whse-item.cust-part-no:SCREEN-VALUE IN BROWSE {&browse-name} = b-itemfg.part-no.    
      END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val-plant-id B-table-Win 
PROCEDURE val-plant-id :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-vend-plant FOR vend-plant.

DO WITH FRAME {&FRAME-NAME}:
   IF NOT CAN-FIND(FIRST b-vend-plant WHERE b-vend-plant.company = cocode
                                        AND b-vend-plant.plant-id = vend-whse-item.vendor-plant-code:SCREEN-VALUE IN BROWSE {&browse-name}) THEN DO:
      MESSAGE "Invalid Customers Plant ID" 
         VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO vend-whse-item.vendor-plant-code IN BROWSE {&browse-name}.
      RETURN ERROR.
   END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val-vend-code B-table-Win 
PROCEDURE val-vend-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
   IF NOT CAN-FIND(FIRST vend-code-cust-xref WHERE vend-code-cust-xref.company = cocode
                                               AND vend-code-cust-xref.cust-no = vend-whse-item.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}
                                               AND vend-code-cust-xref.vendor-code = vend-whse-item.vendor-code:SCREEN-VALUE IN BROWSE {&browse-name})
      THEN DO:
      MESSAGE "Invalid Customers A/P Code    " 
         VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO vend-whse-item.vendor-code IN BROWSE {&browse-name}.
      RETURN ERROR.
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

