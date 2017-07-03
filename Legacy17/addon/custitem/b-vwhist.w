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
&SCOPED-DEFINE dataGridInclude dataGrid\addon\custitem\b-vwhist.i
&SCOPED-DEFINE yellowColumnsName vend-whse-trans-hist
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}

ASSIGN
 cocode = g_company.

&SCOPED-DEFINE item-key-phrase TRUE

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target,Navigation-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Browser-Table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES vend-whse-trans-hist

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table vend-whse-trans-hist.r-no ~
vend-whse-trans-hist.trans-type vend-whse-trans-hist.trans-date ~
vend-whse-trans-hist.trans-qty vend-whse-trans-hist.item-po-no ~
vend-whse-trans-hist.item-line-no vend-whse-trans-hist.cust-part-no ~
vend-whse-trans-hist.fg-item-no vend-whse-trans-hist.vendor-code ~
vend-whse-trans-hist.vendor-plant-code ~
vend-whse-trans-hist.vendor-dept-code vend-whse-trans-hist.vend-ord-no ~
vend-whse-trans-hist.vend-job-no vend-whse-trans-hist.vend-job-no2 ~
vend-whse-trans-hist.sell-price vend-whse-trans-hist.plant-tot-oh-qty 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH vend-whse-trans-hist WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH vend-whse-trans-hist WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table vend-whse-trans-hist
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table vend-whse-trans-hist


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
     SIZE 119.2 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 100 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 2.86.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      vend-whse-trans-hist
    FIELDS(vend-whse-trans-hist.r-no
      vend-whse-trans-hist.trans-type
      vend-whse-trans-hist.trans-date
      vend-whse-trans-hist.trans-qty
      vend-whse-trans-hist.item-po-no
      vend-whse-trans-hist.item-line-no
      vend-whse-trans-hist.cust-part-no
      vend-whse-trans-hist.fg-item-no
      vend-whse-trans-hist.vendor-code
      vend-whse-trans-hist.vendor-plant-code
      vend-whse-trans-hist.vendor-dept-code
      vend-whse-trans-hist.vend-ord-no
      vend-whse-trans-hist.vend-job-no
      vend-whse-trans-hist.vend-job-no2
      vend-whse-trans-hist.sell-price
      vend-whse-trans-hist.plant-tot-oh-qty) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      vend-whse-trans-hist.r-no COLUMN-LABEL "Seq#" FORMAT ">>>>>>>9":U
            LABEL-BGCOLOR 14
      vend-whse-trans-hist.trans-type COLUMN-LABEL "Trans!Type" FORMAT "x(1)":U
            WIDTH 7.8 LABEL-BGCOLOR 14
      vend-whse-trans-hist.trans-date COLUMN-LABEL "Trans!Date" FORMAT "99/99/99":U
            WIDTH 10.8 LABEL-BGCOLOR 14
      vend-whse-trans-hist.trans-qty COLUMN-LABEL "Trans!Quantity" FORMAT "->>,>>>,>>9.9<<":U
            WIDTH 11.6 LABEL-BGCOLOR 14
      vend-whse-trans-hist.item-po-no FORMAT "x(9)":U WIDTH 13
            LABEL-BGCOLOR 14
      vend-whse-trans-hist.item-line-no FORMAT "99":U WIDTH 13.4
            LABEL-BGCOLOR 14
      vend-whse-trans-hist.cust-part-no FORMAT "x(12)":U WIDTH 13.4
            LABEL-BGCOLOR 14
      vend-whse-trans-hist.fg-item-no COLUMN-LABEL "Suppliers!FG Item" FORMAT "x(15)":U
            WIDTH 14 LABEL-BGCOLOR 14
      vend-whse-trans-hist.vendor-code COLUMN-LABEL "Custmers!A/P Code" FORMAT "x(8)":U
            WIDTH 12.8 LABEL-BGCOLOR 14
      vend-whse-trans-hist.vendor-plant-code FORMAT "x(8)":U WIDTH 13.2
            LABEL-BGCOLOR 14
      vend-whse-trans-hist.vendor-dept-code FORMAT "x(8)":U WIDTH 13.2
            LABEL-BGCOLOR 14
      vend-whse-trans-hist.vend-ord-no COLUMN-LABEL "Suppliers!Order#" FORMAT ">>>>>9":U
            WIDTH 13.4 LABEL-BGCOLOR 14
      vend-whse-trans-hist.vend-job-no COLUMN-LABEL "Suppliers!Job#" FORMAT "x(6)":U
            WIDTH 13.2 LABEL-BGCOLOR 14
      vend-whse-trans-hist.vend-job-no2 FORMAT ">9":U
      vend-whse-trans-hist.sell-price COLUMN-LABEL "Suppliers Item!Sell Price" FORMAT ">,>>>,>>9.99<<<<":U
            WIDTH 20 LABEL-BGCOLOR 14
      vend-whse-trans-hist.plant-tot-oh-qty FORMAT "->>,>>>,>>9.9<<":U
            WIDTH 16.2 LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 16.43
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 17.81 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     fi_sortby AT ROW 17.91 COL 105 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     auto_find AT ROW 19.33 COL 11 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 19.33 COL 132 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 17.81 COL 2
     RECT-4 AT ROW 17.67 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0  WIDGET-ID 100.


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
         WIDTH              = 145.
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
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB Browser-Table TEXT-1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 4
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE
       Browser-Table:COLUMN-RESIZABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fi_sortby:HIDDEN IN FRAME F-Main           = TRUE
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "vend-whse-trans-hist"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED"
     _FldNameList[1]   > vend-whse-trans-hist.r-no
"vend-whse-trans-hist.r-no" "Seq#" ? "integer" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > vend-whse-trans-hist.trans-type
"vend-whse-trans-hist.trans-type" "Trans!Type" ? "character" ? ? ? 14 ? ? no ? no no "7.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > vend-whse-trans-hist.trans-date
"vend-whse-trans-hist.trans-date" "Trans!Date" ? "date" ? ? ? 14 ? ? no ? no no "10.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > vend-whse-trans-hist.trans-qty
"vend-whse-trans-hist.trans-qty" "Trans!Quantity" ? "decimal" ? ? ? 14 ? ? no ? no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > vend-whse-trans-hist.item-po-no
"vend-whse-trans-hist.item-po-no" ? ? "character" ? ? ? 14 ? ? no ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > vend-whse-trans-hist.item-line-no
"vend-whse-trans-hist.item-line-no" ? ? "integer" ? ? ? 14 ? ? no ? no no "13.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > vend-whse-trans-hist.cust-part-no
"vend-whse-trans-hist.cust-part-no" ? ? "character" ? ? ? 14 ? ? no ? no no "13.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > vend-whse-trans-hist.fg-item-no
"vend-whse-trans-hist.fg-item-no" "Suppliers!FG Item" ? "character" ? ? ? 14 ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > vend-whse-trans-hist.vendor-code
"vend-whse-trans-hist.vendor-code" "Custmers!A/P Code" ? "character" ? ? ? 14 ? ? no ? no no "12.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > vend-whse-trans-hist.vendor-plant-code
"vend-whse-trans-hist.vendor-plant-code" ? ? "character" ? ? ? 14 ? ? no ? no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > vend-whse-trans-hist.vendor-dept-code
"vend-whse-trans-hist.vendor-dept-code" ? ? "character" ? ? ? 14 ? ? no ? no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > vend-whse-trans-hist.vend-ord-no
"vend-whse-trans-hist.vend-ord-no" "Suppliers!Order#" ? "integer" ? ? ? 14 ? ? no ? no no "13.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > vend-whse-trans-hist.vend-job-no
"vend-whse-trans-hist.vend-job-no" "Suppliers!Job#" ? "character" ? ? ? 14 ? ? no ? no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   = vend-whse-trans-hist.vend-job-no2
     _FldNameList[15]   > vend-whse-trans-hist.sell-price
"vend-whse-trans-hist.sell-price" "Suppliers Item!Sell Price" ? "decimal" ? ? ? 14 ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > vend-whse-trans-hist.plant-tot-oh-qty
"vend-whse-trans-hist.plant-tot-oh-qty" ? ? "decimal" ? ? ? 14 ? ? no ? no no "16.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
   DEF VAR phandle AS HANDLE NO-UNDO.
   DEF VAR char-hdl AS cha NO-UNDO.

   {methods/run_link.i "container-source" "select-page" "(2)"}  
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
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
  {src/adm/template/snd-list.i "vend-whse-trans-hist"}

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

