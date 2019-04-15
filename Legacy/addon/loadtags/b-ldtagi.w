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

&SCOPED-DEFINE yellowColumnsName b-lgtag

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}

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
&Scoped-define INTERNAL-TABLES loadtag

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table loadtag.tag-no loadtag.loc ~
loadtag.loc-bin loadtag.job-no loadtag.job-no2 loadtag.po-no loadtag.ord-no ~
loadtag.i-no loadtag.i-name loadtag.qty-case loadtag.case-bundle ~
loadtag.pallet-count loadtag.partial loadtag.tot-cases loadtag.misc-char[1] 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH loadtag WHERE ~{&KEY-PHRASE} ~
      AND loadtag.company EQ g_company ~
AND loadtag.item-type EQ YES /* RM Item */ ~
AND loadtag.is-case-tag EQ NO ~
AND loadtag.tag-no BEGINS tb_tag-no ~
AND loadtag.loc BEGINS tb_loc ~
AND loadtag.loc-bin BEGINS tb_loc-bin ~
AND (loadtag.job-no EQ tb_job-no OR tb_job-no EQ '') ~
AND (loadtag.job-no2 EQ tb_job-no2 OR tb_job-no EQ '') ~
AND (loadtag.po-no EQ tb_po-no OR tb_po-no EQ 0) ~
AND (loadtag.ord-no EQ tb_ord-no OR tb_ord-no EQ 0) ~
AND loadtag.i-no BEGINS tb_i-no ~
AND loadtag.i-name BEGINS tb_i-name ~
AND loadtag.misc-char[1] BEGINS tb_vend-tag NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH loadtag WHERE ~{&KEY-PHRASE} ~
      AND loadtag.company EQ g_company ~
AND loadtag.item-type EQ YES /* RM Item */ ~
AND loadtag.is-case-tag EQ NO ~
AND loadtag.tag-no BEGINS tb_tag-no ~
AND loadtag.loc BEGINS tb_loc ~
AND loadtag.loc-bin BEGINS tb_loc-bin ~
AND (loadtag.job-no EQ tb_job-no OR tb_job-no EQ '') ~
AND (loadtag.job-no2 EQ tb_job-no2 OR tb_job-no EQ '') ~
AND (loadtag.po-no EQ tb_po-no OR tb_po-no EQ 0) ~
AND (loadtag.ord-no EQ tb_ord-no OR tb_ord-no EQ 0) ~
AND loadtag.i-no BEGINS tb_i-no ~
AND loadtag.i-name BEGINS tb_i-name ~
AND loadtag.misc-char[1] BEGINS tb_vend-tag NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table loadtag
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table loadtag


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-4 tb_tag-no tb_loc tb_loc-bin tb_job-no ~
tb_job-no2 tb_po-no tb_ord-no tb_i-no tb_i-name tb_vend-tag Browser-Table ~
browse-order auto_find Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS tb_tag-no tb_loc tb_loc-bin tb_job-no ~
tb_job-no2 tb_po-no tb_ord-no tb_i-no tb_i-name tb_vend-tag browse-order ~
fi_sortby auto_find 

/* Custom List Definitions                                              */
/* filterFields,List-2,List-3,List-4,List-5,List-6                      */
&Scoped-define filterFields tb_tag-no tb_loc tb_loc-bin tb_job-no ~
tb_job-no2 tb_po-no tb_ord-no tb_i-no tb_i-name tb_vend-tag 

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
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sorted By" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE tb_i-name AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE tb_i-no AS CHARACTER FORMAT "x(15)" 
     VIEW-AS FILL-IN 
     SIZE 21.8 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE tb_job-no AS CHARACTER FORMAT "x(6)" 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE tb_job-no2 AS INTEGER FORMAT ">9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 3.8 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE tb_loc AS CHARACTER FORMAT "x(5)" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE tb_loc-bin AS CHARACTER FORMAT "x(8)" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE tb_ord-no AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.8 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE tb_po-no AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.3 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE tb_tag-no AS CHARACTER FORMAT "X(20)" 
     VIEW-AS FILL-IN 
     SIZE 31.8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_vend-tag AS CHARACTER FORMAT "X(20)" 
     VIEW-AS FILL-IN 
     SIZE 31.8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 55 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 3.1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      loadtag
    FIELDS(loadtag.tag-no
      loadtag.loc
      loadtag.loc-bin
      loadtag.job-no
      loadtag.job-no2
      loadtag.po-no
      loadtag.ord-no
      loadtag.i-no
      loadtag.i-name
      loadtag.qty-case
      loadtag.case-bundle
      loadtag.pallet-count
      loadtag.partial
      loadtag.tot-cases
      loadtag.misc-char[1]) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      loadtag.tag-no COLUMN-LABEL "Tag" FORMAT "X(23)":U LABEL-BGCOLOR 14
      loadtag.loc FORMAT "x(5)":U LABEL-BGCOLOR 14
      loadtag.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U LABEL-BGCOLOR 14
      loadtag.job-no COLUMN-LABEL "Job" FORMAT "x(6)":U LABEL-BGCOLOR 14
      loadtag.job-no2 COLUMN-LABEL "#" FORMAT ">9":U
      loadtag.po-no COLUMN-LABEL "PO" FORMAT ">>>>>9":U LABEL-BGCOLOR 14
      loadtag.ord-no COLUMN-LABEL "Order" FORMAT ">>>>>9":U LABEL-BGCOLOR 14
      loadtag.i-no COLUMN-LABEL "Item" FORMAT "x(15)":U LABEL-BGCOLOR 14
      loadtag.i-name FORMAT "x(30)":U LABEL-BGCOLOR 14
      loadtag.qty-case COLUMN-LABEL "Unit!Count" FORMAT "->,>>>,>>9":U
      loadtag.case-bundle COLUMN-LABEL "Units/!Pallet" FORMAT "->,>>>,>>9":U
      loadtag.pallet-count COLUMN-LABEL "Qty Per Pallet/Tag#" FORMAT "->,>>>,>>9":U
      loadtag.partial COLUMN-LABEL "Partial" FORMAT ">>>,>>9":U
      loadtag.tot-cases FORMAT "->,>>>,>>9":U
      loadtag.misc-char[1] COLUMN-LABEL "Vendor Tag#" FORMAT "x(23)":U
            LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 14.76
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     tb_tag-no AT ROW 1.71 COL 2 NO-LABEL
     tb_loc AT ROW 1.71 COL 32 COLON-ALIGNED HELP
          "Enter the plant/warehouse location" NO-LABEL
     tb_loc-bin AT ROW 1.71 COL 46 COLON-ALIGNED HELP
          "Enter Bin Location where Item is Stocked" NO-LABEL
     tb_job-no AT ROW 1.71 COL 58 COLON-ALIGNED HELP
          "Job Number." NO-LABEL
     tb_job-no2 AT ROW 1.71 COL 67 COLON-ALIGNED HELP
          "Enter Job sub-number." NO-LABEL
     tb_po-no AT ROW 1.71 COL 71 COLON-ALIGNED NO-LABEL
     tb_ord-no AT ROW 1.71 COL 80 COLON-ALIGNED NO-LABEL
     tb_i-no AT ROW 1.71 COL 89 COLON-ALIGNED HELP
          "Enter Item Number." NO-LABEL
     tb_i-name AT ROW 1.71 COL 111 COLON-ALIGNED HELP
          "Enter finished goods item name." NO-LABEL
     tb_vend-tag AT ROW 2.86 COL 2 NO-LABEL WIDGET-ID 2
     Browser-Table AT ROW 4.33 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 19.33 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     fi_sortby AT ROW 19.33 COL 70 COLON-ALIGNED
     auto_find AT ROW 19.33 COL 111 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 19.33 COL 132 HELP
          "CLEAR AUTO FIND Value"
     "Job" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1.05 COL 64
          FGCOLOR 9 FONT 6
     "PO" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 1.05 COL 75
          FGCOLOR 9 FONT 6
     "Order" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.05 COL 83
          FGCOLOR 9 FONT 6
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 19.33 COL 2
     "Warehouse" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 1.05 COL 34
          FGCOLOR 9 FONT 6
     "Tag#/Vendor Tag#" VIEW-AS TEXT
          SIZE 23 BY .62 AT ROW 1.05 COL 7.2
          FGCOLOR 9 FONT 6
     "Bin" VIEW-AS TEXT
          SIZE 4 BY .62 AT ROW 1.05 COL 52
          FGCOLOR 9 FONT 6
     "Item" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1.05 COL 98
          FGCOLOR 9 FONT 6
     "Name" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1.05 COL 126
          FGCOLOR 9 FONT 6
     RECT-4 AT ROW 19.1 COL 1
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
{custom/yellowColumns.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB Browser-Table tb_vend-tag F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "2"
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR RECTANGLE RECT-5 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tb_i-name IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN tb_i-no IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN tb_job-no IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN tb_job-no2 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN tb_loc IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN tb_loc-bin IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN tb_ord-no IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN tb_po-no IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN tb_tag-no IN FRAME F-Main
   ALIGN-L 1                                                            */
/* SETTINGS FOR FILL-IN tb_vend-tag IN FRAME F-Main
   ALIGN-L 1                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.loadtag"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED"
     _Where[1]         = "ASI.loadtag.company EQ g_company
AND loadtag.item-type EQ YES /* RM Item */
AND loadtag.is-case-tag EQ NO
AND loadtag.tag-no BEGINS tb_tag-no
AND loadtag.loc BEGINS tb_loc
AND loadtag.loc-bin BEGINS tb_loc-bin
AND (loadtag.job-no EQ tb_job-no OR tb_job-no EQ '')
AND (loadtag.job-no2 EQ tb_job-no2 OR tb_job-no EQ '')
AND (loadtag.po-no EQ tb_po-no OR tb_po-no EQ 0)
AND (loadtag.ord-no EQ tb_ord-no OR tb_ord-no EQ 0)
AND loadtag.i-no BEGINS tb_i-no
AND loadtag.i-name BEGINS tb_i-name
AND loadtag.misc-char[1] BEGINS tb_vend-tag"
     _FldNameList[1]   > ASI.loadtag.tag-no
"tag-no" "Tag" "X(23)" "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.loadtag.loc
"loc" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.loadtag.loc-bin
"loc-bin" "Bin" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.loadtag.job-no
"job-no" "Job" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.loadtag.job-no2
"job-no2" "#" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.loadtag.po-no
"po-no" "PO" ? "integer" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.loadtag.ord-no
"ord-no" "Order" ? "integer" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.loadtag.i-no
"i-no" "Item" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.loadtag.i-name
"i-name" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.loadtag.qty-case
"qty-case" "Unit!Count" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.loadtag.case-bundle
"case-bundle" "Units/!Pallet" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.loadtag.pallet-count
"pallet-count" "Qty Per Pallet/Tag#" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.loadtag.partial
"partial" "Partial" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   = ASI.loadtag.tot-cases
     _FldNameList[15]   > ASI.loadtag.misc-char[1]
"misc-char[1]" "Vendor Tag#" "x(23)" "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_tag-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_tag-no B-table-Win
ON RETURN OF tb_tag-no IN FRAME F-Main
,tb_loc,tb_loc-bin,tb_job-no,tb_job-no2,tb_po-no,tb_ord-no,tb_i-no,tb_i-name,tb_vend-tag
DO:
  ASSIGN {&filterFields}.
  IF tb_job-no NE '' THEN
     tb_job-no = FILL(' ',6 - LENGTH(TRIM(tb_job-no))) + TRIM(tb_job-no).
  RUN openQuery.
  APPLY 'VALUE-CHANGED':U TO BROWSE {&BROWSE-NAME}.
  APPLY 'ENTRY':U TO BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_vend-tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_vend-tag B-table-Win
ON RETURN OF tb_vend-tag IN FRAME F-Main
,tb_loc,tb_loc-bin,tb_job-no,tb_job-no2,tb_po-no,tb_ord-no,tb_i-no,tb_i-name
DO:
  ASSIGN {&filterFields}.
  IF tb_job-no NE '' THEN
  tb_job-no = FILL(' ',6 - LENGTH(TRIM(tb_job-no))) + TRIM(tb_job-no).
  RUN openQuery.
  APPLY 'VALUE-CHANGED':U TO BROWSE {&BROWSE-NAME}.
  APPLY 'ENTRY':U TO BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-is-case-tag B-table-Win 
PROCEDURE get-is-case-tag :
DEF OUTPUT PARAM op-is-case-tag LIKE loadtag.is-case-tag INIT NO NO-UNDO.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-item-type B-table-Win 
PROCEDURE get-item-type :
DEF OUTPUT PARAM op-item-type LIKE loadtag.item-type INIT YES NO-UNDO.
       
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
  {src/adm/template/snd-list.i "loadtag"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadtag-rm B-table-Win 
PROCEDURE loadtag-rm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF AVAIL loadtag THEN
        RUN rmrep/rmloadtg4.w(INPUT YES,INPUT loadtag.tag-no)  .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
