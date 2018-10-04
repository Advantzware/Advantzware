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

  File:  b-wiptag.w

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

&SCOPED-DEFINE dataGridInclude dataGrid\jcrep\b-wiptag.i
&SCOPED-DEFINE yellowColumnsName b-wiptag
&SCOPED-DEFINE winReSize
&SCOPED-DEFINE browseOnly
{methods/defines/winReSize.i}

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
&Scoped-define INTERNAL-TABLES wiptag

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table wiptag.tag-no wiptag.rm-bin ~
wiptag.rm-whs wiptag.wip-warehouse wiptag.wip-rm-bin wiptag.job-no ~
wiptag.job-no2 wiptag.rm-i-no wiptag.rm-i-name wiptag.fg-i-no ~
wiptag.fg-i-name wiptag.pallet-count wiptag.rm-tag-no 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH wiptag WHERE ~{&KEY-PHRASE} ~
      AND wiptag.company EQ g_company AND ~
ASI.wiptag.tag-no BEGINS tb_tag-no AND ~
ASI.wiptag.rm-whs BEGINS tb_rm-loc AND ~
ASI.wiptag.rm-bin BEGINS tb_rm-bin AND ~
ASI.wiptag.rm-i-no BEGINS tb_rm-i-no AND ~
ASI.wiptag.rm-i-name BEGINS tb_rm-i-name AND ~
ASI.wiptag.fg-i-no BEGINS tb_fg-i-no AND ~
ASI.wiptag.fg-i-name BEGINS tb_fg-i-name AND ~
(wiptag.job-no EQ tb_job-no OR tb_job-no EQ '') AND ~
(wiptag.job-no2 EQ tb_job-no2 OR tb_job-no EQ '') NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH wiptag WHERE ~{&KEY-PHRASE} ~
      AND wiptag.company EQ g_company AND ~
ASI.wiptag.tag-no BEGINS tb_tag-no AND ~
ASI.wiptag.rm-whs BEGINS tb_rm-loc AND ~
ASI.wiptag.rm-bin BEGINS tb_rm-bin AND ~
ASI.wiptag.rm-i-no BEGINS tb_rm-i-no AND ~
ASI.wiptag.rm-i-name BEGINS tb_rm-i-name AND ~
ASI.wiptag.fg-i-no BEGINS tb_fg-i-no AND ~
ASI.wiptag.fg-i-name BEGINS tb_fg-i-name AND ~
(wiptag.job-no EQ tb_job-no OR tb_job-no EQ '') AND ~
(wiptag.job-no2 EQ tb_job-no2 OR tb_job-no EQ '') NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table wiptag
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table wiptag


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-4 tb_tag-no tb_rm-loc tb_rm-bin ~
tb_job-no tb_job-no2 tb_rm-i-no tb_rm-i-name tb_fg-i-no tb_fg-i-name ~
browse-order auto_find Btn_Clear_Find Browser-Table 
&Scoped-Define DISPLAYED-OBJECTS tb_tag-no tb_rm-loc tb_rm-bin tb_job-no ~
tb_job-no2 tb_rm-i-no tb_rm-i-name tb_fg-i-no tb_fg-i-name browse-order ~
fi_sortby auto_find 

/* Custom List Definitions                                              */
/* filterFields,List-2,List-3,List-4,List-5,List-6                      */
&Scoped-define filterFields tb_tag-no tb_rm-loc tb_rm-bin tb_job-no ~
tb_job-no2 tb_rm-i-no tb_rm-i-name tb_fg-i-no tb_fg-i-name 

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
     SIZE 22 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE tb_fg-i-name AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 19.8 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE tb_fg-i-no AS CHARACTER FORMAT "x(15)" 
     VIEW-AS FILL-IN 
     SIZE 15.8 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE tb_job-no AS CHARACTER FORMAT "x(6)" 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE tb_job-no2 AS INTEGER FORMAT ">9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4.8 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE tb_rm-bin AS CHARACTER FORMAT "x(8)" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE tb_rm-i-name AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 19.4 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE tb_rm-i-no AS CHARACTER FORMAT "x(15)" 
     VIEW-AS FILL-IN 
     SIZE 15.8 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE tb_rm-loc AS CHARACTER FORMAT "x(5)" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE tb_tag-no AS CHARACTER FORMAT "X(20)" 
     VIEW-AS FILL-IN 
     SIZE 30.8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 63 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.91.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      wiptag
    FIELDS(wiptag.tag-no
      wiptag.rm-bin
      wiptag.rm-whs
      wiptag.wip-warehouse
      wiptag.wip-rm-bin
      wiptag.job-no
      wiptag.job-no2
      wiptag.rm-i-no
      wiptag.rm-i-name
      wiptag.fg-i-no
      wiptag.fg-i-name
      wiptag.pallet-count
      wiptag.rm-tag-no) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      wiptag.tag-no FORMAT "X(20)":U WIDTH 26 LABEL-BGCOLOR 14
      wiptag.rm-bin FORMAT "x(8)":U WIDTH 11 LABEL-BGCOLOR 14
      wiptag.rm-whs COLUMN-LABEL "RM Whs" FORMAT "x(5)":U WIDTH 11
            LABEL-BGCOLOR 14
      wiptag.wip-warehouse COLUMN-LABEL "WIP Whs" FORMAT "x(5)":U
            WIDTH 11 LABEL-BGCOLOR 14
      wiptag.wip-rm-bin COLUMN-LABEL "WIP Bin" FORMAT "x(8)":U
            WIDTH 12 LABEL-BGCOLOR 14
      wiptag.job-no COLUMN-LABEL "Job" FORMAT "x(6)":U WIDTH 10
            LABEL-BGCOLOR 14
      wiptag.job-no2 COLUMN-LABEL "#" FORMAT ">9":U WIDTH 3.4
      wiptag.rm-i-no FORMAT "x(15)":U LABEL-BGCOLOR 14
      wiptag.rm-i-name COLUMN-LABEL "RM Name" FORMAT "x(30)":U
            LABEL-BGCOLOR 14
      wiptag.fg-i-no FORMAT "x(15)":U LABEL-BGCOLOR 14
      wiptag.fg-i-name COLUMN-LABEL "FG Name" FORMAT "x(30)":U
            LABEL-BGCOLOR 14
      wiptag.pallet-count COLUMN-LABEL "Tag Qty." FORMAT "->,>>>,>>9":U
            WIDTH 19 LABEL-BGCOLOR 14
      wiptag.rm-tag-no COLUMN-LABEL "RM Tag#" FORMAT "X(20)":U
            WIDTH 26
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 16.19
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     tb_tag-no AT ROW 1.71 COL 2 NO-LABEL
     tb_rm-loc AT ROW 1.71 COL 31 COLON-ALIGNED HELP
          "Enter the plant/warehouse location" NO-LABEL
     tb_rm-bin AT ROW 1.71 COL 45 COLON-ALIGNED HELP
          "Enter Bin Location where Item is Stocked" NO-LABEL
     tb_job-no AT ROW 1.71 COL 57 COLON-ALIGNED HELP
          "Job Number." NO-LABEL
     tb_job-no2 AT ROW 1.71 COL 66 COLON-ALIGNED HELP
          "Enter Job sub-number." NO-LABEL
     tb_rm-i-no AT ROW 1.71 COL 71.4 COLON-ALIGNED HELP
          "Enter Item Number." NO-LABEL
     tb_rm-i-name AT ROW 1.71 COL 87.6 COLON-ALIGNED HELP
          "Enter finished goods item name." NO-LABEL
     tb_fg-i-no AT ROW 1.71 COL 107 COLON-ALIGNED HELP
          "Enter Item Number." NO-LABEL WIDGET-ID 2
     tb_fg-i-name AT ROW 1.71 COL 123.2 COLON-ALIGNED HELP
          "Enter finished goods item name." NO-LABEL WIDGET-ID 6
     browse-order AT ROW 3.14 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     fi_sortby AT ROW 3.14 COL 78 COLON-ALIGNED
     auto_find AT ROW 3.14 COL 111 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 3.14 COL 132 HELP
          "CLEAR AUTO FIND Value"
     Browser-Table AT ROW 4.33 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     "FG Item" VIEW-AS TEXT
          SIZE 9.2 BY .62 AT ROW 1.05 COL 112.6 WIDGET-ID 4
          FGCOLOR 9 FONT 6
     "RM Name" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 1.05 COL 93.8
          FGCOLOR 9 FONT 6
     "RM Item" VIEW-AS TEXT
          SIZE 10.6 BY .62 AT ROW 1.05 COL 76.6
          FGCOLOR 9 FONT 6
     "RM Bin" VIEW-AS TEXT
          SIZE 8.8 BY .62 AT ROW 1.05 COL 48.6
          FGCOLOR 9 FONT 6
     "Tag#" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1.05 COL 16
          FGCOLOR 9 FONT 6
     "RM Whs" VIEW-AS TEXT
          SIZE 10.6 BY .62 AT ROW 1.05 COL 34.6
          FGCOLOR 9 FONT 6
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 3.14 COL 2
     "Job" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1.05 COL 61.6
          FGCOLOR 9 FONT 6
     "FG Name" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1.05 COL 129.2 WIDGET-ID 8
          FGCOLOR 9 FONT 6
     RECT-4 AT ROW 2.91 COL 1
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

{Advantzware/WinKit/dataGridProc.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB Browser-Table Btn_Clear_Find F-Main */
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
/* SETTINGS FOR FILL-IN tb_fg-i-name IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN tb_fg-i-no IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN tb_job-no IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN tb_job-no2 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN tb_rm-bin IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN tb_rm-i-name IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN tb_rm-i-no IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN tb_rm-loc IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN tb_tag-no IN FRAME F-Main
   ALIGN-L 1                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.wiptag"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED"
     _Where[1]         = "ASI.wiptag.company EQ g_company AND
ASI.wiptag.tag-no BEGINS tb_tag-no AND
ASI.wiptag.rm-whs BEGINS tb_rm-loc AND
ASI.wiptag.rm-bin BEGINS tb_rm-bin AND
ASI.wiptag.rm-i-no BEGINS tb_rm-i-no AND
ASI.wiptag.rm-i-name BEGINS tb_rm-i-name AND
ASI.wiptag.fg-i-no BEGINS tb_fg-i-no AND
ASI.wiptag.fg-i-name BEGINS tb_fg-i-name AND
(wiptag.job-no EQ tb_job-no OR tb_job-no EQ '') AND
(wiptag.job-no2 EQ tb_job-no2 OR tb_job-no EQ '')"
     _FldNameList[1]   > ASI.wiptag.tag-no
"wiptag.tag-no" ? ? "character" ? ? ? 14 ? ? no ? no no "26" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.wiptag.rm-bin
"wiptag.rm-bin" ? ? "character" ? ? ? 14 ? ? no ? no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.wiptag.rm-whs
"wiptag.rm-whs" "RM Whs" ? "character" ? ? ? 14 ? ? no ? no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.wiptag.wip-warehouse
"wiptag.wip-warehouse" "WIP Whs" ? "character" ? ? ? 14 ? ? no ? no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.wiptag.wip-rm-bin
"wiptag.wip-rm-bin" "WIP Bin" ? "character" ? ? ? 14 ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.wiptag.job-no
"wiptag.job-no" "Job" ? "character" ? ? ? 14 ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.wiptag.job-no2
"wiptag.job-no2" "#" ? "integer" ? ? ? ? ? ? no "" no no "3.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.wiptag.rm-i-no
"wiptag.rm-i-no" ? ? "character" ? ? ? 14 ? ? no "" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.wiptag.rm-i-name
"wiptag.rm-i-name" "RM Name" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.wiptag.fg-i-no
"wiptag.fg-i-no" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.wiptag.fg-i-name
"wiptag.fg-i-name" "FG Name" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.wiptag.pallet-count
"wiptag.pallet-count" "Tag Qty." ? "integer" ? ? ? 14 ? ? no ? no no "19" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.wiptag.rm-tag-no
"wiptag.rm-tag-no" "RM Tag#" ? "character" ? ? ? ? ? ? no ? no no "26" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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


&Scoped-define SELF-NAME tb_fg-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fg-i-no B-table-Win
ON HELP OF tb_fg-i-no IN FRAME F-Main
DO:
   def var char-val as cha no-undo.
   DEF VAR help-recid AS RECID NO-UNDO.
   RUN windows/l-itemfg.w (cocode,"","",OUTPUT char-val) NO-ERROR.
         IF char-val <> "" THEN DO:
             ASSIGN 
                 tb_fg-i-no:SCREEN-VALUE = ENTRY(1,char-val).
                APPLY "entry" TO tb_fg-i-no.
         END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_job-no B-table-Win
ON HELP OF tb_job-no IN FRAME F-Main
DO:
   def var char-val as cha no-undo.
   DEF VAR help-recid AS RECID NO-UNDO.
  run windows/l-jobno.w (cocode,FOCUS:SCREEN-VALUE, output char-val, OUTPUT help-recid).
         if char-val <> "" then do:
            assign
             focus:screen-value in browse {&BROWSE-NAME} = ENTRY(1,char-val)
             tb_job-no2:screen-value = ENTRY(2,char-val).

         END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_rm-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_rm-bin B-table-Win
ON HELP OF tb_rm-bin IN FRAME F-Main
DO:
   def var char-val as cha no-undo.
   DEF VAR help-recid AS RECID NO-UNDO.
    run rm/l-locbin.w (cocode,tb_rm-loc:screen-value, output char-val).
         IF char-val <> "" THEN DO:
             ASSIGN 
                 tb_rm-bin:SCREEN-VALUE = ENTRY(1,char-val).
                APPLY "entry" TO tb_rm-bin.
         END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_rm-i-name
&Scoped-define SELF-NAME tb_rm-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_rm-i-no B-table-Win
ON HELP OF tb_rm-i-no IN FRAME F-Main
DO:
   def var char-val as cha no-undo.
   DEF VAR help-recid AS RECID NO-UNDO.
  run windows/l-item.w (cocode,"","B,P",tb_rm-i-no:screen-value , output char-val).
        if char-val <> "" then do:
          /*FIND ITEM WHERE RECID(ITEM) EQ int(char-val) NO-LOCK NO-ERROR.
          IF AVAIL ITEM THEN DO:*/
            tb_rm-i-no:SCREEN-VALUE = ENTRY(1,char-val) .
          /*END.*/
         END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_rm-loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_rm-loc B-table-Win
ON HELP OF tb_rm-loc IN FRAME F-Main
DO:
   def var char-val as cha no-undo.
   DEF VAR help-recid AS RECID NO-UNDO.
  run rm/l-loc.w (cocode,tb_rm-loc:screen-value, output char-val).
         IF char-val <> "" THEN DO:
             ASSIGN 
                 tb_rm-loc:SCREEN-VALUE = ENTRY(1,char-val).
                APPLY "entry" TO tb_rm-loc .
         END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_tag-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_tag-no B-table-Win
ON HELP OF tb_tag-no IN FRAME F-Main
DO:
   def var char-val as cha no-undo.
   DEF VAR help-recid AS RECID NO-UNDO.
   run windows/l-wptag1.w (cocode, focus:screen-value, output char-val, OUTPUT help-recid).
         if char-val <> "" then do:
            assign
             focus:screen-value in browse {&BROWSE-NAME} = ENTRY(1,char-val).

         END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_tag-no B-table-Win
ON RETURN OF tb_tag-no IN FRAME F-Main
,tb_rm-loc,tb_rm-bin,tb_job-no,tb_job-no2,tb_rm-i-no,tb_rm-i-name,tb_fg-i-no,tb_fg-i-name
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
  {src/adm/template/snd-list.i "wiptag"}

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

