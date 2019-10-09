&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
/* Procedure Description
"ASI SmartNavBrowser Object Template with Wizard.

Use this template to create a new SmartNavBrowser object with the assistance of the SmartBrowser Wizard. When completed, this object can then be drawn onto any 'smart' container such as a SmartWindow, SmartDialog or SmartFrame."
*/
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

&SCOPED-DEFINE winReSize
&SCOPED-DEFINE browseOnly
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR ll-first AS LOG INIT YES NO-UNDO.
DEF VAR lv-sort-by AS CHAR INIT "job-no" NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "Job#" NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.
DEF VAR ld-cons-qty AS DEC NO-UNDO.
DEF VAR ld-cons-uom AS CHAR NO-UNDO.

&SCOPED-DEFINE key-phrase job.company EQ cocode AND job.opened EQ YES

&SCOPED-DEFINE for-each1                       ~
    FOR EACH job                               ~
        WHERE {&key-phrase}                    ~
          AND job.job-no     BEGINS fi_job-no  ~
          AND (job.job-no2   EQ fi_job-no2 OR fi_job-no2 EQ 0 OR fi_job-no EQ "")

&SCOPED-DEFINE for-each2                     ~
    EACH job-mat OF job                      ~
    WHERE job-mat.all-flg EQ YES             ~
      AND job-mat.rm-i-no BEGINS fi_rm-i-no  ~
      AND (job-mat.i-no EQ fi_rm-i-no OR fi_rm-i-no EQ "") ~
    NO-LOCK

&SCOPED-DEFINE sortby-log                                                                        ~
    IF lv-sort-by EQ "rm-i-no"    THEN job-mat.rm-i-no                                      ELSE ~
    IF lv-sort-by EQ "qty-uom"    THEN job-mat.qty-uom                                      ELSE ~
    IF lv-sort-by EQ "sc-uom"     THEN job-mat.sc-uom                                       ELSE ~
    IF lv-sort-by EQ "frm"        THEN STRING(job-mat.frm,"9999999999")                     ELSE ~
    IF lv-sort-by EQ "blank-no"   THEN STRING(job-mat.blank-no,"9999999999")                ELSE ~
    IF lv-sort-by EQ "qty"        THEN STRING(job-mat.qty,"-9999999999.99999")              ELSE ~
    IF lv-sort-by EQ "qty-all"    THEN STRING(job-mat.qty-all,"-9999999999.99999")          ELSE ~
    IF lv-sort-by EQ "wid"        THEN STRING(job-mat.wid,"-9999999999.99999")              ELSE ~
    IF lv-sort-by EQ "due-date"   THEN STRING(job.due-date)              ELSE ~
    IF lv-sort-by EQ "len"        THEN STRING(job-mat.len,"-9999999999.99999")              ELSE ~
                                       STRING(job-mat.job-no,"x(6)") + STRING(job-mat.job-no2,"99")

&SCOPED-DEFINE sortby BY job-mat.rm-i-no BY job-mat.job-no BY job-mat.job-no2

&SCOPED-DEFINE sortby-phrase-asc  ~
    BY ({&sortby-log})            ~
    {&sortby}

&SCOPED-DEFINE sortby-phrase-desc ~
    BY ({&sortby-log}) DESC       ~
    {&sortby}

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

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES item
&Scoped-define FIRST-EXTERNAL-TABLE item


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR item.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES job job-mat

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table job-mat.rm-i-no job-mat.job-no ~
job-mat.job-no2 job-mat.frm job-mat.blank-no job-mat.qty job-mat.qty-all ~
job-mat.qty-uom cons-qty () @ ld-cons-qty cons-uom() @ ld-cons-uom ~
job-mat.wid job-mat.len job.due-date 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table job-mat.rm-i-no ~
job-mat.job-no job-mat.job-no2 job-mat.frm job-mat.blank-no job-mat.qty ~
job-mat.qty-all job-mat.qty-uom job-mat.wid job-mat.len job.due-date 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table job-mat job
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table job-mat
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-Browser-Table job
&Scoped-define QUERY-STRING-Browser-Table FOR EACH job WHERE TRUE /* Join to itemfg incomplete */ ~
      AND job.job-no eq "zzzzzzzzzzzzzzzzzzzz" NO-LOCK, ~
      EACH job-mat OF job ~
      WHERE job-mat.all-flg eq yes NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH job WHERE TRUE /* Join to itemfg incomplete */ ~
      AND job.job-no eq "zzzzzzzzzzzzzzzzzzzz" NO-LOCK, ~
      EACH job-mat OF job ~
      WHERE job-mat.all-flg eq yes NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table job job-mat
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table job
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table job-mat


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi_rm-i-no fi_job-no fi_job-no2 btn_go ~
btn_show Browser-Table RECT-1 
&Scoped-Define DISPLAYED-OBJECTS fi_rm-i-no fi_job-no fi_job-no2 fi_sort-by ~
fi_name fi_q-onh fi_q-ton fi_q-lf fi_q-msf 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD cons-qty B-table-Win 
FUNCTION cons-qty RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD cons-uom B-table-Win 
FUNCTION cons-uom RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_go 
     LABEL "&Go" 
     SIZE 12 BY 1.

DEFINE BUTTON btn_show 
     LABEL "&Show All" 
     SIZE 12 BY 1.

DEFINE VARIABLE fi_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Job#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_job-no2 AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_name AS CHARACTER FORMAT "x(30)" 
     LABEL "Item Name" 
     VIEW-AS FILL-IN 
     SIZE 44.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_q-lf AS INTEGER FORMAT "->>>,>>>,>>9.9<<<<" INITIAL 0 
     LABEL "LF" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_q-msf AS INTEGER FORMAT "->>>,>>>,>>9.9<<<<" INITIAL 0 
     LABEL "MSF" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_q-onh AS INTEGER FORMAT "->>>,>>>,>>9.9<<<<" INITIAL 0 
     LABEL "OH:  EA" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_q-ton AS INTEGER FORMAT "->>>,>>>,>>9.9<<<<" INITIAL 0 
     LABEL "TON" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_rm-i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "RM Item#" 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_sort-by AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sorted By" 
     VIEW-AS FILL-IN 
     SIZE 86 BY 1
     BGCOLOR 14  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 148 BY 4.76.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      job
    FIELDS(job.due-date), 
      job-mat SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      job-mat.rm-i-no COLUMN-LABEL "RM Item#" FORMAT "x(10)":U
            WIDTH 15 LABEL-BGCOLOR 14
      job-mat.job-no COLUMN-LABEL "  Job#" FORMAT "x(6)":U WIDTH 9
            LABEL-BGCOLOR 14
      job-mat.job-no2 COLUMN-LABEL "" FORMAT "99":U WIDTH 4 LABEL-BGCOLOR 14
      job-mat.frm COLUMN-LABEL "S" FORMAT ">>>":U WIDTH 5 LABEL-BGCOLOR 14
      job-mat.blank-no COLUMN-LABEL "B" FORMAT ">>>":U WIDTH 5
            LABEL-BGCOLOR 14
      job-mat.qty COLUMN-LABEL "Required" FORMAT "->>>,>>9.9<<<<":U
            WIDTH 16 LABEL-BGCOLOR 14
      job-mat.qty-all COLUMN-LABEL "Allocated" FORMAT "->>>,>>9.9<<<<":U
            WIDTH 16 LABEL-BGCOLOR 14
      job-mat.qty-uom COLUMN-LABEL "Job UOM" FORMAT "x(4)":U WIDTH 10
            LABEL-BGCOLOR 14
      cons-qty () @ ld-cons-qty COLUMN-LABEL "Allocated" FORMAT "->>>,>>9.9<<<<":U
            WIDTH 16 LABEL-BGCOLOR 14
      cons-uom() @ ld-cons-uom COLUMN-LABEL "RM UOM" FORMAT "X(4)":U
            WIDTH 10 LABEL-BGCOLOR 14
      job-mat.wid FORMAT ">>9.99<<":U LABEL-BGCOLOR 14
      job-mat.len FORMAT ">>9.99<<":U LABEL-BGCOLOR 14
      job.due-date FORMAT "99/99/9999":U LABEL-BGCOLOR 14
  ENABLE
      job-mat.rm-i-no
      job-mat.job-no
      job-mat.job-no2
      job-mat.frm
      job-mat.blank-no
      job-mat.qty
      job-mat.qty-all
      job-mat.qty-uom
      job-mat.wid
      job-mat.len
      job.due-date
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 148 BY 15
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi_rm-i-no AT ROW 1.48 COL 16 COLON-ALIGNED
     fi_job-no AT ROW 1.48 COL 62 COLON-ALIGNED
     fi_job-no2 AT ROW 1.48 COL 81 COLON-ALIGNED
     btn_go AT ROW 2.91 COL 14
     btn_show AT ROW 2.91 COL 28
     fi_sort-by AT ROW 2.91 COL 59 COLON-ALIGNED
     fi_name AT ROW 4.33 COL 2.6 HELP
          "Enter Finished Goods Name used for Alpha Numeric Searches."
     fi_q-onh AT ROW 4.33 COL 67 COLON-ALIGNED
     fi_q-ton AT ROW 4.33 COL 89 COLON-ALIGNED
     fi_q-lf AT ROW 4.33 COL 109 COLON-ALIGNED
     fi_q-msf AT ROW 4.33 COL 131 COLON-ALIGNED
     Browser-Table AT ROW 5.76 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 
         DEFAULT-BUTTON btn_go.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser Template
   External Tables: asi.item
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
   Other Settings: PERSISTENT-ONLY
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
         WIDTH              = 148.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/navbrows.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
/* BROWSE-TAB Browser-Table fi_q-msf F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi_name IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fi_q-lf IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_q-msf IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_q-onh IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_q-ton IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_sort-by IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.job Where asi.itemfg ...,ASI.job-mat OF ASI.job"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,"
     _Where[1]         = "job.job-no eq ""zzzzzzzzzzzzzzzzzzzz"""
     _Where[2]         = "job-mat.all-flg eq yes"
     _FldNameList[1]   > ASI.job-mat.rm-i-no
"job-mat.rm-i-no" "RM Item#" ? "character" ? ? ? 14 ? ? yes ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.job-mat.job-no
"job-mat.job-no" "  Job#" ? "character" ? ? ? 14 ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.job-mat.job-no2
"job-mat.job-no2" "" "99" "integer" ? ? ? 14 ? ? yes ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.job-mat.frm
"job-mat.frm" "S" ">>>" "integer" ? ? ? 14 ? ? yes ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.job-mat.blank-no
"job-mat.blank-no" "B" ">>>" "integer" ? ? ? 14 ? ? yes ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.job-mat.qty
"job-mat.qty" "Required" "->>>,>>9.9<<<<" "decimal" ? ? ? 14 ? ? yes ? no no "16" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.job-mat.qty-all
"job-mat.qty-all" "Allocated" "->>>,>>9.9<<<<" "decimal" ? ? ? 14 ? ? yes ? no no "16" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.job-mat.qty-uom
"job-mat.qty-uom" "Job UOM" ? "character" ? ? ? 14 ? ? yes ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"cons-qty () @ ld-cons-qty" "Allocated" "->>>,>>9.9<<<<" ? ? ? ? 14 ? ? no ? no no "16" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"cons-uom() @ ld-cons-uom" "RM UOM" "X(4)" ? ? ? ? 14 ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > asi.job-mat.wid
"job-mat.wid" ? ? "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > asi.job-mat.len
"job-mat.len" ? ? "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > asi.job.due-date
"job.due-date" ? ? "date" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE Browser-Table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "SmartBrowserCues" B-table-Win _INLINE
/* Actions: adecomm/_so-cue.w ? adecomm/_so-cued.p ? adecomm/_so-cuew.p */
/* SmartBrowser,uib,49266
A SmartBrowser is a procedure object that retrieves and visualizes data using a browse.

CREATING A MASTER

Step 1
If necessary, specify an external table (steps 1a-1c). 

An external table completes join criteria for a query. For example, a query on 'Order OF Customer' requires the external table Customer. The external table is supplied by another procedure object--typically a SmartBrowser or SmartViewer.

Step 1a
In the UIB main window, Choose the Procedure button.

Step 1b
In the Procedure Settings dialog, choose Add.

Step 1c
From the Table Selector dialog, select the external table.

Step 2 
Double-click the browse to invoke the Query Builder.

Step 3
Using the Query Builder, specify the tables and fields for the browse.

Step 4 [Optional]
In the Code Section Editor, change the Foreign Keys and/or Sort Options for the browse query. Use the "List..." button to access these sections.

Step 5
Save and close the SmartBrowser master.

INSERTING AN INSTANCE

Step 1
Open or create a SmartContainer, such as a SmartWindow.

Step 2 
Choose the SmartBrowser master from the Object Palette.

Step 3
Draw the SmartBrowser instance into the SmartContainer.

Step 4
Add all necessary SmartLinks between the SmartBrowser and other SmartObjects. 

During assembly, the PROGRESS Advisor suggests links and creates them for you. However, you can also add and remove SmartLinks with the SmartLinks dialog box. To access this dialog box, choose the Procedure button from the UIB main window. Then choose the SmartLinks button from the Procedure Settings dialog box.
*/
/* _UIB-CODE-BLOCK-END */
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
  DEF VAR lh-column AS HANDLE NO-UNDO.
  DEF VAR lv-column-nam AS CHAR NO-UNDO.
  DEF VAR lv-column-lab AS CHAR NO-UNDO.


  ASSIGN
   lh-column     = {&BROWSE-NAME}:CURRENT-COLUMN 
   lv-column-nam = lh-column:NAME
   lv-column-lab = lh-column:LABEL.

  IF lv-column-lab EQ "Allocated" THEN lv-column-nam = "qty-all".

  IF lv-column-nam BEGINS "li-" THEN DO:
    APPLY 'END-SEARCH' TO {&BROWSE-NAME}.
    RETURN NO-APPLY.
  END.

  IF lv-column-nam BEGINS "job-no" THEN
    ASSIGN
     lv-column-nam = "job-no"
     lv-column-lab = "Job#".

  IF lv-sort-by EQ lv-column-nam THEN ll-sort-asc = NOT ll-sort-asc.

  ELSE
    ASSIGN
     lv-sort-by     = lv-column-nam
     lv-sort-by-lab = lv-column-lab.

  APPLY 'END-SEARCH' TO {&BROWSE-NAME}.

  APPLY "choose" TO btn_go.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

  IF AVAIL job-mat THEN RUN display-item (ROWID(job-mat)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Go */
DO:
  SESSION:SET-WAIT-STATE("general").

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     fi_rm-i-no
     fi_job-no
     fi_job-no2.

    RUN dispatch ("open-query").
  END.

  SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_show B-table-Win
ON CHOOSE OF btn_show IN FRAME F-Main /* Show All */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     fi_rm-i-no:SCREEN-VALUE   = ""
     fi_job-no:SCREEN-VALUE    = ""
     fi_job-no2:SCREEN-VALUE   = "".

    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_job-no B-table-Win
ON VALUE-CHANGED OF fi_job-no IN FRAME F-Main /* Job# */
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_job-no2 B-table-Win
ON LEAVE OF fi_job-no2 IN FRAME F-Main /* - */
DO:
  IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_rm-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_rm-i-no B-table-Win
ON VALUE-CHANGED OF fi_rm-i-no IN FRAME F-Main /* RM Item# */
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}

SESSION:DATA-ENTRY-RETURN = YES.

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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "item"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "item"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Disable-Navigation B-table-Win 
PROCEDURE Disable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-item B-table-Win 
PROCEDURE display-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

  DEF BUFFER b-item FOR item.

  DEF VAR v-onh           LIKE item.q-onh EXTENT 4       NO-UNDO.
  DEF VAR v-uom           LIKE item.cons-uom EXTENT 4    NO-UNDO
                          INIT ["EA","TON","LF","MSF"].


  FIND job-mat WHERE ROWID(job-mat) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL job-mat THEN
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST b-item
        WHERE b-item.company EQ cocode
          AND b-item.i-no    EQ job-mat.rm-i-no
        NO-LOCK NO-ERROR.
    IF AVAIL b-item THEN DO:

      DO i = 1 TO 4:
        IF b-item.cons-uom EQ v-uom[i] THEN
          v-onh[i] = b-item.q-onh.
        ELSE
          RUN sys/ref/convquom2.p(cocode, b-item.cons-uom, v-uom[i], b-item.basis-w,
                                 (if b-item.r-wid eq 0 THEN b-item.s-len
                                                       ELSE 12),
                                 (if b-item.r-wid eq 0 THEN b-item.s-wid
                                                       ELSE b-item.r-wid),
                                 b-item.s-dep,                    
                                 b-item.q-onh, OUTPUT v-onh[i]).
      END.

      ASSIGN
       fi_name:SCREEN-VALUE    = b-item.i-name
       fi_q-onh:SCREEN-VALUE   = STRING(v-onh[1])
       fi_q-ton:SCREEN-VALUE   = STRING(v-onh[2])
       fi_q-lf:SCREEN-VALUE    = STRING(v-onh[3])
       fi_q-msf:SCREEN-VALUE   = STRING(v-onh[4]).
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Enable-Navigation B-table-Win 
PROCEDURE Enable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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
  IF AVAIL job-mat THEN RUN display-item (ROWID(job-mat)).

  DO WITH FRAME {&FRAME-NAME}:
    fi_sort-by:SCREEN-VALUE = TRIM(lv-sort-by-lab)               + " " +
                              TRIM(STRING(ll-sort-asc,"As/Des")) + "cending".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   job-mat.rm-i-no:READ-ONLY IN BROWSE {&browse-name} = YES
   job-mat.job-no:READ-ONLY IN BROWSE {&browse-name} = YES
   job-mat.job-no2:READ-ONLY IN BROWSE {&browse-name} = YES
   job-mat.frm:READ-ONLY IN BROWSE {&browse-name} = YES
   job-mat.blank-no:READ-ONLY IN BROWSE {&browse-name} = YES
   job-mat.qty:READ-ONLY IN BROWSE {&browse-name} = YES
   job-mat.qty-all:READ-ONLY IN BROWSE {&browse-name} = YES
   job-mat.qty-uom:READ-ONLY IN BROWSE {&browse-name} = YES
   /*item.cons-uom:READ-ONLY IN BROWSE {&browse-name} = YES*/
   job-mat.wid:READ-ONLY IN BROWSE {&browse-name} = YES
   job-mat.len:READ-ONLY IN BROWSE {&browse-name} = YES
   job.due-date:READ-ONLY IN BROWSE {&browse-name} = YES.

  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
  {methods/winReSizeLocInit.i}

  RUN set-focus.

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
  /*RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .*/

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAIL item THEN DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     fi_rm-i-no:SCREEN-VALUE = item.i-no
     fi_rm-i-no              = item.i-no.
    DISABLE fi_rm-i-no.
  END.

  {rminq/j-rmainq.i}

  RUN dispatch ("display-fields").

  RUN dispatch ("row-changed").

  ll-first = NO.

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
  {src/adm/template/snd-list.i "item"}
  {src/adm/template/snd-list.i "job"}
  {src/adm/template/snd-list.i "job-mat"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-focus B-table-Win 
PROCEDURE set-focus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/setfocus.i {&BROWSE-NAME}}

  DO WITH FRAME {&FRAME-NAME}:
    APPLY "tab" TO {&BROWSE-NAME}.
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-xl B-table-Win 
PROCEDURE export-xl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR first-cust AS CHAR NO-UNDO.
DEF VAR last-cust AS CHAR NO-UNDO.

/*GET FIRST Browser-Table .
ASSIGN first-cust = cust.cust-no .
GET LAST Browser-Table .
ASSIGN last-cust = cust.cust-no . */

/*RUN fg/phon-exp.w (first-cust ,last-cust).*/

RUN rminq/rmnq-exp.w ("", "").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION cons-qty B-table-Win 
FUNCTION cons-qty RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

DEF BUFFER b-item FOR item.

DEF VAR v-comm      AS   DEC                                            NO-UNDO.
DEF VAR v-bwt       LIKE item.basis-w                                   NO-UNDO.
DEF VAR v-len       LIKE item.s-len                                     NO-UNDO.
DEF VAR v-wid       LIKE item.s-wid                                     NO-UNDO.


  FIND FIRST b-item NO-LOCK
      WHERE b-item.company EQ job-mat.company
        AND b-item.i-no    EQ job-mat.rm-i-no
      NO-ERROR.

  IF AVAIL b-item THEN DO WITH FRAME {&FRAME-NAME}:
    v-comm = job-mat.qty-all.

    IF job-mat.qty-uom NE b-item.cons-uom THEN DO:
      ASSIGN
       v-bwt = job-mat.basis-w
       v-len = job-mat.len
       v-wid = job-mat.wid.

      IF v-len EQ 0 THEN v-len = b-item.s-len.

      IF v-wid eq 0 THEN
        v-wid = IF b-item.r-wid NE 0 THEN b-item.r-wid ELSE b-item.s-wid.

      IF v-bwt EQ 0 THEN v-bwt = b-item.basis-w.

      RUN sys/ref/convquom2.p(cocode,job-mat.qty-uom, b-item.cons-uom,
                             v-bwt, v-len, v-wid, b-item.s-dep,
                             v-comm, OUTPUT v-comm).
    END.
  END.

  RETURN v-comm.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION cons-uom B-table-Win 
FUNCTION cons-uom RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

DEF BUFFER b-item FOR ITEM.

DEF VAR v-uom AS CHAR NO-UNDO.

FIND FIRST b-item NO-LOCK
      WHERE b-item.company EQ job-mat.company
        AND b-item.i-no    EQ job-mat.rm-i-no
      NO-ERROR.


IF AVAIL b-item THEN v-uom = b-item.cons-uom.

RETURN v-uom.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

