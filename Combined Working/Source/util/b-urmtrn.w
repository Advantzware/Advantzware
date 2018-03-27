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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR ll-first AS LOG INIT YES NO-UNDO.
DEF VAR lv-sort-by AS CHAR INIT "trans-date" NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "TR Date" NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.
DEF VAR ld-ext-cost AS DEC NO-UNDO.
DEF VAR fi_po-no AS INT NO-UNDO.

DEF BUFFER rm-rcpth-1 FOR rm-rcpth.
DEF BUFFER rm-rdtlh-1 FOR rm-rdtlh.

&SCOPED-DEFINE key-phrase rm-rcpth.company EQ cocode

&SCOPED-DEFINE for-each1                              ~
    FOR EACH rm-rcpth                                 ~
        WHERE {&key-phrase}                           ~
          AND rm-rcpth.trans-date GE fi_date          ~
          AND rm-rcpth.i-no       BEGINS fi_rm-i-no   ~
          AND rm-rcpth.rita-code  BEGINS fi_rita-code ~
          AND rm-rcpth.job-no     BEGINS fi_job-no    ~
          AND (rm-rcpth.job-no2   EQ fi_job-no2 OR fi_job-no2 EQ 0 OR fi_job-no EQ "")

&SCOPED-DEFINE for-each2                           ~
    EACH rm-rdtlh                                  ~
    WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no      ~
      AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code ~
      AND rm-rdtlh.tag MATCHES fi_tag#             ~
    NO-LOCK

&SCOPED-DEFINE sortby-log                                                                                  ~
    IF lv-sort-by EQ "i-no"       THEN rm-rcpth.i-no                                                  ELSE ~
    IF lv-sort-by EQ "rita-code"  THEN rm-rcpth.rita-code                                             ELSE ~
    IF lv-sort-by EQ "loc"        THEN rm-rdtlh.loc                                                   ELSE ~
    IF lv-sort-by EQ "loc-bin"    THEN rm-rdtlh.loc-bin                                               ELSE ~
    IF lv-sort-by EQ "tag"        THEN rm-rdtlh.tag                                                   ELSE ~
    IF lv-sort-by EQ "qty"        THEN STRING(rm-rdtlh.qty,"9999999999")                              ELSE ~
    IF lv-sort-by EQ "cost"       THEN STRING(rm-rdtlh.cost,"9999999999.99999")                       ELSE ~
    IF lv-sort-by EQ "job-no"     THEN STRING(rm-rcpth.job-no,"x(6)") + STRING(rm-rcpth.job-no2,"99") ELSE ~
                                       STRING(YEAR(rm-rcpth.trans-date),"9999") + STRING(MONTH(rm-rcpth.trans-date),"99") + STRING(DAY(rm-rcpth.trans-date),"99") + STRING(rm-rcpth.r-no,"9999999999")

&SCOPED-DEFINE sortby BY rm-rcpth.i-no BY rm-rcpth.job-no BY rm-rcpth.job-no2

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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rm-rcpth rm-rdtlh

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table rm-rcpth.i-no rm-rcpth.job-no ~
rm-rcpth.job-no2 rm-rcpth.trans-date rm-rcpth.rita-code rm-rdtlh.loc ~
rm-rdtlh.loc-bin rm-rdtlh.tag rm-rdtlh.qty rm-rdtlh.cost ~
calc-ext-cost (1) @ ld-ext-cost 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table rm-rcpth.i-no ~
rm-rcpth.job-no rm-rcpth.job-no2 rm-rcpth.trans-date rm-rcpth.rita-code ~
rm-rdtlh.loc rm-rdtlh.loc-bin rm-rdtlh.tag rm-rdtlh.qty rm-rdtlh.cost 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table rm-rcpth rm-rdtlh
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table rm-rcpth
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-Browser-Table rm-rdtlh
&Scoped-define QUERY-STRING-Browser-Table FOR EACH rm-rcpth WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      EACH rm-rdtlh OF rm-rcpth NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH rm-rcpth WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      EACH rm-rdtlh OF rm-rcpth NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table rm-rcpth rm-rdtlh
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table rm-rcpth
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table rm-rdtlh


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table btn_go btn_show fi_rm-i-no ~
fi_tag# fi_job-no fi_job-no2 fi_rita-code fi_date RECT-1 
&Scoped-Define DISPLAYED-OBJECTS fi_sort-by fi_name fi_q-onh fi_q-ton ~
fi_q-lf fi_q-msf fi_rm-i-no fi_tag# fi_job-no fi_job-no2 fi_rita-code ~
fi_date 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 fi_tag# 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calc-ext-cost B-table-Win 
FUNCTION calc-ext-cost RETURNS DECIMAL
  ( INPUT ip-type AS INT )  FORWARD.

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

DEFINE VARIABLE fi_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/00 
     LABEL "From Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Job#" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_job-no2 AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_name AS CHARACTER FORMAT "x(30)" 
     LABEL "Item Name" 
     VIEW-AS FILL-IN 
     SIZE 48 BY 1
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

DEFINE VARIABLE fi_rita-code AS CHARACTER FORMAT "X":U 
     LABEL "Trans Code" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_rm-i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "RM Item#" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_sort-by AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sorted By" 
     VIEW-AS FILL-IN 
     SIZE 86 BY 1 NO-UNDO.

DEFINE VARIABLE fi_tag# AS CHARACTER FORMAT "X(20)":U 
     LABEL "Tag#" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 148 BY 4.76.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      rm-rcpth
    FIELDS(rm-rcpth.i-no
      rm-rcpth.job-no
      rm-rcpth.job-no2
      rm-rcpth.trans-date
      rm-rcpth.rita-code), 
      rm-rdtlh SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      rm-rcpth.i-no COLUMN-LABEL "Item#" FORMAT "x(10)":U
      rm-rcpth.job-no FORMAT "x(6)":U
      rm-rcpth.job-no2 COLUMN-LABEL "" FORMAT "99":U
      rm-rcpth.trans-date COLUMN-LABEL "TR Date" FORMAT "99/99/99":U
      rm-rcpth.rita-code COLUMN-LABEL "C" FORMAT "x(1)":U
      rm-rdtlh.loc COLUMN-LABEL "Whs" FORMAT "x(5)":U
      rm-rdtlh.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U
      rm-rdtlh.tag COLUMN-LABEL "Tag" FORMAT "x(8)":U
      rm-rdtlh.qty COLUMN-LABEL "Qty" FORMAT "->>>>,>>9.9<<<<<":U
      rm-rdtlh.cost COLUMN-LABEL "Cost/EA" FORMAT "->>>,>>9.99<<<<":U
      calc-ext-cost (1) @ ld-ext-cost COLUMN-LABEL "Ext Cost" FORMAT "->>,>>>,>>9.99":U
  ENABLE
      rm-rcpth.i-no
      rm-rcpth.job-no
      rm-rcpth.job-no2
      rm-rcpth.trans-date
      rm-rcpth.rita-code
      rm-rdtlh.loc
      rm-rdtlh.loc-bin
      rm-rdtlh.tag
      rm-rdtlh.qty
      rm-rdtlh.cost
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 148 BY 15
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 5.76 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     btn_go AT ROW 2.91 COL 19
     btn_show AT ROW 2.91 COL 33
     fi_sort-by AT ROW 2.91 COL 59 COLON-ALIGNED
     fi_name AT ROW 4.33 COL 1.6 HELP
          "Enter Finished Goods Name used for Alpha Numeric Searches."
     fi_q-onh AT ROW 4.33 COL 70 COLON-ALIGNED
     fi_q-ton AT ROW 4.33 COL 91 COLON-ALIGNED
     fi_q-lf AT ROW 4.33 COL 110 COLON-ALIGNED
     fi_q-msf AT ROW 4.33 COL 131 COLON-ALIGNED
     fi_rm-i-no AT ROW 1.48 COL 10.6 COLON-ALIGNED
     fi_tag# AT ROW 1.48 COL 39.6 COLON-ALIGNED
     fi_job-no AT ROW 1.48 COL 85.2 COLON-ALIGNED
     fi_job-no2 AT ROW 1.48 COL 96.2 COLON-ALIGNED
     fi_rita-code AT ROW 1.48 COL 112.6 COLON-ALIGNED
     fi_date AT ROW 1.48 COL 129.6 COLON-ALIGNED
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
/* BROWSE-TAB Browser-Table 1 F-Main */
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
/* SETTINGS FOR FILL-IN fi_tag# IN FRAME F-Main
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.rm-rcpth,ASI.rm-rdtlh OF ASI.rm-rcpth"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,"
     _FldNameList[1]   > ASI.rm-rcpth.i-no
"rm-rcpth.i-no" "Item#" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.rm-rcpth.job-no
"rm-rcpth.job-no" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.rm-rcpth.job-no2
"rm-rcpth.job-no2" "" "99" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.rm-rcpth.trans-date
"rm-rcpth.trans-date" "TR Date" "99/99/99" "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.rm-rcpth.rita-code
"rm-rcpth.rita-code" "C" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.rm-rdtlh.loc
"rm-rdtlh.loc" "Whs" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.rm-rdtlh.loc-bin
"rm-rdtlh.loc-bin" "Bin" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.rm-rdtlh.tag
"rm-rdtlh.tag" "Tag" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.rm-rdtlh.qty
"rm-rdtlh.qty" "Qty" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.rm-rdtlh.cost
"rm-rdtlh.cost" "Cost/EA" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"calc-ext-cost (1) @ ld-ext-cost" "Ext Cost" "->>,>>>,>>9.99" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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

  IF AVAIL rm-rcpth THEN RUN display-item (ROWID(rm-rcpth)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rdtlh.cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rdtlh.cost Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rdtlh.cost IN BROWSE Browser-Table /* Cost/EA */
DO:
  ld-ext-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(calc-ext-cost (2)).
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
     fi_tag#
     fi_job-no
     fi_job-no2
     fi_rita-code
     fi_date.

    {rminq/j-rmiinq.i}
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
     fi_job-no2:SCREEN-VALUE   = ""
     fi_rita-code:SCREEN-VALUE = ""
     fi_date:SCREEN-VALUE      = "01/01/0001"
     fi_tag#:SCREEN-VALUE      = "".

    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_job-no B-table-Win
ON LEAVE OF fi_job-no IN FRAME F-Main /* Job# */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN fi_job-no:SCREEN-VALUE = FILL(" ",6 - LENGTH(TRIM(fi_job-no:SCREEN-VALUE)))
                                  + TRIM(fi_job-no:SCREEN-VALUE).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_job-no B-table-Win
ON VALUE-CHANGED OF fi_job-no IN FRAME F-Main /* Job# */
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1 + IF LASTKEY EQ 32 THEN 1 ELSE 0. /* added by script _caps.p */
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


&Scoped-define SELF-NAME fi_rita-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_rita-code B-table-Win
ON VALUE-CHANGED OF fi_rita-code IN FRAME F-Main /* Trans Code */
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1 + IF LASTKEY EQ 32 THEN 1 ELSE 0. /* added by script _caps.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_rm-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_rm-i-no B-table-Win
ON LEAVE OF fi_rm-i-no IN FRAME F-Main /* RM Item# */
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_tag#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_tag# B-table-Win
ON HELP OF fi_tag# IN FRAME F-Main /* Tag# */
DO:
  DEF VAR lv-char-val AS CHAR NO-UNDO.

  RUN windows/l-fgtag.w (cocode,fi_rm-i-no:SCREEN-VALUE,'',OUTPUT lv-char-val).
  IF ENTRY(1,lv-char-val) NE SELF:SCREEN-VALUE THEN DO: 
    SELF:SCREEN-VALUE = ENTRY(1,lv-char-val).
    APPLY 'VALUE-CHANGED' TO {&self-name}.
  END.
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

  DEF VAR v-onh           LIKE item.q-onh EXTENT 4       NO-UNDO.
  DEF VAR v-uom           LIKE item.cons-uom EXTENT 4    NO-UNDO
                          INIT ["EA","TON","LF","MSF"].


  FIND rm-rcpth WHERE ROWID(rm-rcpth) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL rm-rcpth THEN
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST item
        WHERE item.company EQ cocode
          AND item.i-no    EQ rm-rcpth.i-no
        NO-LOCK NO-ERROR.
    IF AVAIL item THEN DO:
      DO i = 1 TO 4:
        IF ITEM.cons-uom EQ v-uom[i] THEN
          v-onh[i] = item.q-onh.
        ELSE
          RUN sys/ref/convquom.p(item.cons-uom, v-uom[i], item.basis-w,
                                 (if item.r-wid eq 0 THEN item.s-len
                                                     ELSE 12),
                                 (if item.r-wid eq 0 THEN item.s-wid
                                                     ELSE item.r-wid),
                                 item.s-dep,                    
                                 item.q-onh, OUTPUT v-onh[i]).
      END.

      ASSIGN
       fi_name:SCREEN-VALUE    = item.i-name
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
  IF AVAIL rm-rcpth THEN RUN display-item (ROWID(rm-rcpth)).

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
   rm-rcpth.i-no:READ-ONLY IN BROWSE {&browse-name} = YES
   rm-rcpth.job-no:READ-ONLY IN BROWSE {&browse-name} = YES
   rm-rcpth.job-no2:READ-ONLY IN BROWSE {&browse-name} = YES
   rm-rcpth.trans-date:READ-ONLY IN BROWSE {&browse-name} = YES
   rm-rcpth.rita-code:READ-ONLY IN BROWSE {&browse-name} = YES
   rm-rdtlh.loc:READ-ONLY IN BROWSE {&browse-name} = YES
   rm-rdtlh.loc-bin:READ-ONLY IN BROWSE {&browse-name} = YES
   rm-rdtlh.tag:READ-ONLY IN BROWSE {&browse-name} = YES
   rm-rdtlh.qty:READ-ONLY IN BROWSE {&browse-name} = YES
   .

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
  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR lv-i-no LIKE item.i-no NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  /*RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .*/

  /* Code placed here will execute AFTER standard behavior.    */
  IF ll-first THEN DO:
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"history-source", OUTPUT char-hdl).
    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO WITH FRAME {&FRAME-NAME}:
      RUN get-i-no IN WIDGET-HANDLE(char-hdl) (OUTPUT fi_rm-i-no).
      DISPLAY fi_rm-i-no.
      DISABLE fi_rm-i-no.
      ll-first = NO.
    END.

    ELSE 
      OPEN QUERY Browser-Table FOR EACH rm-rcpth WHERE rm-rcpth.company EQ cocode AND rm-rcpth.i-no EQ "zzzzzzzzzzzzzzzzzzzzzzzzz" NO-LOCK, ~
                                   EACH rm-rdtlh WHERE TRUE /* Join to rm-rcpth incomplete */ ~
                                                   AND rm-rdtlh.r-no eq rm-rcpth.r-no and ~
                                                       rm-rdtlh.rita-code eq rm-rcpth.rita-code NO-LOCK.
  END.

  IF NOT ll-first THEN DO:
    {rminq/j-rmiinq.i}
  END.

  RUN dispatch ("display-fields").

  RUN dispatch ("row-changed").

  ll-first = YES.

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
  {src/adm/template/snd-list.i "rm-rcpth"}
  {src/adm/template/snd-list.i "rm-rdtlh"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-defaults B-table-Win 
PROCEDURE set-defaults :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     fi_rm-i-no   = ""
     fi_job-no    = ""
     fi_job-no2   = 0
     fi_rita-code = ""
     fi_date      = 01/01/0001
     fi_tag#      = ""
     ll-first     = yes. /* DD 05/01/2007 TASK 0501-0709 */

    DISPLAY
     fi_rm-i-no
     fi_job-no
     fi_job-no2
     fi_rita-code
     fi_date
     fi_tag#.
  END.

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
    APPLY "entry" TO fi_rm-i-no.
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calc-ext-cost B-table-Win 
FUNCTION calc-ext-cost RETURNS DECIMAL
  ( INPUT ip-type AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/


  RETURN IF ip-type EQ 1 THEN (rm-rdtlh.qty * rm-rdtlh.cost)
         ELSE (DEC(rm-rdtlh.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) *
               DEC(rm-rdtlh.cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

