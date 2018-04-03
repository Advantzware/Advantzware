&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  apinq\b-apinq2.w

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
&SCOPED-DEFINE setUserPrint
&SCOPED-DEFINE winReSize
&SCOPED-DEFINE sizeOption HEIGHT
&SCOPED-DEFINE browseOnly
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}
{sys/inc/varasgn.i}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR lv-frst-rowid AS ROWID NO-UNDO.
DEF VAR lv-last-rowid AS ROWID NO-UNDO.
DEF VAR char-hdl AS CHAR NO-UNDO.
DEF VAR phandle AS HANDLE NO-UNDO.
DEF VAR lv-sort-by AS CHAR INIT "inv-no" NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "Invoice#" NO-UNDO.
DEF VAR ll-sort-asc AS LOG INIT NO NO-UNDO.
DEF VAR v-col-move AS LOG INIT TRUE NO-UNDO.

&SCOPED-DEFINE key-phrase ap-inv.company EQ cocode

&SCOPED-DEFINE for-each1                                   ~
    FOR EACH ap-inv                                        ~
        WHERE {&key-phrase}                                ~
          AND ap-inv.recur   EQ NO                         ~
          AND ap-inv.vend-no BEGINS fi_vend                ~
          AND ap-inv.inv-no  BEGINS fi_finv                ~
          AND (ap-inv.inv-date GE fi_date OR fi_date EQ ?) ~
          AND (ap-inv.inv-date LE fi_date-to OR fi_date-to EQ ?) ~
          AND ((ap-inv.posted EQ YES AND tb_posted) OR     ~
               (ap-inv.posted EQ NO  AND tb_unposted))

&SCOPED-DEFINE sortby-log                                                                                                                                ~
    IF lv-sort-by EQ "vend-no"  THEN ap-inv.vend-no                                                                                                 ELSE ~
    IF lv-sort-by EQ "inv-no"   THEN ap-inv.inv-no                                                                                                  ELSE ~
    IF lv-sort-by EQ "stat"     THEN ap-inv.stat                                                                                                    ELSE ~
    IF lv-sort-by EQ "user-id"  THEN ap-inv.user-id                                                                                               ELSE ~
    IF lv-sort-by EQ "net"      THEN STRING(ap-inv.net,"-9999999999.99999")                                                                         ELSE ~
    IF lv-sort-by EQ "paid"     THEN STRING(ap-inv.paid,"-9999999999.99999")                                                                        ELSE ~
    IF lv-sort-by EQ "due"      THEN STRING(ap-inv.due,"-9999999999.99999")                                                                         ELSE ~
    IF lv-sort-by EQ "inv-date" THEN STRING(YEAR(ap-inv.inv-date),"9999") + STRING(MONTH(ap-inv.inv-date),"99") + STRING(DAY(ap-inv.inv-date),"99") ELSE ~
                                     STRING(YEAR(ap-inv.due-date),"9999") + STRING(MONTH(ap-inv.due-date),"99") + STRING(DAY(ap-inv.due-date),"99")
    

&SCOPED-DEFINE sortby BY ap-inv.vend-no BY ap-inv.inv-no

&SCOPED-DEFINE sortby-phrase-asc  ~
    BY ({&sortby-log})            ~
    {&sortby}

&SCOPED-DEFINE sortby-phrase-desc ~
    BY ({&sortby-log}) DESC       ~
    {&sortby}

DEF VAR ll-first AS LOG INIT YES NO-UNDO.
DEF BUFFER b-vend FOR vend.

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
&Scoped-define INTERNAL-TABLES ap-inv

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table ap-inv.inv-no ap-inv.vend-no ~
ap-inv.inv-date ap-inv.due-date ap-inv.net ap-inv.paid ap-inv.due ~
ap-inv.stat ap-inv.user-id 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table ap-inv.inv-no ~
ap-inv.vend-no ap-inv.inv-date ap-inv.due-date ap-inv.net ap-inv.paid ~
ap-inv.due ap-inv.stat ap-inv.user-id 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table ap-inv
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table ap-inv
&Scoped-define QUERY-STRING-Browser-Table FOR EACH ap-inv WHERE ~{&KEY-PHRASE} ~
      AND ap-inv.i-no eq 999999999 NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH ap-inv WHERE ~{&KEY-PHRASE} ~
      AND ap-inv.i-no eq 999999999 NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table ap-inv
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table ap-inv


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 fi_date tb_posted fi_finv fi_vend ~
fi_date-to tb_unposted btn_go btn_show Browser-Table 
&Scoped-Define DISPLAYED-OBJECTS fi_date tb_posted fi_finv fi_vend ~
fi_date-to tb_unposted fi_sort-by FI_moveCol 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_go 
     LABEL "&Go" 
     SIZE 12 BY 1.

DEFINE BUTTON btn_show 
     LABEL "&Show All" 
     SIZE 12 BY 1.

DEFINE VARIABLE fi_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "From Inv Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_date-to AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "To Inv Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_finv AS CHARACTER FORMAT "x(12)" 
     LABEL "Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI_moveCol AS CHARACTER FORMAT "X(4)":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE fi_sort-by AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sorted By" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE fi_vend AS CHARACTER FORMAT "X(8)":U 
     LABEL "Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 4.29.

DEFINE VARIABLE tb_posted AS LOGICAL INITIAL no 
     LABEL "Posted" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE tb_unposted AS LOGICAL INITIAL yes 
     LABEL "Unposted" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      ap-inv SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      ap-inv.inv-no COLUMN-LABEL "Invoice#" FORMAT "x(12)":U WIDTH 18.2
            LABEL-BGCOLOR 14
      ap-inv.vend-no COLUMN-LABEL "Vendor#" FORMAT "x(8)":U WIDTH 17.2
            LABEL-BGCOLOR 14
      ap-inv.inv-date FORMAT "99/99/9999":U WIDTH 17.2 LABEL-BGCOLOR 14
      ap-inv.due-date FORMAT "99/99/9999":U WIDTH 16.2 LABEL-BGCOLOR 14
      ap-inv.net FORMAT "->,>>>,>>9.99":U LABEL-BGCOLOR 14
      ap-inv.paid FORMAT "->,>>>,>>9.99":U WIDTH 18.2 LABEL-BGCOLOR 14
      ap-inv.due COLUMN-LABEL "Balance Due" FORMAT "->,>>>,>>9.99":U
            WIDTH 19.2 LABEL-BGCOLOR 14
      ap-inv.stat FORMAT "x":U LABEL-BGCOLOR 14                           /*Task# 01311403*/
      ap-inv.user-id COLUMN-LABEL "User" FORMAT "x(8)":U LABEL-BGCOLOR 14  /*Task# 01311403*/
  ENABLE
      ap-inv.inv-no
      ap-inv.vend-no
      ap-inv.inv-date
      ap-inv.due-date
      ap-inv.net
      ap-inv.paid
      ap-inv.due
      ap-inv.stat
      ap-inv.user-id
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 15.24
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi_date AT ROW 1.19 COL 120.2 COLON-ALIGNED
     tb_posted AT ROW 1.24 COL 4
     fi_finv AT ROW 1.95 COL 39 COLON-ALIGNED
     fi_vend AT ROW 1.95 COL 82 COLON-ALIGNED
     fi_date-to AT ROW 2.29 COL 120.2 COLON-ALIGNED WIDGET-ID 8
     tb_unposted AT ROW 2.43 COL 4
     fi_sort-by AT ROW 3.76 COL 61 COLON-ALIGNED
     FI_moveCol AT ROW 3.76 COL 132.6 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     btn_go AT ROW 3.86 COL 12
     btn_show AT ROW 3.86 COL 31
     Browser-Table AT ROW 5.29 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     "Browser Col. Mode:" VIEW-AS TEXT
          SIZE 22.6 BY .62 AT ROW 4 COL 111.8 WIDGET-ID 6
          FONT 6
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 6
         DEFAULT-BUTTON btn_go.


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

{src/adm/method/navbrows.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB Browser-Table btn_show F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "2".

/* SETTINGS FOR FILL-IN FI_moveCol IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_sort-by IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.ap-inv"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", OUTER"
     _Where[1]         = "ap-inv.i-no eq 999999999"
     _FldNameList[1]   > ASI.ap-inv.inv-no
"ap-inv.inv-no" "Invoice#" ? "character" ? ? ? 14 ? ? yes ? no no "18.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.ap-inv.vend-no
"ap-inv.vend-no" "Vendor#" ? "character" ? ? ? 14 ? ? yes ? no no "17.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.ap-inv.inv-date
"ap-inv.inv-date" ? ? "date" ? ? ? 14 ? ? yes ? no no "17.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.ap-inv.due-date
"ap-inv.due-date" ? ? "date" ? ? ? 14 ? ? yes ? no no "16.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.ap-inv.net
"ap-inv.net" ? ? "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.ap-inv.paid
"ap-inv.paid" ? ? "decimal" ? ? ? 14 ? ? yes ? no no "18.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.ap-inv.due
"ap-inv.due" "Balance Due" ? "decimal" ? ? ? 14 ? ? yes ? no no "19.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.ap-inv.stat
"ap-inv.stat" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.ap-inv.user-id
"ap-inv.user-id" "User" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
   {methods/template/selectpg.i}
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
  DEF VAR lh-column AS HANDLE NO-UNDO.
  DEF VAR lv-column-nam AS CHAR NO-UNDO.
  DEF VAR lv-column-lab AS CHAR NO-UNDO.
  
  ASSIGN
   lh-column     = {&BROWSE-NAME}:CURRENT-COLUMN 
   lv-column-nam = lh-column:NAME
   lv-column-lab = lh-column:LABEL.

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

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-source",OUTPUT char-hdl).
  RUN get-ATTRIBUTE IN WIDGET-HANDLE(char-hdl) ("NEW-AP").
  IF RETURN-VALUE <> "YES" THEN 
     APPLY 'entry' TO BROWSE {&browse-name}.

  IF AVAILABLE(ap-inv) THEN DO:
      RUN custom/setUserPrint.p (g_company,'ap-inv_.',
                   'begin_vend,end_vend,begin_date,end_date',
                             ap-inv.vend-no + ',' + ap-inv.vend-no + ',' +
                             STRING(ap-inv.inv-date) + ',' + STRING(ap-inv.inv-date)).

      {methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header"
                       "(ap-inv.rec_key,{methods/headers/ap-inv.i})"}

      FIND FIRST b-vend WHERE
           b-vend.company EQ ap-inv.company AND
           b-vend.vend-no EQ ap-inv.vend-no
           NO-LOCK NO-ERROR.

      IF AVAIL b-vend THEN
         RUN pushpin-image-proc(INPUT b-vend.rec_key).
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Go */
DO:
  RUN assign-all.

  RUN dispatch ("open-query").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_show B-table-Win
ON CHOOSE OF btn_show IN FRAME F-Main /* Show All */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    RUN set-defaults.

    tb_unposted:SCREEN-VALUE = "yes".

    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_date B-table-Win
ON LEAVE OF fi_date IN FRAME F-Main /* From Inv Date */
DO:
  fi_date-to:SCREEN-VALUE = fi_date:SCREEN-VALUE. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_vend B-table-Win
ON LEAVE OF fi_vend IN FRAME F-Main /* Vendor# */
DO:
  fi_vend:SCREEN-VALUE = CAPS(fi_vend:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}

&SCOPED-DEFINE cellColumnDat apinqb-apinq2

{methods/browsers/setCellColumns.i}

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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assign-all B-table-Win 
PROCEDURE assign-all :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     tb_posted
     tb_unposted
     fi_finv
     fi_vend
     fi_date
     fi_date-to   .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE browse-rowid B-table-Win 
PROCEDURE browse-rowid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-rowid AS ROWID NO-UNDO.

  IF AVAIL {&FIRST-TABLE-IN-QUERY-{&browse-name}} THEN
    op-rowid = ROWID({&FIRST-TABLE-IN-QUERY-{&browse-name}}).

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

  {methods/run_link.i "NAVIGATION-SOURCE" "dispatch" "('disable':U) NO-ERROR"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-note B-table-Win 
PROCEDURE disable-note :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF OUTPUT PARAMETER op-enable-note AS LOG  NO-UNDO.
 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Enable-Navigation B-table-Win 
PROCEDURE Enable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/run_link.i "NAVIGATION-SOURCE" "dispatch" "('enable':U) NO-ERROR"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE first-query B-table-Win 
PROCEDURE first-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-ap-inv FOR ap-inv.

  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-i-no LIKE ap-inv.i-no NO-UNDO.

  RUN set-defaults.
  
  li = 0.

  {&for-each1}
        AND ap-inv.posted EQ NO
      USE-INDEX posted NO-LOCK
      BREAK BY ap-inv.i-no DESC:
    IF FIRST-OF(ap-inv.i-no) THEN li = li + 1.
    lv-i-no = ap-inv.i-no.
    IF li GE 30 THEN LEAVE.
  END.

  &SCOPED-DEFINE open-query              ~
      OPEN QUERY {&browse-name}          ~
        {&for-each1}                     ~
              AND ap-inv.i-no GE lv-i-no ~
              AND ap-inv.posted EQ NO    ~
            USE-INDEX i-no NO-LOCK

  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.

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
  DO WITH FRAME {&FRAME-NAME}:
    fi_sort-by:SCREEN-VALUE = TRIM(lv-sort-by-lab)               + " " +
                              TRIM(STRING(ll-sort-asc,"As/Des")) + "cending".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-get-first B-table-Win 
PROCEDURE local-get-first :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'get-first':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/template/local/setvalue.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-get-last B-table-Win 
PROCEDURE local-get-last :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'get-last':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/template/local/setvalue.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-get-next B-table-Win 
PROCEDURE local-get-next :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'get-next':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/template/local/setvalue.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-get-prev B-table-Win 
PROCEDURE local-get-prev :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'get-prev':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/template/local/setvalue.i}

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

  RUN setCellColumns.

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN 
   ap-inv.vend-no:READ-ONLY IN BROWSE {&browse-name} = YES
   ap-inv.inv-no:READ-ONLY IN BROWSE {&browse-name} = YES
   ap-inv.inv-date:READ-ONLY IN BROWSE {&browse-name} = YES
   ap-inv.due-date:READ-ONLY IN BROWSE {&browse-name} = YES
   ap-inv.net:READ-ONLY IN BROWSE {&browse-name} = YES
   ap-inv.paid:READ-ONLY IN BROWSE {&browse-name} = YES
   ap-inv.net:READ-ONLY IN BROWSE {&browse-name} = YES
   ap-inv.due:READ-ONLY IN BROWSE {&browse-name} = YES
   ap-inv.stat:READ-ONLY IN BROWSE {&browse-name} = YES
   ap-inv.user-id:READ-ONLY IN BROWSE {&browse-name} = YES
  FI_moveCol = "Sort".
  DISPLAY FI_moveCol WITH FRAME {&FRAME-NAME}.
  
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
  RUN ap/delinvap.p (cocode).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF ll-first THEN RUN first-query.
  ELSE DO:
    {apinq/j-apinq2.i} 
  END.
  
  ll-first = NO.

  RUN dispatch ("display-fields").

  RUN dispatch ("row-changed").

  RUN dispatch ('get-last':U).
  IF AVAIL {&FIRST-TABLE-IN-QUERY-{&browse-name}} THEN
    lv-last-rowid  = ROWID({&FIRST-TABLE-IN-QUERY-{&browse-name}}).
    
  RUN dispatch ('get-first':U).
  IF AVAIL {&FIRST-TABLE-IN-QUERY-{&browse-name}} THEN
    lv-frst-rowid  = ROWID({&FIRST-TABLE-IN-QUERY-{&browse-name}}).

  IF AVAIL ap-inv THEN
  DO:
     FIND FIRST b-vend WHERE
          b-vend.company EQ ap-inv.company AND
          b-vend.vend-no EQ ap-inv.vend-no
          NO-LOCK NO-ERROR.
    
     IF AVAIL b-vend THEN
        RUN pushpin-image-proc(INPUT b-vend.rec_key).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view B-table-Win 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/template/local/setvalue.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE move-columns B-table-Win 
PROCEDURE move-columns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN
     Browser-Table:COLUMN-MOVABLE = v-col-move
     Browser-Table:COLUMN-RESIZABLE = v-col-move
     v-col-move = NOT v-col-move
     FI_moveCol = IF v-col-move = NO THEN "Move" ELSE "Sort".
  DISPLAY FI_moveCol.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE navigate-browser B-table-Win 
PROCEDURE navigate-browser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT  PARAMETER ip-nav-type AS CHAR.
  DEF OUTPUT PARAMETER op-nav-type AS CHAR.


  IF ip-nav-type NE "" THEN
  CASE ip-nav-type:
    WHEN "F" THEN RUN dispatch ('get-first':U).
    WHEN "L" THEN RUN dispatch ('get-last':U).
    WHEN "N" THEN RUN dispatch ('get-next':U).
    WHEN "P" THEN RUN dispatch ('get-prev':U).
  END CASE.
    
  IF ROWID({&FIRST-TABLE-IN-QUERY-{&browse-name}}) EQ lv-last-rowid THEN
    op-nav-type = "L".
      
  IF ROWID({&FIRST-TABLE-IN-QUERY-{&browse-name}}) EQ lv-frst-rowid THEN
    op-nav-type = IF op-nav-type EQ "L" THEN "B" ELSE "F".
  RUN dispatch ("row-changed").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE navigate-browser2 B-table-Win 
PROCEDURE navigate-browser2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT  PARAMETER ip-nav-type AS CHAR.
  DEF OUTPUT PARAMETER op-nav-type AS CHAR.
  
  DEF VAR hld-rowid AS ROWID NO-UNDO.

  RUN navigate-browser (ip-nav-type, OUTPUT op-nav-type).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pushpin-image-proc B-table-Win 
PROCEDURE pushpin-image-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-rec_key AS CHAR NO-UNDO.
   
   DEF VAR v-att AS LOG NO-UNDO.
   DEF VAR v-inv-no AS CHAR NO-UNDO.

   ASSIGN
   v-inv-no = STRING(ap-inv.inv-no,"X(12)") + "APINV"
   v-att = CAN-FIND(FIRST attach WHERE
           attach.company = cocode and
           attach.rec_key = ip-rec_key AND
           (attach.est-no eq v-inv-no OR ATTACH.est-no EQ "")).

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'attachinv-target':U, OUTPUT char-hdl).

   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN pushpin-image IN WIDGET-HANDLE(char-hdl) (INPUT v-att).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE record-added B-table-Win 
PROCEDURE record-added :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ll-first = YES.

    RUN set-defaults.
    
    RUN assign-all.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-query1 B-table-Win 
PROCEDURE reopen-query1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    RUN dispatch ("open-query").
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
    RUN dispatch ("row-changed").
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
  {src/adm/template/snd-list.i "ap-inv"}

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
     tb_posted   = NO
     tb_unposted = YES
     fi_finv     = ""
     fi_vend     = ""
     fi_date     = TODAY - 180
     fi_date-to  = TODAY
     .

    DISPLAY tb_posted tb_unposted fi_finv fi_vend fi_date fi_date-to.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE value-changed-proc B-table-Win 
PROCEDURE value-changed-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
      APPLY "VALUE-CHANGED" TO BROWSE {&browse-name}.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

