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

&SCOPED-DEFINE winReSize
&SCOPED-DEFINE sizeOption HEIGHT
&SCOPED-DEFINE browseOnly
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR lv-frst-rowid AS ROWID NO-UNDO.
DEF VAR lv-last-rowid AS ROWID NO-UNDO.
DEF VAR lv-frst-rowid2 AS ROWID NO-UNDO.
DEF VAR lv-last-rowid2 AS ROWID NO-UNDO.
DEF VAR char-hdl AS CHAR NO-UNDO.
DEF VAR phandle AS HANDLE NO-UNDO.
DEF VAR lv-sort-by AS CHAR INIT "check-no" NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "Memo#" NO-UNDO.
DEF VAR ll-sort-asc AS LOG INIT NO NO-UNDO.
DEF VAR lv-save-char AS CHAR INIT "" NO-UNDO.

&SCOPED-DEFINE key-phrase ap-pay.company EQ cocode AND ap-pay.posted EQ YES AND ASI.ap-pay.memo EQ YES

&SCOPED-DEFINE for-each1                     ~
    FOR EACH ap-pay                          ~
        WHERE {&key-phrase}                  ~
          AND ap-pay.vend-no BEGINS fi_vend

&SCOPED-DEFINE for-each2                     ~
    FIRST vend OF ap-pay NO-LOCK

&SCOPED-DEFINE sortby-log                                                                                                                                      ~
    IF lv-sort-by EQ "vend-no"  THEN ap-pay.vend-no                                                                                                       ELSE ~
    IF lv-sort-by EQ "check-no" THEN STRING(ap-pay.check-no,"9999999999")                                                                                 ELSE ~
    IF lv-sort-by EQ "name"     THEN vend.name                                                                                                            ELSE ~
                                     STRING(YEAR(ap-pay.check-date),"9999") + STRING(MONTH(ap-pay.check-date),"99") + STRING(DAY(ap-pay.check-date),"99")

&SCOPED-DEFINE sortby BY ap-pay.check-no

&SCOPED-DEFINE sortby-phrase-asc  ~
    BY ({&sortby-log})            ~
    {&sortby}

&SCOPED-DEFINE sortby-phrase-desc ~
    BY ({&sortby-log}) DESC       ~
    {&sortby}

DEF VAR ll-first AS LOG INIT YES NO-UNDO.

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
&Scoped-define INTERNAL-TABLES ap-pay vend

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table ap-pay.vend-no vend.name ~
ap-pay.check-no ap-pay.check-date display-amount() @ ap-pay.check-amt 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table ap-pay.vend-no ~
vend.name ap-pay.check-no ap-pay.check-date 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table ap-pay vend
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table ap-pay
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-Browser-Table vend
&Scoped-define QUERY-STRING-Browser-Table FOR EACH ap-pay WHERE ~{&KEY-PHRASE} ~
      AND ap-pay.check-no gt 999999999 NO-LOCK, ~
      EACH vend OF ap-pay NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH ap-pay WHERE ~{&KEY-PHRASE} ~
      AND ap-pay.check-no gt 999999999 NO-LOCK, ~
      EACH vend OF ap-pay NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table ap-pay vend
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table ap-pay
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table vend


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 fi_fchk fi_vend btn_go btn_show ~
Browser-Table 
&Scoped-Define DISPLAYED-OBJECTS fi_fchk fi_vend fi_name fi_sort-by 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-amount B-table-Win 
FUNCTION display-amount RETURNS DECIMAL
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

DEFINE VARIABLE fi_fchk AS INTEGER FORMAT ">>>>>>>>>" INITIAL 0 
     LABEL "Memo#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_name AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_sort-by AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sorted By" 
     VIEW-AS FILL-IN 
     SIZE 72 BY 1
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE fi_vend AS CHARACTER FORMAT "X(8)":U 
     LABEL "Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 3.33.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      ap-pay, 
      vend SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      ap-pay.vend-no COLUMN-LABEL "Vendor#" FORMAT "x(8)":U WIDTH 17.2
            LABEL-BGCOLOR 14
      vend.name FORMAT "x(30)":U LABEL-BGCOLOR 14
      ap-pay.check-no COLUMN-LABEL "Memo#" FORMAT ">>>>>>>>>>":U
            WIDTH 19.2 LABEL-BGCOLOR 14
      ap-pay.check-date COLUMN-LABEL "Memo Date" FORMAT "99/99/9999":U
            WIDTH 18.2 LABEL-BGCOLOR 14
      display-amount() @ ap-pay.check-amt COLUMN-LABEL "Amount" FORMAT "->>>,>>>,>>9.99":U
  ENABLE
      ap-pay.vend-no
      vend.name
      ap-pay.check-no
      ap-pay.check-date
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 16.19
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi_fchk AT ROW 1.48 COL 19 COLON-ALIGNED
     fi_vend AT ROW 1.48 COL 60 COLON-ALIGNED
     fi_name AT ROW 1.48 COL 79 COLON-ALIGNED HELP
          "Enter Finished Goods Name used for Alpha Numeric Searches." NO-LABEL
     btn_go AT ROW 2.91 COL 12
     btn_show AT ROW 2.91 COL 31
     fi_sort-by AT ROW 2.91 COL 61 COLON-ALIGNED
     Browser-Table AT ROW 4.33 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
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
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB Browser-Table fi_sort-by F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "2".

/* SETTINGS FOR FILL-IN fi_name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_sort-by IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.ap-pay,ASI.vend OF ASI.ap-pay"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ","
     _Where[1]         = "ASI.ap-pay.check-no gt 999999999"
     _FldNameList[1]   > ASI.ap-pay.vend-no
"ap-pay.vend-no" "Vendor#" ? "character" ? ? ? 14 ? ? yes ? no no "17.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.vend.name
"vend.name" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.ap-pay.check-no
"ap-pay.check-no" "Memo#" ">>>>>>>>>>" "integer" ? ? ? 14 ? ? yes ? no no "19.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.ap-pay.check-date
"ap-pay.check-date" "Memo Date" ? "date" ? ? ? 14 ? ? yes ? no no "18.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"display-amount() @ ap-pay.check-amt" "Amount" "->>>,>>>,>>9.99" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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


&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win            /* Task# 10251303 */
ON HELP OF FRAME F-Main
DO:
  DEF VAR char-val AS cha NO-UNDO.
  DEF VAR rec-val AS RECID NO-UNDO.


  CASE FOCUS:NAME:
    WHEN "fi_vend" THEN DO:
      RUN windows/l-vendno.w (g_company, "A", FOCUS:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE ""                                      AND
         TRIM(FOCUS:SCREEN-VALUE) NE TRIM(ENTRY(1,char-val)) THEN DO:
        FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
        RUN new-vend.
      END.
    END.  
  END CASE.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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

  IF lv-column-lab EQ "Amount" THEN RETURN NO-APPLY.

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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Go */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     fi_fchk
     fi_vend
     fi_name.
  END.

  RUN dispatch ("open-query").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_show B-table-Win
ON CHOOSE OF btn_show IN FRAME F-Main /* Show All */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     fi_fchk:SCREEN-VALUE = ""
     fi_vend:SCREEN-VALUE = ""
     fi_name:SCREEN-VALUE = "".

    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_vend B-table-Win
ON ENTRY OF fi_vend IN FRAME F-Main /* Vendor# */
DO:
  IF lv-save-char NE {&self-name}:SCREEN-VALUE THEN RUN new-vend.

  lv-save-char = {&self-name}:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_vend B-table-Win
ON LEAVE OF fi_vend IN FRAME F-Main /* Vendor# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN new-vend.

    RUN valid-vend NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_vend B-table-Win
ON VALUE-CHANGED OF fi_vend IN FRAME F-Main /* Vendor# */
DO:
  RUN new-vend.
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

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
  DEF BUFFER b-ap-pay FOR ap-pay.

  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-c-no LIKE ap-pay.c-no NO-UNDO.


  RUN set-defaults.
  
  li = 0.

  {&for-each1}
      USE-INDEX c-no NO-LOCK,
      {&for-each2}
      BREAK BY ap-pay.c-no DESC:
    IF FIRST-OF(ap-pay.c-no) THEN li = li + 1.
    lv-c-no = ap-pay.c-no.
    IF li GE 30 THEN LEAVE.
  END.

  &SCOPED-DEFINE open-query              ~
      OPEN QUERY {&browse-name}          ~
        {&for-each1}                     ~
              AND ap-pay.c-no GE lv-c-no ~
            USE-INDEX c-no NO-LOCK,      ~
            {&for-each2}

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

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN 
   ap-pay.vend-no:READ-ONLY IN BROWSE {&browse-name} = YES
   vend.name:READ-ONLY IN BROWSE {&browse-name} = YES
   ap-pay.check-no:READ-ONLY IN BROWSE {&browse-name} = YES
   ap-pay.check-date:READ-ONLY IN BROWSE {&browse-name} = YES
   .

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
  IF ll-first THEN RUN first-query.
  ELSE DO:
    {apinq/j-pcminq.i}
  END.
  
  ll-first = NO.

  RUN dispatch ("display-fields").

  RUN dispatch ("row-changed").

  RUN dispatch ('get-last':U).
  IF AVAIL {&FIRST-TABLE-IN-QUERY-{&browse-name}} THEN
    lv-last-rowid  = ROWID({&FIRST-TABLE-IN-QUERY-{&browse-name}}).
  IF AVAIL {&SECOND-TABLE-IN-QUERY-{&browse-name}} THEN
    lv-last-rowid2 = ROWID({&SECOND-TABLE-IN-QUERY-{&browse-name}}).
    
  RUN dispatch ('get-first':U).
  IF AVAIL {&FIRST-TABLE-IN-QUERY-{&browse-name}} THEN
    lv-frst-rowid  = ROWID({&FIRST-TABLE-IN-QUERY-{&browse-name}}).
  IF AVAIL {&SECOND-TABLE-IN-QUERY-{&browse-name}} THEN
    lv-frst-rowid2 = ROWID({&SECOND-TABLE-IN-QUERY-{&browse-name}}).

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

  /*hld-rowid = ROWID({&SECOND-TABLE-IN-QUERY-{&browse-name}}).

  IF ip-nav-type NE "" THEN
  CASE ip-nav-type:
    WHEN "F" THEN RUN dispatch ('get-first':U).
    WHEN "L" THEN RUN dispatch ('get-last':U).
    WHEN "N" THEN DO WHILE ROWID({&SECOND-TABLE-IN-QUERY-{&browse-name}}) EQ hld-rowid:
                    RUN dispatch ('get-next':U).
                  END.
    WHEN "P" THEN DO WHILE ROWID({&SECOND-TABLE-IN-QUERY-{&browse-name}}) EQ hld-rowid:
                    RUN dispatch ('get-prev':U).
                  END.
  END CASE.
    
  IF ROWID({&SECOND-TABLE-IN-QUERY-{&browse-name}}) EQ lv-last-rowid2 THEN
    op-nav-type = "L".
      
  IF ROWID({&SECOND-TABLE-IN-QUERY-{&browse-name}}) EQ lv-frst-rowid2 THEN
    op-nav-type = IF op-nav-type EQ "L" THEN "B" ELSE "F".*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-vend B-table-Win 
PROCEDURE new-vend :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND vend NO-LOCK
        WHERE vend.company EQ cocode
          AND vend.vend-no EQ fi_vend:SCREEN-VALUE
        NO-ERROR.
    IF AVAIL vend THEN fi_name:SCREEN-VALUE = vend.name.
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
  {src/adm/template/snd-list.i "ap-pay"}
  {src/adm/template/snd-list.i "vend"}

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
     fi_fchk:SCREEN-VALUE = ""
     fi_vend:SCREEN-VALUE = ""
     fi_name:SCREEN-VALUE = "".
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

RUN apinq/pcnq-exp.w ("", "").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-vend B-table-Win 
PROCEDURE valid-vend :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    fi_vend:SCREEN-VALUE = CAPS(fi_vend:SCREEN-VALUE).

    IF NOT CAN-FIND(FIRST vend WHERE vend.company EQ cocode
                                 AND vend.vend-no EQ fi_vend:SCREEN-VALUE)
    THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fi_vend.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-amount B-table-Win 
FUNCTION display-amount RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/  
  DEF VAR ld-amt AS DEC NO-UNDO.

  ld-amt = 0.
  FOR EACH ap-payl OF ap-pay NO-LOCK:
      ld-amt =  ld-amt - ap-payl.amt-paid + ap-payl.amt-disc.
  END.
  RETURN ld-amt.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

